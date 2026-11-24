;;; lispm-ui-frp.el --- Functional reactive signals, streams and effects  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
;; URL: https://github.com/LispMachineLab/lispm-ui
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Functional reactive primitives for LispM UI:
;;
;; - `lispm-ui-signal': continuous time-varying behaviors (source and
;;   derived signals) forming a glitch-free directed acyclic graph (DAG)
;;   with topological push-pull propagation and transactional batching
;;   (`lispm-ui-frp-batch').
;; - `lispm-ui-effect': managed side-effect observers that subscribe to
;;   a set of dependency signals and run after a transaction settles.
;; - `lispm-ui-stream': discrete event streams with algebraic combinators
;;   (`map', `filter', `fold', `merge', `buffer-within', and queue
;;   coalescing) for keyboard, mouse, and asynchronous UI events.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-ui-base)

;;;; Structured conditions

(define-error 'lispm-ui-error "LispM UI error")
(define-error 'lispm-ui-frp-error "LispM UI reactive graph error" 'lispm-ui-error)

;;;; Signals (continuous behaviors)

;;; Invariant: (and (integerp (lispm-ui-signal-level sig)) (>= (lispm-ui-signal-level sig) 0))
;;; Invariant: For every source signal, `compute' and `sources' are nil.
;;; Invariant: For every derived signal, `level' > max(`level' of `sources').
(cl-defstruct (lispm-ui-signal
               (:constructor lispm-ui-signal--create)
               (:copier nil)
               (:predicate lispm-ui-signal-p))
  "A time-varying reactive value in the FRP dependency graph."
  value
  (test #'equal :type function)
  (compute nil)
  (sources nil :type list)
  (level 0 :type integer)
  (stale nil :type boolean)
  (dependents nil :type list)
  (effects nil :type list))

(cl-defstruct (lispm-ui-effect
               (:constructor lispm-ui-effect--create)
               (:copier nil)
               (:predicate lispm-ui-effect-p))
  "A managed side-effect observer attached to one or more signals."
  (sources nil :type list)
  (callback #'ignore :type function)
  (active t :type boolean))

(defvar lispm-ui-frp--batch-depth 0
  "Current nesting depth of `lispm-ui-frp-batch' transactions.")

(defvar lispm-ui-frp--pending-signals nil
  "Derived signals marked dirty during the active transaction.")

(defvar lispm-ui-frp--pending-effects nil
  "Effects scheduled to run when the outermost transaction commits.")

(defun lispm-ui-frp-in-batch-p ()
  "Return non-nil when execution is inside a `lispm-ui-frp-batch' transaction."
  (> lispm-ui-frp--batch-depth 0))

(defun lispm-ui-signal-source-p (signal)
  "Return non-nil when SIGNAL is a mutable source signal."
  (and (lispm-ui-signal-p signal)
       (null (lispm-ui-signal-compute signal))))

(defun lispm-ui-signal-derived-p (signal)
  "Return non-nil when SIGNAL is a computed derived signal."
  (and (lispm-ui-signal-p signal)
       (functionp (lispm-ui-signal-compute signal))))

(defun lispm-ui-signal (initial &optional test)
  "Return a fresh source signal holding INITIAL, compared with TEST.
TEST defaults to `equal'."
  (lispm-ui-signal--create :value initial :test (or test #'equal)))

(defun lispm-ui-signal-derived (sources compute &optional test)
  "Return a derived signal computed from SOURCES by applying COMPUTE.
SOURCES is a list of `lispm-ui-signal' values; COMPUTE is called with
their current values whenever any source changes.  TEST defaults to `equal'."
  (dolist (source sources)
    (unless (lispm-ui-signal-p source)
      (signal 'lispm-ui-frp-error (list "Not a signal" source))))
  (let* ((level (if sources
                    (1+ (apply #'max (mapcar #'lispm-ui-signal-level sources)))
                  0))
         (initial (apply compute (mapcar #'lispm-ui-signal-get sources)))
         (derived (lispm-ui-signal--create
                   :value initial
                   :test (or test #'equal)
                   :compute compute
                   :sources (copy-sequence sources)
                   :level level)))
    (dolist (source sources)
      (cl-pushnew derived (lispm-ui-signal-dependents source) :test #'eq))
    derived))

(defun lispm-ui-signal-map (fn signal &optional test)
  "Return a derived signal applying unary FN to SIGNAL's value."
  (lispm-ui-signal-derived (list signal) fn test))

(defun lispm-ui-signal-combine (fn signals &optional test)
  "Return a derived signal applying FN to the values of SIGNALS."
  (lispm-ui-signal-derived signals fn test))

(defun lispm-ui-frp--alist-p (collection)
  "Return non-nil when COLLECTION is a proper list of conses or nil entries."
  (and (consp collection)
       (proper-list-p collection)
       (seq-some #'consp collection)
       (seq-every-p (lambda (item) (or (null item) (consp item))) collection)))

(defun lispm-ui-signal-select (key signal &optional test)
  "Return a derived signal projecting KEY from alist or plist SIGNAL.
Only propagates when the projected value changes under TEST (default `equal')."
  (lispm-ui-signal-map
   (lambda (collection)
     (cond
      ((null collection) nil)
      ((lispm-ui-frp--alist-p collection)
       (cdr (assoc key collection)))
      ((and (proper-list-p collection)
            (cl-evenp (length collection)))
       (plist-get collection key))
      (t nil)))
   signal
   test))

(defvar lispm-ui-frp--variable-watchers (make-hash-table :test #'eq)
  "Active variable watchers installed by `lispm-ui-signal-watch-variable'.")

(defun lispm-ui-frp--variable-nonlocal-value (variable)
  "Return VARIABLE's current non-buffer-local value, or nil if unbound."
  (if (local-variable-p variable)
      (and (default-boundp variable) (default-value variable))
    (and (boundp variable) (symbol-value variable))))

(defun lispm-ui-signal-watch-variable (variable &optional signal test)
  "Bind VARIABLE's value to source SIGNAL (or a fresh one with TEST).
When VARIABLE is updated (`setq', `set-default', Customize) or dynamically
bound/unbound (`let' / `unlet'), SIGNAL is updated to the new value.
During top-level non-buffer-local `set' operations, NEWVAL is committed
to VARIABLE before effects run; during `let', `unlet', or when a buffer
has a local binding for VARIABLE, Emacs's pre-change watcher contract
runs effects with the incoming signal value before the binding mutates.
Return SIGNAL."
  (unless (symbolp variable)
    (signal 'lispm-ui-frp-error (list "Not a variable symbol" variable)))
  (let* ((initial (lispm-ui-frp--variable-nonlocal-value variable))
         (sig (or signal (lispm-ui-signal initial test)))
         (applying nil))
    (unless (lispm-ui-signal-source-p sig)
      (signal 'lispm-ui-frp-error (list "Cannot watch into a derived signal" sig)))
    (when-let* ((old (gethash variable lispm-ui-frp--variable-watchers)))
      (remove-variable-watcher variable old))
    (let ((watcher (lambda (sym newval operation where)
                     (when (and (not applying)
                                (memq operation '(set let unlet))
                                (null where))
                       (setq applying t)
                       (unwind-protect
                           (progn
                             ;; Pre-commit NEWVAL for non-buffer-local `set' so
                             ;; effects reading SYM observe NEWVAL without
                             ;; creating or clobbering a buffer-local binding
                             ;; on `set-default'.
                             (when (and (eq operation 'set)
                                        (not (local-variable-p sym))
                                        (not (local-variable-if-set-p sym)))
                               (set sym newval))
                             (lispm-ui-signal-set sig newval))
                         (setq applying nil))))))
      (puthash variable watcher lispm-ui-frp--variable-watchers)
      (add-variable-watcher variable watcher))
    (lispm-ui-signal-set sig (lispm-ui-frp--variable-nonlocal-value variable))
    sig))

(defun lispm-ui-signal-unwatch-variable (variable)
  "Remove any FRP variable watcher attached to VARIABLE."
  (when-let* ((watcher (gethash variable lispm-ui-frp--variable-watchers)))
    (remove-variable-watcher variable watcher)
    (remhash variable lispm-ui-frp--variable-watchers)
    t))

(defun lispm-ui-signal--recompute (signal)
  "Re-evaluate derived SIGNAL if stale; return non-nil when its value changed."
  (when (and (lispm-ui-signal-derived-p signal)
             (lispm-ui-signal-stale signal))
    (dolist (source (lispm-ui-signal-sources signal))
      (when (lispm-ui-signal-stale source)
        (lispm-ui-signal--recompute source)))
    (let ((next (apply (lispm-ui-signal-compute signal)
                       (mapcar #'lispm-ui-signal-value
                               (lispm-ui-signal-sources signal))))
          (old (lispm-ui-signal-value signal)))
      (setf (lispm-ui-signal-stale signal) nil)
      (setq lispm-ui-frp--pending-signals
            (delq signal lispm-ui-frp--pending-signals))
      (unless (funcall (lispm-ui-signal-test signal) old next)
        (setf (lispm-ui-signal-value signal) next)
        (lispm-ui-frp--enqueue-effects signal)
        t))))

(defun lispm-ui-signal-get (signal)
  "Return the current up-to-date value of SIGNAL."
  (unless (lispm-ui-signal-p signal)
    (signal 'lispm-ui-frp-error (list "Not a signal" signal)))
  (when (lispm-ui-signal-stale signal)
    (lispm-ui-signal--recompute signal))
  (lispm-ui-signal-value signal))

(defun lispm-ui-signal-peek (signal)
  "Return the last cached value of SIGNAL without forcing recomputation."
  (lispm-ui-signal-value signal))

(defun lispm-ui-frp--mark-dependents (signal)
  "Mark all transitive derived dependents of SIGNAL as stale."
  (dolist (dep (lispm-ui-signal-dependents signal))
    (unless (lispm-ui-signal-stale dep)
      (setf (lispm-ui-signal-stale dep) t)
      (cl-pushnew dep lispm-ui-frp--pending-signals :test #'eq)
      (lispm-ui-frp--mark-dependents dep))))

(defun lispm-ui-frp--enqueue-effects (signal)
  "Schedule active effects directly observing SIGNAL."
  (dolist (effect (lispm-ui-signal-effects signal))
    (when (lispm-ui-effect-active effect)
      (cl-pushnew effect lispm-ui-frp--pending-effects :test #'eq))))

(defun lispm-ui-frp--flush ()
  "Propagate pending signals in topological order and run triggered effects.
Effects run one at a time while batching stays active; if an effect
mutates a signal, dirty derived signals are topologically recomputed
before the next pending effect runs."
  (let ((lispm-ui-frp--batch-depth (max 1 lispm-ui-frp--batch-depth)))
    (while (or lispm-ui-frp--pending-signals lispm-ui-frp--pending-effects)
      (while lispm-ui-frp--pending-signals
        (let* ((queue (sort lispm-ui-frp--pending-signals
                            (lambda (a b)
                              (< (lispm-ui-signal-level a)
                                 (lispm-ui-signal-level b)))))
               (next (car queue)))
          (setq lispm-ui-frp--pending-signals (cdr queue))
          (lispm-ui-signal--recompute next)))
      (when (and (null lispm-ui-frp--pending-signals)
                 lispm-ui-frp--pending-effects)
        (let* ((ordered (nreverse lispm-ui-frp--pending-effects))
               (effect (car ordered)))
          (setq lispm-ui-frp--pending-effects (nreverse (cdr ordered)))
          (when (lispm-ui-effect-active effect)
            (apply (lispm-ui-effect-callback effect)
                   (mapcar #'lispm-ui-signal-get
                           (lispm-ui-effect-sources effect)))))))))

(defmacro lispm-ui-frp-batch (&rest body)
  "Execute BODY in a glitch-free reactive transaction.
Signal updates inside BODY are batched; derived signals and effects
propagate once in topological order when the outermost batch finishes."
  (declare (indent 0) (debug t))
  (let ((outer (make-symbol "outer")))
    `(let ((,outer (= lispm-ui-frp--batch-depth 0)))
       (setq lispm-ui-frp--batch-depth (1+ lispm-ui-frp--batch-depth))
       (unwind-protect
           (prog1 (progn ,@body)
             (when ,outer
               (setq lispm-ui-frp--batch-depth 0)
               (lispm-ui-frp--flush)))
         (when ,outer
           (setq lispm-ui-frp--batch-depth 0)
           (when (or lispm-ui-frp--pending-signals lispm-ui-frp--pending-effects)
             (unwind-protect
                 (lispm-ui-frp--flush)
               (setq lispm-ui-frp--pending-signals nil
                     lispm-ui-frp--pending-effects nil))))))))

(defun lispm-ui-signal-set (signal value)
  "Set source SIGNAL to VALUE, propagating changes if different."
  (unless (lispm-ui-signal-source-p signal)
    (signal 'lispm-ui-frp-error (list "Cannot set a derived signal" signal)))
  (let ((old (lispm-ui-signal-value signal)))
    (unless (funcall (lispm-ui-signal-test signal) old value)
      (setf (lispm-ui-signal-value signal) value)
      (lispm-ui-frp-batch
        (lispm-ui-frp--mark-dependents signal)
        (lispm-ui-frp--enqueue-effects signal))))
  value)

(defun lispm-ui-signal-update (signal fn &rest args)
  "Update source SIGNAL by applying FN to its current value and ARGS."
  (lispm-ui-signal-set signal (apply fn (lispm-ui-signal-get signal) args)))

(defun lispm-ui-effect (sources callback &optional run-immediately)
  "Attach an effect running CALLBACK when any of SOURCES changes.
SOURCES is a signal or list of signals; CALLBACK receives their values.
When RUN-IMMEDIATELY is non-nil, CALLBACK also runs once right away.
Return the `lispm-ui-effect' handle (pass to `lispm-ui-effect-dispose')."
  (let* ((list (if (lispm-ui-signal-p sources)
                   (list sources)
                 (and (listp sources) sources))))
    (unless list
      (signal 'lispm-ui-frp-error (list "Not a signal" sources)))
    (dolist (source list)
      (unless (lispm-ui-signal-p source)
        (signal 'lispm-ui-frp-error (list "Not a signal" source))))
    (let* ((unique (cl-remove-duplicates (copy-sequence list) :test #'eq))
           (effect (lispm-ui-effect--create :sources (copy-sequence list)
                                            :callback callback)))
      (dolist (source unique)
        (unless (memq effect (lispm-ui-signal-effects source))
          (setf (lispm-ui-signal-effects source)
                (append (lispm-ui-signal-effects source) (list effect)))))
      (when run-immediately
        (apply callback (mapcar #'lispm-ui-signal-get list)))
      effect)))

(defun lispm-ui-effect-dispose (effect)
  "Detach and deactivate EFFECT from all its source signals."
  (when (lispm-ui-effect-p effect)
    (setf (lispm-ui-effect-active effect) nil)
    (dolist (source (lispm-ui-effect-sources effect))
      (setf (lispm-ui-signal-effects source)
            (delq effect (lispm-ui-signal-effects source))))
    nil))

;;;; Streams (discrete events)

(cl-defstruct (lispm-ui-stream
               (:constructor lispm-ui-stream--create)
               (:copier nil)
               (:predicate lispm-ui-stream-p))
  "A discrete stream of events pushing occurrences to subscribers."
  (listeners nil :type list))

(defun lispm-ui-stream ()
  "Return a fresh discrete event stream."
  (lispm-ui-stream--create))

(defun lispm-ui-stream-subscribe (stream listener)
  "Subscribe LISTENER to STREAM; return an unsubscribe thunk."
  (unless (lispm-ui-stream-p stream)
    (signal 'lispm-ui-frp-error (list "Not a stream" stream)))
  (setf (lispm-ui-stream-listeners stream)
        (append (lispm-ui-stream-listeners stream) (list listener)))
  (lambda ()
    (setf (lispm-ui-stream-listeners stream)
          (delq listener (lispm-ui-stream-listeners stream)))))

(defun lispm-ui-stream-emit (stream event)
  "Emit discrete EVENT on STREAM within a reactive batch."
  (unless (lispm-ui-stream-p stream)
    (signal 'lispm-ui-frp-error (list "Not a stream" stream)))
  (lispm-ui-frp-batch
    (dolist (listener (copy-sequence (lispm-ui-stream-listeners stream)))
      (funcall listener event)))
  event)

(defun lispm-ui-stream-map (fn stream)
  "Return a stream emitting `(funcall FN event)' for each event on STREAM."
  (let ((out (lispm-ui-stream)))
    (lispm-ui-stream-subscribe
     stream (lambda (event) (lispm-ui-stream-emit out (funcall fn event))))
    out))

(defun lispm-ui-stream-filter (pred stream)
  "Return a stream emitting only events of STREAM satisfying PRED."
  (let ((out (lispm-ui-stream)))
    (lispm-ui-stream-subscribe
     stream (lambda (event)
              (when (funcall pred event)
                (lispm-ui-stream-emit out event))))
    out))

(defun lispm-ui-stream-merge (&rest streams)
  "Return a stream emitting events from any of STREAMS."
  (let ((out (lispm-ui-stream)))
    (dolist (stream streams)
      (lispm-ui-stream-subscribe
       stream (lambda (event) (lispm-ui-stream-emit out event))))
    out))

(defun lispm-ui-stream-fold (step initial stream &optional test)
  "Fold STREAM with STEP from INITIAL into a continuous `lispm-ui-signal'.
STEP is called as `(funcall STEP state event)'."
  (let ((sig (lispm-ui-signal initial test)))
    (lispm-ui-stream-subscribe
     stream (lambda (event)
              (lispm-ui-signal-set sig (funcall step (lispm-ui-signal-get sig) event))))
    sig))

(defun lispm-ui-stream-buffer-within (window-seconds combine empty stream &optional clock)
  "Accumulate events on STREAM occurring within WINDOW-SECONDS of each other.
COMBINE folds `(funcall COMBINE acc event)' starting from EMPTY; when an
event arrives more than WINDOW-SECONDS after the previous one, the
accumulator resets to EMPTY first.  CLOCK defaults to `float-time'.
Return a `lispm-ui-signal' of the current accumulated value."
  (let ((clock (or clock #'float-time))
        (last-time 0.0)
        (sig (lispm-ui-signal empty)))
    (lispm-ui-stream-subscribe
     stream (lambda (event)
              (let* ((now (funcall clock))
                     (base (if (< (- now last-time) window-seconds)
                               (lispm-ui-signal-get sig)
                             empty)))
                (setq last-time now)
                (lispm-ui-signal-set sig (funcall combine base event)))))
    sig))

(defun lispm-ui-frp-type-select-step (typed-pair char &optional window-seconds now)
  "Accumulate CHAR into TYPED-PAIR `(STRING . TIMESTAMP)' within WINDOW-SECONDS.
Return `(NEW-STRING . NOW)', resetting when more than WINDOW-SECONDS
\(default 1.0) have elapsed since TIMESTAMP."
  (let* ((now (or now (float-time)))
         (window (or window-seconds 1.0))
         (prev-text (or (car-safe typed-pair) ""))
         (prev-time (or (cdr-safe typed-pair) 0.0))
         (prefix (if (< (- now prev-time) window) prev-text "")))
    (cons (concat prefix (char-to-string char)) now)))

(provide 'lispm-ui-frp)
;;; lispm-ui-frp.el ends here
