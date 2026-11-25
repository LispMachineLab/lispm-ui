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
  (effects nil :type list)
  (cleanups nil :type list)
  (version 0 :type integer)
  (source-versions nil :type list)
  (disposed nil :type boolean))

(cl-defstruct (lispm-ui-effect
               (:constructor lispm-ui-effect--create)
               (:copier nil)
               (:predicate lispm-ui-effect-p))
  "A managed side-effect observer attached to one or more signals."
  (sources nil :type list)
  (callback #'ignore :type function)
  (active t :type boolean))

(cl-defstruct (lispm-ui-scope
               (:constructor lispm-ui-scope--create)
               (:copier nil)
               (:predicate lispm-ui-scope-p))
  "An ownership scope that disposes registered reactive resources together."
  (cleanups nil :type list)
  (disposed nil :type boolean))

(defvar lispm-ui-frp--current-scope nil
  "Active `lispm-ui-scope' capturing newly created reactive resources.")

(defvar lispm-ui-frp-max-flush-steps 1000
  "Maximum propagation rounds allowed in one `lispm-ui-frp--flush' cycle.")

(defun lispm-ui-scope-create ()
  "Return a fresh ownership scope for reactive signals, effects, and streams."
  (lispm-ui-scope--create))

(defun lispm-ui-scope-add (scope cleanup)
  "Register zero-argument CLEANUP thunk on SCOPE and return CLEANUP.
When SCOPE is already disposed, run CLEANUP immediately."
  (when (functionp cleanup)
    (cond
     ((null scope) nil)
     ((not (lispm-ui-scope-p scope))
      (signal 'lispm-ui-frp-error (list "Not a scope" scope)))
     ((lispm-ui-scope-disposed scope)
      (funcall cleanup))
     (t
      (push cleanup (lispm-ui-scope-cleanups scope)))))
  cleanup)

(defun lispm-ui-scope-dispose (scope)
  "Dispose SCOPE, running all registered cleanups in reverse creation order."
  (when (and (lispm-ui-scope-p scope)
             (not (lispm-ui-scope-disposed scope)))
    (setf (lispm-ui-scope-disposed scope) t)
    (let ((cleanups (lispm-ui-scope-cleanups scope)))
      (setf (lispm-ui-scope-cleanups scope) nil)
      (dolist (cleanup cleanups)
        (when (functionp cleanup)
          (funcall cleanup)))))
  nil)

(defmacro lispm-ui-with-scope (scope &rest body)
  "Run BODY with SCOPE capturing newly created signals, effects, and streams."
  (declare (indent 1) (debug t))
  `(let ((lispm-ui-frp--current-scope ,scope))
     ,@body))

(defun lispm-ui-scope-attach-buffer (scope &optional buffer)
  "Dispose SCOPE automatically when BUFFER (default current) is killed."
  (with-current-buffer (or buffer (current-buffer))
    (add-hook 'kill-buffer-hook
              (lambda () (lispm-ui-scope-dispose scope))
              nil t))
  scope)

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
                   :source-versions (mapcar #'lispm-ui-signal-version sources)
                   :level level)))
    (dolist (source sources)
      (cl-pushnew derived (lispm-ui-signal-dependents source) :test #'eq))
    (when lispm-ui-frp--current-scope
      (lispm-ui-scope-add lispm-ui-frp--current-scope
                          (lambda () (lispm-ui-signal-dispose derived))))
    derived))

(defun lispm-ui-signal-dispose (signal)
  "Detach SIGNAL from upstream sources and run its registered cleanups."
  (when (lispm-ui-signal-p signal)
    (setf (lispm-ui-signal-disposed signal) t)
    (dolist (source (lispm-ui-signal-sources signal))
      (setf (lispm-ui-signal-dependents source)
            (delq signal (lispm-ui-signal-dependents source))))
    (setf (lispm-ui-signal-sources signal) nil
          (lispm-ui-signal-dependents signal) nil)
    (let ((cleanups (lispm-ui-signal-cleanups signal)))
      (setf (lispm-ui-signal-cleanups signal) nil)
      (dolist (cleanup cleanups)
        (when (functionp cleanup)
          (funcall cleanup)))))
  nil)

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
                                (memq operation '(set let unlet makunbound))
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
    (let ((cur-versions (mapcar #'lispm-ui-signal-version
                                (lispm-ui-signal-sources signal))))
      (if (and (lispm-ui-signal-source-versions signal)
               (equal cur-versions (lispm-ui-signal-source-versions signal)))
          (progn
            (setf (lispm-ui-signal-stale signal) nil)
            (setq lispm-ui-frp--pending-signals
                  (delq signal lispm-ui-frp--pending-signals))
            nil)
        (let ((next (apply (lispm-ui-signal-compute signal)
                           (mapcar #'lispm-ui-signal-value
                                   (lispm-ui-signal-sources signal))))
              (old (lispm-ui-signal-value signal)))
          (setf (lispm-ui-signal-stale signal) nil
                (lispm-ui-signal-source-versions signal) cur-versions)
          (setq lispm-ui-frp--pending-signals
                (delq signal lispm-ui-frp--pending-signals))
          (unless (funcall (lispm-ui-signal-test signal) old next)
            (setf (lispm-ui-signal-value signal) next)
            (cl-incf (lispm-ui-signal-version signal))
            (lispm-ui-frp--enqueue-effects signal)
            t))))))

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
  "Mark all transitive derived dependents of SIGNAL as stale and pending."
  (dolist (dep (lispm-ui-signal-dependents signal))
    (unless (or (lispm-ui-signal-disposed dep)
                (memq dep lispm-ui-frp--pending-signals))
      (setf (lispm-ui-signal-stale dep) t)
      (push dep lispm-ui-frp--pending-signals)
      (lispm-ui-frp--mark-dependents dep))))

(defun lispm-ui-frp--enqueue-effects (signal)
  "Schedule active effects directly observing SIGNAL."
  (dolist (effect (lispm-ui-signal-effects signal))
    (when (lispm-ui-effect-active effect)
      (cl-pushnew effect lispm-ui-frp--pending-effects :test #'eq))))

(defun lispm-ui-frp--pop-lowest-signal ()
  "Remove and return the pending signal with minimum topological level."
  (let ((best (car lispm-ui-frp--pending-signals)))
    (dolist (sig (cdr lispm-ui-frp--pending-signals))
      (when (< (lispm-ui-signal-level sig) (lispm-ui-signal-level best))
        (setq best sig)))
    (setq lispm-ui-frp--pending-signals
          (delq best lispm-ui-frp--pending-signals))
    best))

(defun lispm-ui-frp--flush ()
  "Propagate pending signals in topological order and run triggered effects."
  (let ((lispm-ui-frp--batch-depth (max 1 lispm-ui-frp--batch-depth))
        (steps 0)
        (fifo nil))
    (while (or lispm-ui-frp--pending-signals lispm-ui-frp--pending-effects fifo)
      (cl-incf steps)
      (when (> steps lispm-ui-frp-max-flush-steps)
        (setq lispm-ui-frp--pending-signals nil
              lispm-ui-frp--pending-effects nil
              fifo nil)
        (signal 'lispm-ui-frp-error
                (list "Reactive propagation exceeded step limit" steps)))
      (while lispm-ui-frp--pending-signals
        (lispm-ui-signal--recompute (lispm-ui-frp--pop-lowest-signal)))
      (when lispm-ui-frp--pending-effects
        (let ((added (nreverse lispm-ui-frp--pending-effects)))
          (setq lispm-ui-frp--pending-effects nil)
          (dolist (eff added)
            (unless (memq eff fifo)
              (setq fifo (nconc fifo (list eff)))))))
      (when fifo
        (let ((effect (pop fifo)))
          (when (lispm-ui-effect-active effect)
            (apply (lispm-ui-effect-callback effect)
                   (mapcar #'lispm-ui-signal-get
                           (lispm-ui-effect-sources effect)))))))))

(defun lispm-ui-frp-begin-batch ()
  "Begin a reactive batch transaction; pair with `lispm-ui-frp-end-batch'."
  (setq lispm-ui-frp--batch-depth (1+ lispm-ui-frp--batch-depth)))

(defun lispm-ui-frp-end-batch (&optional commit)
  "End a reactive batch transaction started by `lispm-ui-frp-begin-batch'.
When COMMIT is non-nil on the outermost transaction, flush queued updates;
otherwise drop pending queues cleanly without double-flushing."
  (let ((depth (max 0 (1- lispm-ui-frp--batch-depth))))
    (setq lispm-ui-frp--batch-depth depth)
    (when (zerop depth)
      (unwind-protect
          (when commit
            (lispm-ui-frp--flush))
        (setq lispm-ui-frp--pending-signals nil
              lispm-ui-frp--pending-effects nil)))))

(defalias 'lispm-ui-frp--begin-batch #'lispm-ui-frp-begin-batch)
(defalias 'lispm-ui-frp--end-batch #'lispm-ui-frp-end-batch)

(defmacro lispm-ui-frp-batch (&rest body)
  "Execute BODY in a glitch-free reactive transaction.
Signal updates inside BODY are batched; derived signals and effects
propagate once in topological order when the outermost batch commits."
  (declare (indent 0) (debug t))
  (let ((committed (make-symbol "committed")))
    `(let ((,committed nil))
       (lispm-ui-frp-begin-batch)
       (unwind-protect
           (prog1 (progn ,@body)
             (setq ,committed t))
         (lispm-ui-frp-end-batch ,committed)))))

(defun lispm-ui-signal-set (signal value)
  "Set source SIGNAL to VALUE, propagating changes if different."
  (unless (lispm-ui-signal-source-p signal)
    (signal 'lispm-ui-frp-error (list "Cannot set a derived signal" signal)))
  (let ((old (lispm-ui-signal-value signal)))
    (unless (funcall (lispm-ui-signal-test signal) old value)
      (setf (lispm-ui-signal-value signal) value)
      (cl-incf (lispm-ui-signal-version signal))
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
      (when lispm-ui-frp--current-scope
        (lispm-ui-scope-add lispm-ui-frp--current-scope
                            (lambda () (lispm-ui-effect-dispose effect))))
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
  (listeners nil :type list)
  (cleanups nil :type list))

(defun lispm-ui-stream ()
  "Return a fresh discrete event stream."
  (lispm-ui-stream--create))

(defun lispm-ui-stream-dispose (stream)
  "Detach STREAM from upstream subscriptions and clear its listeners."
  (when (lispm-ui-stream-p stream)
    (setf (lispm-ui-stream-listeners stream) nil)
    (let ((cleanups (lispm-ui-stream-cleanups stream)))
      (setf (lispm-ui-stream-cleanups stream) nil)
      (dolist (cleanup cleanups)
        (when (functionp cleanup)
          (funcall cleanup)))))
  nil)

(defun lispm-ui-stream-subscribe (stream listener)
  "Subscribe LISTENER to STREAM; return an unsubscribe thunk."
  (unless (lispm-ui-stream-p stream)
    (signal 'lispm-ui-frp-error (list "Not a stream" stream)))
  (setf (lispm-ui-stream-listeners stream)
        (append (lispm-ui-stream-listeners stream) (list listener)))
  (let ((unsub (lambda ()
                 (setf (lispm-ui-stream-listeners stream)
                       (delq listener (lispm-ui-stream-listeners stream))))))
    (when lispm-ui-frp--current-scope
      (lispm-ui-scope-add lispm-ui-frp--current-scope unsub))
    unsub))

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
  (let ((scope lispm-ui-frp--current-scope)
        (lispm-ui-frp--current-scope nil)
        (out (lispm-ui-stream)))
    (push (lispm-ui-stream-subscribe
           stream (lambda (event) (lispm-ui-stream-emit out (funcall fn event))))
          (lispm-ui-stream-cleanups out))
    (when scope
      (lispm-ui-scope-add scope (lambda () (lispm-ui-stream-dispose out))))
    out))

(defun lispm-ui-stream-filter (pred stream)
  "Return a stream emitting only events of STREAM satisfying PRED."
  (let ((scope lispm-ui-frp--current-scope)
        (lispm-ui-frp--current-scope nil)
        (out (lispm-ui-stream)))
    (push (lispm-ui-stream-subscribe
           stream (lambda (event)
                    (when (funcall pred event)
                      (lispm-ui-stream-emit out event))))
          (lispm-ui-stream-cleanups out))
    (when scope
      (lispm-ui-scope-add scope (lambda () (lispm-ui-stream-dispose out))))
    out))

(defun lispm-ui-stream-merge (&rest streams)
  "Return a stream emitting events from any of STREAMS."
  (let ((scope lispm-ui-frp--current-scope)
        (lispm-ui-frp--current-scope nil)
        (out (lispm-ui-stream)))
    (dolist (stream streams)
      (push (lispm-ui-stream-subscribe
             stream (lambda (event) (lispm-ui-stream-emit out event)))
            (lispm-ui-stream-cleanups out)))
    (when scope
      (lispm-ui-scope-add scope (lambda () (lispm-ui-stream-dispose out))))
    out))

(defun lispm-ui-stream-fold (step initial stream &optional test)
  "Fold STREAM with STEP from INITIAL into a continuous `lispm-ui-signal'.
STEP is called as `(funcall STEP state event)'."
  (let ((scope lispm-ui-frp--current-scope)
        (lispm-ui-frp--current-scope nil)
        (sig (lispm-ui-signal initial test)))
    (push (lispm-ui-stream-subscribe
           stream (lambda (event)
                    (lispm-ui-signal-set sig (funcall step (lispm-ui-signal-get sig) event))))
          (lispm-ui-signal-cleanups sig))
    (when scope
      (lispm-ui-scope-add scope (lambda () (lispm-ui-signal-dispose sig))))
    sig))

(defun lispm-ui-stream-buffer-within (window-seconds combine empty stream &optional clock)
  "Accumulate events on STREAM occurring within WINDOW-SECONDS of each other.
COMBINE folds `(funcall COMBINE acc event)' starting from EMPTY; when an
event arrives more than WINDOW-SECONDS after the previous one, the
accumulator resets to EMPTY first.  CLOCK defaults to `float-time'.
Return a `lispm-ui-signal' of the current accumulated value."
  (let ((scope lispm-ui-frp--current-scope)
        (lispm-ui-frp--current-scope nil)
        (clock (or clock #'float-time))
        (last-time 0.0)
        (sig (lispm-ui-signal empty)))
    (push (lispm-ui-stream-subscribe
           stream (lambda (event)
                    (let* ((now (funcall clock))
                           (base (if (< (- now last-time) window-seconds)
                                     (lispm-ui-signal-get sig)
                                   empty)))
                      (setq last-time now)
                      (lispm-ui-signal-set sig (funcall combine base event)))))
          (lispm-ui-signal-cleanups sig))
    (when scope
      (lispm-ui-scope-add scope (lambda () (lispm-ui-signal-dispose sig))))
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
