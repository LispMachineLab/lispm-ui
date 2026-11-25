;;; lispm-ui-frp-async.el --- Async resources, rate limiting, and stores -*- lexical-binding: t; -*-

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

;; Async extensions on top of `lispm-ui-frp':
;;
;; - `lispm-ui-resource': managed async state (`idle', `loading', `ok',
;;   `error') with stale-response cancellation and automatic scope disposal.
;; - `lispm-ui-stream-debounce' & `lispm-ui-stream-throttle': timer-backed
;;   rate-limiting combinators with automatic timer cleanup.
;; - `lispm-ui-store': structured multi-slice state container with atomic
;;   batched writes, memoized selectors, snapshots, and reset.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'lispm-ui-frp)

;;;; Async resource signal

(cl-defstruct (lispm-ui-resource
               (:constructor lispm-ui-resource--create)
               (:copier nil)
               (:predicate lispm-ui-resource-p))
  "Reactive async resource tracking idle/loading/ok/error lifecycle."
  (signal nil)
  (fetcher #'ignore :type function)
  (source nil)
  (effect nil)
  (request-id 0 :type integer)
  (cancel-fn nil)
  (on-cancel nil)
  (disposed nil :type boolean))

(defun lispm-ui-resource--make-state (status data err)
  "Return normalized resource state plist for STATUS, DATA, and ERR."
  (list :status status
        :data data
        :error err
        :loading (eq status 'loading)))

(defun lispm-ui-resource--abort (resource)
  "Invoke any active cancellation hook on RESOURCE and clear cancel state."
  (when-let* ((cancel (lispm-ui-resource-cancel-fn resource)))
    (setf (lispm-ui-resource-cancel-fn resource) nil)
    (ignore-errors (funcall cancel)))
  (when-let* ((cb (lispm-ui-resource-on-cancel resource)))
    (ignore-errors (funcall cb))))

;;;###autoload
(cl-defun lispm-ui-resource (fetcher &key source initial on-cancel
                                     (auto-fetch t) immediate)
  "Create a managed async resource driven by FETCHER.
FETCHER is called as `(funcall FETCHER INPUT RESOLVE REJECT)' and may
return a zero-argument cancellation function.
When SOURCE is a `lispm-ui-signal', reload whenever SOURCE changes (and
immediately when AUTO-FETCH is non-nil).  When IMMEDIATE is non-nil and
SOURCE is nil, start the initial fetch at creation time."
  (when (and source (not (lispm-ui-signal-p source)))
    (signal 'lispm-ui-frp-error (list "Resource source is not a signal" source)))
  (let* ((sig (lispm-ui-signal (lispm-ui-resource--make-state 'idle initial nil)))
         (res (lispm-ui-resource--create
               :signal sig
               :fetcher fetcher
               :source source
               :on-cancel on-cancel)))
    (when source
      (let ((lispm-ui-frp--current-scope nil))
        (setf (lispm-ui-resource-effect res)
              (lispm-ui-effect
               source
               (lambda (val)
                 (unless (lispm-ui-resource-disposed res)
                   (lispm-ui-resource-reload res val)))
               auto-fetch))))
    (when (and immediate (null source))
      (lispm-ui-resource-reload res nil))
    (when lispm-ui-frp--current-scope
      (lispm-ui-scope-add lispm-ui-frp--current-scope
                          (lambda () (lispm-ui-resource-dispose res))))
    res))

(defun lispm-ui-resource-reload (resource &optional input)
  "Trigger a fetch on RESOURCE using INPUT (or current source value)."
  (unless (lispm-ui-resource-p resource)
    (signal 'lispm-ui-frp-error (list "Not a resource" resource)))
  (unless (lispm-ui-resource-disposed resource)
    (lispm-ui-resource--abort resource)
    (let* ((my-id (cl-incf (lispm-ui-resource-request-id resource)))
           (arg (or input
                    (and (lispm-ui-signal-p (lispm-ui-resource-source resource))
                         (lispm-ui-signal-get (lispm-ui-resource-source resource)))))
           (prev-data (plist-get (lispm-ui-signal-get (lispm-ui-resource-signal resource))
                                 :data))
           (sig (lispm-ui-resource-signal resource))
           (active-p (lambda ()
                       (and (not (lispm-ui-resource-disposed resource))
                            (= my-id (lispm-ui-resource-request-id resource)))))
           (resolve (lambda (data)
                      (when (funcall active-p)
                        (setf (lispm-ui-resource-cancel-fn resource) nil)
                        (lispm-ui-signal-set
                         sig (lispm-ui-resource--make-state 'ok data nil)))))
           (reject (lambda (err)
                     (when (funcall active-p)
                       (setf (lispm-ui-resource-cancel-fn resource) nil)
                       (lispm-ui-signal-set
                        sig (lispm-ui-resource--make-state 'error prev-data err))))))
      (lispm-ui-signal-set sig (lispm-ui-resource--make-state 'loading prev-data nil))
      (condition-case err
          (let ((cancel (funcall (lispm-ui-resource-fetcher resource)
                                 arg resolve reject)))
            (when (and (functionp cancel) (funcall active-p))
              (setf (lispm-ui-resource-cancel-fn resource) cancel)))
        (error (funcall reject err)))))
  resource)

(defun lispm-ui-resource-state (resource)
  "Return current state plist `(:status :data :error :loading)' of RESOURCE."
  (lispm-ui-signal-get (lispm-ui-resource-signal resource)))

(defun lispm-ui-resource-status (resource)
  "Return current status symbol of RESOURCE (`idle', `loading', `ok', `error')."
  (plist-get (lispm-ui-resource-state resource) :status))

(defun lispm-ui-resource-data (resource)
  "Return latest resolved data stored on RESOURCE, or nil."
  (plist-get (lispm-ui-resource-state resource) :data))

(defun lispm-ui-resource-error (resource)
  "Return latest error recorded on RESOURCE, or nil."
  (plist-get (lispm-ui-resource-state resource) :error))

(defun lispm-ui-resource-loading-p (resource)
  "Return non-nil when RESOURCE is currently loading."
  (plist-get (lispm-ui-resource-state resource) :loading))

(defun lispm-ui-resource-mutate (resource data)
  "Optimistically set RESOURCE to `ok' state holding DATA."
  (when (and (lispm-ui-resource-p resource)
             (not (lispm-ui-resource-disposed resource)))
    (lispm-ui-resource--abort resource)
    (cl-incf (lispm-ui-resource-request-id resource))
    (lispm-ui-signal-set (lispm-ui-resource-signal resource)
                         (lispm-ui-resource--make-state 'ok data nil)))
  data)

(defun lispm-ui-resource-cancel (resource)
  "Cancel any in-flight request on RESOURCE."
  (when (lispm-ui-resource-p resource)
    (cl-incf (lispm-ui-resource-request-id resource))
    (lispm-ui-resource--abort resource)
    (let* ((sig (lispm-ui-resource-signal resource))
           (cur (lispm-ui-signal-peek sig)))
      (when (eq (plist-get cur :status) 'loading)
        (lispm-ui-signal-set
         sig (lispm-ui-resource--make-state
              (if (plist-get cur :data) 'ok 'idle)
              (plist-get cur :data)
              nil)))))
  nil)

(defun lispm-ui-resource-dispose (resource)
  "Dispose RESOURCE, cancelling pending fetches and source effect."
  (when (and (lispm-ui-resource-p resource)
             (not (lispm-ui-resource-disposed resource)))
    (setf (lispm-ui-resource-disposed resource) t)
    (cl-incf (lispm-ui-resource-request-id resource))
    (lispm-ui-resource--abort resource)
    (when (lispm-ui-resource-effect resource)
      (lispm-ui-effect-dispose (lispm-ui-resource-effect resource))
      (setf (lispm-ui-resource-effect resource) nil))
    (lispm-ui-signal-dispose (lispm-ui-resource-signal resource)))
  nil)

;;;; Rate-limited streams

(defun lispm-ui-stream-debounce (delay stream)
  "Return a stream emitting STREAM events after DELAY seconds of quiet."
  (let ((scope lispm-ui-frp--current-scope)
        (lispm-ui-frp--current-scope nil)
        (out (lispm-ui-stream))
        (timer nil))
    (let ((unsub (lispm-ui-stream-subscribe
                  stream
                  (lambda (event)
                    (when (timerp timer)
                      (cancel-timer timer))
                    (setq timer
                          (run-with-timer
                           delay nil
                           (lambda ()
                             (setq timer nil)
                             (lispm-ui-stream-emit out event))))))))
      (push unsub (lispm-ui-stream-cleanups out))
      (push (lambda ()
              (when (timerp timer)
                (cancel-timer timer)
                (setq timer nil)))
            (lispm-ui-stream-cleanups out)))
    (when scope
      (lispm-ui-scope-add scope (lambda () (lispm-ui-stream-dispose out))))
    out))

(defun lispm-ui-stream-throttle (interval stream &optional clock)
  "Return a stream emitting at most one event per INTERVAL seconds.
Leading events emit immediately; suppressed bursts flush their latest
event once INTERVAL elapses.  CLOCK defaults to `float-time'."
  (let ((scope lispm-ui-frp--current-scope)
        (lispm-ui-frp--current-scope nil)
        (out (lispm-ui-stream))
        (clock (or clock #'float-time))
        (last-emit -1.0e12)
        (timer nil)
        (pending-p nil)
        (pending-val nil))
    (let ((unsub
           (lispm-ui-stream-subscribe
            stream
            (lambda (event)
              (let* ((now (funcall clock))
                     (elapsed (- now last-emit)))
                (if (>= elapsed interval)
                    (progn
                      (when (timerp timer)
                        (cancel-timer timer)
                        (setq timer nil))
                      (setq last-emit now
                            pending-p nil)
                      (lispm-ui-stream-emit out event))
                  (setq pending-p t
                        pending-val event)
                  (unless (timerp timer)
                    (setq timer
                          (run-with-timer
                           (max 0.0 (- interval elapsed)) nil
                           (lambda ()
                             (setq timer nil)
                             (when pending-p
                               (let ((val pending-val))
                                 (setq pending-p nil
                                       pending-val nil
                                       last-emit (funcall clock))
                                 (lispm-ui-stream-emit out val)))))))))))))
      (push unsub (lispm-ui-stream-cleanups out))
      (push (lambda ()
              (setq pending-p nil pending-val nil)
              (when (timerp timer)
                (cancel-timer timer)
                (setq timer nil)))
            (lispm-ui-stream-cleanups out)))
    (when scope
      (lispm-ui-scope-add scope (lambda () (lispm-ui-stream-dispose out))))
    out))

;;;; Structured multi-slice store

(cl-defstruct (lispm-ui-store
               (:constructor lispm-ui-store--create)
               (:copier nil)
               (:predicate lispm-ui-store-p))
  "Named reactive state container owning independent signal slices."
  (slices nil :type hash-table)
  (initial nil :type list)
  (test #'equal :type function)
  (disposed nil :type boolean))

(defun lispm-ui-store (initial-plist &optional test)
  "Create a reactive store seeded from INITIAL-PLIST.
Each key holds an independent `lispm-ui-signal' compared with TEST
\(default `equal')."
  (let* ((cmp (or test #'equal))
         (table (make-hash-table :test #'eq))
         (store (lispm-ui-store--create
                 :slices table
                 :initial (copy-sequence initial-plist)
                 :test cmp)))
    (cl-loop for (k v) on initial-plist by #'cddr
             do (puthash k (lispm-ui-signal v cmp) table))
    (when lispm-ui-frp--current-scope
      (lispm-ui-scope-add lispm-ui-frp--current-scope
                          (lambda () (lispm-ui-store-dispose store))))
    store))

(defun lispm-ui-store-signal (store key)
  "Return the `lispm-ui-signal' managing slice KEY in STORE."
  (unless (lispm-ui-store-p store)
    (signal 'lispm-ui-frp-error (list "Not a store" store)))
  (let ((table (lispm-ui-store-slices store)))
    (or (gethash key table)
        (puthash key (lispm-ui-signal nil (lispm-ui-store-test store)) table))))

(defun lispm-ui-store-get (store key)
  "Return current value of slice KEY in STORE."
  (lispm-ui-signal-get (lispm-ui-store-signal store key)))

(defun lispm-ui-store-set (store key value &rest more)
  "Atomically update slice KEY to VALUE (and MORE key-value pairs) in STORE."
  (unless (lispm-ui-store-p store)
    (signal 'lispm-ui-frp-error (list "Not a store" store)))
  (lispm-ui-frp-batch
    (lispm-ui-signal-set (lispm-ui-store-signal store key) value)
    (cl-loop for (k v) on more by #'cddr
             do (lispm-ui-signal-set (lispm-ui-store-signal store k) v)))
  value)

(defun lispm-ui-store-update (store key fn &rest args)
  "Update slice KEY in STORE by applying FN to its current value and ARGS."
  (let ((sig (lispm-ui-store-signal store key)))
    (lispm-ui-signal-set sig (apply fn (lispm-ui-signal-get sig) args))))

(defun lispm-ui-store-select (store keys fn &optional test)
  "Return a derived signal selecting KEYS from STORE through FN.
KEYS may be a single slice key or a list of slice keys.  When FN is nil,
single key passes through its value and multiple keys pass a list."
  (let* ((list-keys (if (and (listp keys) (not (keywordp keys)))
                        keys
                      (list keys)))
         (sigs (mapcar (lambda (k) (lispm-ui-store-signal store k)) list-keys))
         (project (or fn (if (= (length sigs) 1)
                             #'identity
                           (lambda (&rest vals) vals)))))
    (lispm-ui-signal-derived sigs project (or test #'equal))))

(defun lispm-ui-store-snapshot (store)
  "Return a fresh plist snapshot of all slice values in STORE."
  (let ((plist nil))
    (maphash (lambda (k sig)
               (setq plist (plist-put plist k (lispm-ui-signal-get sig))))
             (lispm-ui-store-slices store))
    plist))

(defun lispm-ui-store-reset (store &optional snapshot)
  "Reset STORE slices to SNAPSHOT (or original initial values) in one batch."
  (let ((target (or snapshot (lispm-ui-store-initial store))))
    (lispm-ui-frp-batch
      (maphash (lambda (k sig)
                 (lispm-ui-signal-set sig (plist-get target k)))
               (lispm-ui-store-slices store))
      (cl-loop for (k v) on target by #'cddr
               do (lispm-ui-signal-set (lispm-ui-store-signal store k) v))))
  store)

(defun lispm-ui-store-dispose (store)
  "Dispose all slice signals owned by STORE."
  (when (and (lispm-ui-store-p store)
             (not (lispm-ui-store-disposed store)))
    (setf (lispm-ui-store-disposed store) t)
    (maphash (lambda (_k sig) (lispm-ui-signal-dispose sig))
             (lispm-ui-store-slices store))
    (clrhash (lispm-ui-store-slices store)))
  nil)

(provide 'lispm-ui-frp-async)
;;; lispm-ui-frp-async.el ends here
