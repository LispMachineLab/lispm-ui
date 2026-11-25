;;; lispm-ui-stage.el --- Reactive multi-pane stage and zoom reconciler  -*- lexical-binding: t; -*-

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

;; Functional reactive multi-pane stage layout and focus-zoom built on
;; `lispm-ui-frp':
;;
;; - Pure geometry planning (`lispm-ui-stage-min-size',
;;   `lispm-ui-stage-plan-split', `lispm-ui-stage-plan-initial-split')
;;   fits panes according to per-kind `(MIN-WIDTH . MIN-HEIGHT)' bounds.
;; - Pure state derivation (`lispm-ui-stage-derive-spec') computes the
;;   canonical stage layout from four signals:
;;     `enabled' × `subject' × `subjects' × `zoom' -> `TargetStageSpec'
;; - A single `lispm-ui-effect' reconciles Emacs windows only when the
;;   derived specification structurally changes (`equal'), eliminating
;;   imperative window thrashing and cross-mode buffer leaks.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-ui-base)
(require 'lispm-ui-frp)

;;;; Structured conditions

(define-error 'lispm-ui-stage-error
  "LispM UI stage error" 'lispm-ui-error)

(defvar-local lispm-ui-stage-managed nil
  "Non-nil in buffers managed ephemerally or synthetically by a stage.")
(put 'lispm-ui-stage-managed 'permanent-local t)

;;;; Domain structures

(cl-defstruct (lispm-ui-stage-tile
               (:constructor lispm-ui-stage-tile-create)
               (:copier nil)
               (:predicate lispm-ui-stage-tile-p))
  "A single buffer tile tracked for a subject on the stage."
  buffer
  (kind 'code :type symbol)
  (seq 0 :type integer))

(cl-defstruct (lispm-ui-stage-entry
               (:constructor lispm-ui-stage-entry-create)
               (:copier nil)
               (:predicate lispm-ui-stage-entry-p))
  "Activity state and ordered tile history of one stage subject."
  (busy-p nil :type boolean)
  (tiles nil :type list))

(cl-defstruct (lispm-ui-stage-zoom
               (:constructor lispm-ui-stage-zoom-create)
               (:copier nil)
               (:predicate lispm-ui-stage-zoom-p))
  "Focus-zoom state pinning a single stage buffer for `subject'."
  subject
  buffer
  (kind 'code :type symbol)
  (pinned-idle-p nil :type boolean)
  (last-active 0.0 :type number))

(cl-defstruct (lispm-ui-stage
               (:constructor lispm-ui-stage--create)
               (:copier nil)
               (:predicate lispm-ui-stage-p))
  "Reactive controller reconciling a frame's multi-pane stage area."
  enabled-signal
  subject-signal
  subjects-signal
  zoom-signal
  spec-signal
  reveal-stream
  (min-sizes '((code . (70 . 12))
               (shell . (56 . 8))
               (lisp . (56 . 8))
               (web . (68 . 12)))
             :type list)
  (primary-min-width 52 :type integer)
  (max-tiles 4 :type integer)
  (zoom-timeout 10 :type (or number null))
  (seq 0 :type integer)
  (inhibit-hooks nil :type boolean)
  primary-window-p
  excluded-window-p
  resolve-frame
  anchor-primary-fn
  on-reconcile
  effect
  zoom-timer)

;;;; Pure geometry & state derivation

;;; Post: Returns `(MIN-WIDTH . MIN-HEIGHT)' with positive integers.
(defun lispm-ui-stage-min-size (kind &optional min-sizes)
  "Return `(MIN-WIDTH . MIN-HEIGHT)' for stage KIND from MIN-SIZES."
  (let* ((table (or min-sizes '((code . (70 . 12)))))
         (pair (or (alist-get (or kind 'code) table)
                   (alist-get 'code table)
                   '(70 . 12))))
    (cons (max 20 (car pair)) (max 6 (cdr pair)))))

;;; Pre: Positive dimensions CUR-W, CUR-H, KEEP-W, KEEP-H, REQ-W, REQ-H.
;;; Post: Returns `(:side right|below :size NEGATIVE-INT)' or nil.
(defun lispm-ui-stage-plan-split (cur-w cur-h keep-w keep-h req-w req-h)
  "Plan splitting CUR-W by CUR-H (keeping KEEP-W by KEEP-H) for REQ-W by REQ-H.
Prefers horizontal (`right') when wide enough, else vertical (`below')."
  (cond
   ((and (>= cur-w (+ keep-w req-w 2))
         (>= cur-h (max keep-h req-h)))
    (let* ((share (/ (* cur-w req-w) (max 1 (+ keep-w req-w))))
           (new-w (min (- cur-w keep-w 1) (max req-w share))))
      (list :side 'right :size (- new-w))))
   ((and (>= cur-w (max keep-w req-w))
         (>= cur-h (+ keep-h req-h 1)))
    (let* ((share (/ (* cur-h req-h) (max 1 (+ keep-h req-h))))
           (new-h (min (- cur-h keep-h) (max req-h share))))
      (list :side 'below :size (- new-h))))
   (t nil)))

;;; Post: Returns `(:side right|below :size NEGATIVE-INT-OR-NIL)'.
(defun lispm-ui-stage-plan-initial-split (cur-w cur-h primary-min-w req-w req-h)
  "Plan initial split of CUR-W by CUR-H for REQ-W by REQ-H with PRIMARY-MIN-W."
  (cond
   ((>= cur-w (+ primary-min-w req-w 2))
    (let* ((keep (max primary-min-w (min 68 (/ (* cur-w 40) 100))))
           (stage-w (max req-w (- cur-w keep))))
      (list :side 'right :size (- stage-w))))
   ((>= cur-h (+ 10 req-h 2))
    (list :side 'below :size (- (max req-h (/ cur-h 2)))))
   (t
    (list :side 'right :size nil))))

(defun lispm-ui-stage--live-entry-tiles (entry max-tiles)
  "Return up to MAX-TILES `(BUFFER . KIND)' pairs from ENTRY, oldest first."
  (when (lispm-ui-stage-entry-p entry)
    (let* ((live (seq-filter
                  (lambda (tile)
                    (buffer-live-p (lispm-ui-stage-tile-buffer tile)))
                  (lispm-ui-stage-entry-tiles entry)))
           (recent (seq-take live (max 1 max-tiles))))
      (mapcar (lambda (tile)
                (cons (lispm-ui-stage-tile-buffer tile)
                      (lispm-ui-stage-tile-kind tile)))
              (reverse recent)))))

;;; Law: When ENABLED is nil, returns `'(:mode disabled)' regardless of SUBJECTS.
;;; Law: When SUBJECT is idle and unzoomed, returns `'(:mode closed)'.
(defun lispm-ui-stage-derive-spec (enabled subject subjects zoom max-tiles)
  "Derive canonical stage spec from ENABLED, SUBJECT, SUBJECTS, ZOOM, MAX-TILES."
  (cond
   ((not enabled)
    '(:mode disabled))
   ((and (lispm-ui-stage-zoom-p zoom)
         subject
         (equal (lispm-ui-stage-zoom-subject zoom) subject)
         (buffer-live-p (lispm-ui-stage-zoom-buffer zoom)))
    (list :mode 'zoomed
          :subject subject
          :buffer (lispm-ui-stage-zoom-buffer zoom)
          :kind (lispm-ui-stage-zoom-kind zoom)))
   (subject
    (let* ((entry (cdr (assoc subject subjects #'equal)))
           (tiles (and entry
                       (lispm-ui-stage-entry-busy-p entry)
                       (lispm-ui-stage--live-entry-tiles entry max-tiles))))
      (if tiles
          (list :mode 'tiled :subject subject :tiles tiles)
        '(:mode closed))))
   (t
    '(:mode closed))))

;;;; Window queries & predicates

(defun lispm-ui-stage--primary-p (stage window)
  "Return non-nil when WINDOW is a primary/session window for STAGE."
  (let ((pred (lispm-ui-stage-primary-window-p stage)))
    (and pred (window-live-p window) (funcall pred window))))

(defun lispm-ui-stage--excluded-p (stage window)
  "Return non-nil when WINDOW is excluded from STAGE layout."
  (or (not (window-live-p window))
      (window-dedicated-p window)
      (window-parameter window 'window-side)
      (let ((pred (lispm-ui-stage-excluded-window-p stage)))
        (and pred (funcall pred window)))))

(defun lispm-ui-stage--owns-window-p (stage window)
  "Return non-nil when WINDOW belongs to STAGE (or legacy unstamped stage)."
  (let ((owner (window-parameter window 'lispm-ui-stage)))
    (if owner
        (eq owner stage)
      (and (window-parameter window 'lispm-stage) t))))

(defun lispm-ui-stage-windows (stage &optional frame)
  "Return live stage windows managed by STAGE on FRAME."
  (let ((f (or frame (lispm-ui-stage--target-frame stage))))
    (when (frame-live-p f)
      (seq-filter (lambda (w)
                    (and (not (lispm-ui-stage--excluded-p stage w))
                         (not (lispm-ui-stage--primary-p stage w))
                         (lispm-ui-stage--owns-window-p stage w)))
                  (window-list f 'nomini)))))

(defun lispm-ui-stage--target-frame (stage &optional subject)
  "Resolve the target frame for STAGE and optional SUBJECT."
  (let ((resolver (lispm-ui-stage-resolve-frame stage)))
    (or (and resolver (funcall resolver (or subject (lispm-ui-stage-subject stage))))
        (selected-frame))))

(defun lispm-ui-stage--stamp-window (stage window buffer kind)
  "Display BUFFER of KIND in stage WINDOW and stamp STAGE metadata."
  (set-window-parameter window 'lispm-ui-stage stage)
  (set-window-parameter window 'lispm-stage t)
  (set-window-parameter window 'lispm-stage-kind (or kind 'code))
  (set-window-parameter window 'lispm-stage-seq
                        (cl-incf (lispm-ui-stage-seq stage)))
  (unless (eq (window-buffer window) buffer)
    (record-window-buffer window)
    (set-window-buffer window buffer))
  window)

(defun lispm-ui-stage--unmark-window (window)
  "Clear stage window parameters on WINDOW when it cannot be deleted."
  (when (window-live-p window)
    (set-window-parameter window 'lispm-ui-stage nil)
    (set-window-parameter window 'lispm-stage nil)
    (set-window-parameter window 'lispm-stage-kind nil)
    (set-window-parameter window 'lispm-stage-seq nil)))

(defun lispm-ui-stage--safe-delete-window (stage window)
  "Delete WINDOW of STAGE when sibling main windows exist; else unmark it."
  (when (window-live-p window)
    (let* ((frame (window-frame window))
           (main (seq-remove (lambda (w) (lispm-ui-stage--excluded-p stage w))
                             (window-list frame 'nomini))))
      (if (> (length main) 1)
          (condition-case nil
              (delete-window window)
            (error (lispm-ui-stage--unmark-window window)))
        (lispm-ui-stage--unmark-window window)))))

;;;; Window allocation & reconciliation boundary

(defun lispm-ui-stage--split-existing (stage stage-wins req-w req-h)
  "Split one of STAGE-WINS on STAGE to fit REQ-W by REQ-H, or return nil."
  (let ((ordered (sort (copy-sequence stage-wins)
                       (lambda (a b)
                         (> (* (window-total-width a) (window-total-height a))
                            (* (window-total-width b) (window-total-height b))))))
        (min-sizes (lispm-ui-stage-min-sizes stage))
        created)
    (while (and ordered (null created))
      (let* ((win (pop ordered))
             (w-kind (or (window-parameter win 'lispm-stage-kind) 'code))
             (keep (lispm-ui-stage-min-size w-kind min-sizes))
             (plan (lispm-ui-stage-plan-split
                    (window-total-width win) (window-total-height win)
                    (car keep) (cdr keep) req-w req-h)))
        (when plan
          (setq created
                (condition-case nil
                    (split-window win (plist-get plan :size) (plist-get plan :side))
                  (error nil))))))
    created))

(defun lispm-ui-stage--lru-window (stage-wins kind)
  "Pick the least recently stamped window in STAGE-WINS, preferring KIND."
  (let* ((candidates (or (seq-remove (lambda (w) (eq w (selected-window)))
                                     stage-wins)
                         stage-wins))
         (same (seq-filter (lambda (w)
                             (eq (window-parameter w 'lispm-stage-kind) kind))
                           candidates))
         (pool (or same candidates)))
    (car (sort (copy-sequence pool)
               (lambda (a b)
                 (< (or (window-parameter a 'lispm-stage-seq) 0)
                    (or (window-parameter b 'lispm-stage-seq) 0)))))))

(defun lispm-ui-stage--spare-window (stage frame)
  "Find an existing non-primary, non-stage document window on FRAME for STAGE."
  (car (sort (seq-filter (lambda (w)
                           (and (not (eq w (selected-window)))
                                (not (lispm-ui-stage--excluded-p stage w))
                                (not (lispm-ui-stage--primary-p stage w))
                                (not (window-parameter w 'lispm-ui-stage))
                                (buffer-file-name (window-buffer w))))
                         (window-list frame 'nomini))
             (lambda (a b) (> (window-use-time a) (window-use-time b))))))

(defun lispm-ui-stage--initial-split (stage frame req-w req-h)
  "Create the first stage pane on FRAME for STAGE sized REQ-W by REQ-H."
  (let* ((wins (seq-remove (lambda (w) (lispm-ui-stage--excluded-p stage w))
                           (window-list frame 'nomini)))
         (others (seq-remove (lambda (w) (lispm-ui-stage--primary-p stage w))
                             wins))
         (host (car (sort (copy-sequence (or others wins))
                          (lambda (a b)
                            (> (* (window-total-width a) (window-total-height a))
                               (* (window-total-width b) (window-total-height b)))))))
         (anchor-fn (lispm-ui-stage-anchor-primary-fn stage)))
    (when host
      (let* ((anchor-state (and anchor-fn (funcall anchor-fn host 'capture)))
             (plan (lispm-ui-stage-plan-initial-split
                    (window-total-width host) (window-total-height host)
                    (lispm-ui-stage-primary-min-width stage) req-w req-h))
             (created (or (condition-case nil
                              (split-window host (plist-get plan :size)
                                            (plist-get plan :side))
                            (error nil))
                          (condition-case nil (split-window host nil 'right)
                            (error nil))
                          (condition-case nil (split-window host nil 'below)
                            (error nil)))))
        (when (and created anchor-fn)
          (funcall anchor-fn host anchor-state))
        created))))

(defun lispm-ui-stage--allocate-tile (stage frame buffer kind)
  "Allocate or reuse a stage pane on FRAME for STAGE showing BUFFER of KIND."
  (let* ((req (lispm-ui-stage-min-size kind (lispm-ui-stage-min-sizes stage)))
         (req-w (car req))
         (req-h (cdr req))
         (stage-wins (lispm-ui-stage-windows stage frame))
         (win (cond
               ((null stage-wins)
                (or (lispm-ui-stage--spare-window stage frame)
                    (lispm-ui-stage--initial-split stage frame req-w req-h)))
               ((< (length stage-wins) (max 1 (lispm-ui-stage-max-tiles stage)))
                (or (lispm-ui-stage--split-existing stage stage-wins req-w req-h)
                    (lispm-ui-stage--lru-window stage-wins kind)))
               (t
                (lispm-ui-stage--lru-window stage-wins kind)))))
    (when (window-live-p win)
      (lispm-ui-stage--stamp-window stage win buffer kind))))

(defun lispm-ui-stage--close-windows (stage frame)
  "Close all stage panes managed by STAGE on FRAME."
  (dolist (win (lispm-ui-stage-windows stage frame))
    (lispm-ui-stage--safe-delete-window stage win)))

(defun lispm-ui-stage--reconcile-zoomed (stage frame buffer kind)
  "Reconcile STAGE on FRAME to a single zoomed pane showing BUFFER of KIND."
  (let* ((wins (lispm-ui-stage-windows stage frame))
         (keep (or (seq-find (lambda (w) (eq (window-buffer w) buffer)) wins)
                   (car wins))))
    (dolist (win wins)
      (unless (eq win keep)
        (lispm-ui-stage--safe-delete-window stage win)))
    (if (window-live-p keep)
        (lispm-ui-stage--stamp-window stage keep buffer kind)
      (lispm-ui-stage--allocate-tile stage frame buffer kind))))

(defun lispm-ui-stage--oldest-ranked-window (stage-wins min-rank)
  "Return the non-selected window in STAGE-WINS with rank > MIN-RANK, or nil."
  (let* ((pool (or (seq-remove (lambda (w) (eq w (selected-window))) stage-wins)
                   stage-wins))
         (older (seq-filter
                 (lambda (w)
                   (> (or (window-parameter w 'lispm-stage-rank) most-positive-fixnum)
                      min-rank))
                 pool)))
    (car (sort (copy-sequence older)
               (lambda (a b)
                 (> (or (window-parameter a 'lispm-stage-rank) 0)
                    (or (window-parameter b 'lispm-stage-rank) 0)))))))

;;; Post: Displays the `min(capacity, length(TILES))' newest tiles without
;;; shuffling panes that already hold one of those newest tiles.
(defun lispm-ui-stage--reconcile-tiled (stage frame tiles)
  "Reconcile STAGE on FRAME to match `(BUFFER . KIND)' list TILES."
  (let* ((newest-first (cl-loop for cell in (reverse tiles)
                                for rank from 0
                                collect (list (car cell) (cdr cell) rank)))
         (wins (lispm-ui-stage-windows stage frame))
         (kept-wins nil)
         (pending nil))
    (pcase-dolist (`(,buf ,kind ,rank) newest-first)
      (if-let* ((match (seq-find (lambda (w)
                                   (and (not (memq w kept-wins))
                                        (eq (window-buffer w) buf)))
                                 wins)))
          (progn
            (push match kept-wins)
            (set-window-parameter match 'lispm-stage-rank rank)
            (lispm-ui-stage--stamp-window stage match buf kind))
        (push (list buf kind rank) pending)))
    (setq pending (nreverse pending))
    (let ((free (seq-remove (lambda (w) (memq w kept-wins)) wins)))
      (while (and free (> (length free) (length pending)))
        (lispm-ui-stage--safe-delete-window stage (pop free)))
      (while (and free pending)
        (pcase-let ((win (pop free))
                    (`(,buf ,kind ,rank) (pop pending)))
          (when (window-live-p win)
            (set-window-parameter win 'lispm-stage-rank rank)
            (lispm-ui-stage--stamp-window stage win buf kind))))
      (pcase-dolist (`(,buf ,kind ,rank) pending)
        (let* ((req (lispm-ui-stage-min-size kind (lispm-ui-stage-min-sizes stage)))
               (cur-wins (lispm-ui-stage-windows stage frame))
               (win (cond
                     ((null cur-wins)
                      (or (lispm-ui-stage--spare-window stage frame)
                          (lispm-ui-stage--initial-split
                           stage frame (car req) (cdr req))))
                     ((< (length cur-wins) (max 1 (lispm-ui-stage-max-tiles stage)))
                      (or (lispm-ui-stage--split-existing
                           stage cur-wins (car req) (cdr req))
                          (lispm-ui-stage--oldest-ranked-window cur-wins rank)))
                     (t
                      (lispm-ui-stage--oldest-ranked-window cur-wins rank)))))
          (when (window-live-p win)
            (set-window-parameter win 'lispm-stage-rank rank)
            (lispm-ui-stage--stamp-window stage win buf kind)))))))

(defun lispm-ui-stage--reconcile (stage spec)
  "Reconcile Emacs windows on STAGE's target frame to match SPEC."
  (let ((frame (lispm-ui-stage--target-frame stage (plist-get spec :subject))))
    (when (frame-live-p frame)
      (setf (lispm-ui-stage-inhibit-hooks stage) t)
      (unwind-protect
          (pcase (plist-get spec :mode)
            ((or 'disabled 'closed)
             (lispm-ui-stage--close-windows stage frame))
            ('zoomed
             (lispm-ui-stage--reconcile-zoomed
              stage frame (plist-get spec :buffer) (plist-get spec :kind)))
            ('tiled
             (lispm-ui-stage--reconcile-tiled
              stage frame (plist-get spec :tiles))))
        (setf (lispm-ui-stage-inhibit-hooks stage) nil))
      (lispm-ui-stage--sync-zoom-timer stage)
      (when-let* ((cb (lispm-ui-stage-on-reconcile stage)))
        (funcall cb spec frame)))))

;;;; Zoom timeout & interactive transitions

(defun lispm-ui-stage--sync-zoom-timer (stage)
  "Start or stop STAGE's zoom inactivity timer based on current zoom state."
  (let* ((zoom (lispm-ui-signal-peek (lispm-ui-stage-zoom-signal stage)))
         (timeout (lispm-ui-stage-zoom-timeout stage))
         (need (and (lispm-ui-stage-zoom-p zoom)
                    (not (lispm-ui-stage-zoom-pinned-idle-p zoom))
                    (numberp timeout)
                    (> timeout 0)))
         (timer (lispm-ui-stage-zoom-timer stage)))
    (cond
     ((and need (not (timerp timer)))
      (setf (lispm-ui-stage-zoom-timer stage)
            (run-at-time 1.0 1.0 #'lispm-ui-stage-check-timeout stage)))
     ((and (not need) (timerp timer))
      (cancel-timer timer)
      (setf (lispm-ui-stage-zoom-timer stage) nil)))))

(defun lispm-ui-stage-check-timeout (stage)
  "Unzoom STAGE when the focused pane has been idle past `zoom-timeout'."
  (let* ((zoom (lispm-ui-signal-get (lispm-ui-stage-zoom-signal stage)))
         (timeout (lispm-ui-stage-zoom-timeout stage))
         (subj (and zoom (lispm-ui-stage-zoom-subject zoom))))
    (when (and (lispm-ui-stage-zoom-p zoom)
               (not (lispm-ui-stage-zoom-pinned-idle-p zoom))
               (numberp timeout)
               (> timeout 0)
               (lispm-ui-stage-subject-busy-p stage subj)
               (>= (- (float-time) (lispm-ui-stage-zoom-last-active zoom))
                   timeout))
      (lispm-ui-stage-unzoom stage t))))

(defalias 'lispm-ui-stage--check-timeout #'lispm-ui-stage-check-timeout)

;;;; Public reactive API

(cl-defun lispm-ui-stage-create
    (&key min-sizes (primary-min-width 52) (max-tiles 4) (zoom-timeout 10)
          primary-window-p excluded-window-p resolve-frame
          anchor-primary-fn on-reconcile)
  "Create a reactive `lispm-ui-stage' controller.
MIN-SIZES maps kind symbols to `(MIN-W . MIN-H)'; PRIMARY-MIN-WIDTH
preserves primary pane columns; MAX-TILES caps stage splits; ZOOM-TIMEOUT
sets auto-resume seconds.  PRIMARY-WINDOW-P, EXCLUDED-WINDOW-P,
RESOLVE-FRAME, ANCHOR-PRIMARY-FN and ON-RECONCILE customize host integration."
  (let* ((enabled-sig (lispm-ui-signal t #'eq))
         (subject-sig (lispm-ui-signal nil #'equal))
         (subjects-sig (lispm-ui-signal nil #'equal))
         (zoom-sig (lispm-ui-signal nil #'equal))
         (reveal (lispm-ui-stream))
         (stage (lispm-ui-stage--create
                 :enabled-signal enabled-sig
                 :subject-signal subject-sig
                 :subjects-signal subjects-sig
                 :zoom-signal zoom-sig
                 :reveal-stream reveal
                 :primary-min-width primary-min-width
                 :max-tiles max-tiles
                 :zoom-timeout zoom-timeout
                 :primary-window-p primary-window-p
                 :excluded-window-p excluded-window-p
                 :resolve-frame resolve-frame
                 :anchor-primary-fn anchor-primary-fn
                 :on-reconcile on-reconcile)))
    (when min-sizes
      (setf (lispm-ui-stage-min-sizes stage) min-sizes))
    (let ((spec-sig
           (lispm-ui-signal-derived
            (list enabled-sig subject-sig subjects-sig zoom-sig)
            (lambda (en subj subjs zm)
              (lispm-ui-stage-derive-spec
               en subj subjs zm (lispm-ui-stage-max-tiles stage)))
            #'equal)))
      (setf (lispm-ui-stage-spec-signal stage) spec-sig
            (lispm-ui-stage-effect stage)
            (lispm-ui-effect
             spec-sig
             (lambda (spec) (lispm-ui-stage--reconcile stage spec)))))
    stage))

(defun lispm-ui-stage-dispose (stage)
  "Dispose STAGE's timers and reactive effect."
  (when (lispm-ui-stage-p stage)
    (when (timerp (lispm-ui-stage-zoom-timer stage))
      (cancel-timer (lispm-ui-stage-zoom-timer stage))
      (setf (lispm-ui-stage-zoom-timer stage) nil))
    (lispm-ui-effect-dispose (lispm-ui-stage-effect stage))))

(defun lispm-ui-stage-enabled-p (stage)
  "Return non-nil when STAGE is enabled."
  (lispm-ui-signal-get (lispm-ui-stage-enabled-signal stage)))

(defun lispm-ui-stage-subject (stage)
  "Return STAGE's currently selected subject key, or nil."
  (lispm-ui-signal-get (lispm-ui-stage-subject-signal stage)))

(defun lispm-ui-stage-zoomed-p (stage)
  "Return non-nil when STAGE is actively zoomed (excluding idle pins)."
  (let ((zoom (lispm-ui-signal-get (lispm-ui-stage-zoom-signal stage))))
    (and (eq (plist-get (lispm-ui-signal-get (lispm-ui-stage-spec-signal stage))
                        :mode)
             'zoomed)
         (lispm-ui-stage-zoom-p zoom)
         (not (lispm-ui-stage-zoom-pinned-idle-p zoom)))))

(defun lispm-ui-stage-subject-busy-p (stage subject)
  "Return non-nil when SUBJECT is marked busy in STAGE."
  (when subject
    (let ((entry (cdr (assoc subject
                             (lispm-ui-signal-get
                              (lispm-ui-stage-subjects-signal stage))
                             #'equal))))
      (and entry (lispm-ui-stage-entry-busy-p entry)))))

(defun lispm-ui-stage-retains-buffer-p (stage buffer)
  "Return non-nil when BUFFER is retained by an active focus-zoom on STAGE."
  (and (lispm-ui-stage-zoomed-p stage)
       (let* ((zoom (lispm-ui-signal-get (lispm-ui-stage-zoom-signal stage)))
              (subj (and zoom (lispm-ui-stage-zoom-subject zoom)))
              (entry (and subj
                          (cdr (assoc subj
                                      (lispm-ui-signal-get
                                       (lispm-ui-stage-subjects-signal stage))
                                      #'equal)))))
         (or (and zoom (eq (lispm-ui-stage-zoom-buffer zoom) buffer))
             (and entry
                  (seq-some (lambda (tile)
                              (eq (lispm-ui-stage-tile-buffer tile) buffer))
                            (lispm-ui-stage-entry-tiles entry)))))))

(defun lispm-ui-stage-set-enabled (stage enabled)
  "Set whether STAGE is ENABLED, clearing zoom when disabled."
  (let ((flag (and enabled t)))
    (lispm-ui-frp-batch
      (unless flag
        (lispm-ui-signal-set (lispm-ui-stage-zoom-signal stage) nil))
      (lispm-ui-signal-set (lispm-ui-stage-enabled-signal stage) flag))))

(defun lispm-ui-stage-set-subject (stage subject)
  "Switch STAGE's active session to SUBJECT, dropping foreign zoom state."
  (let ((zoom (lispm-ui-signal-get (lispm-ui-stage-zoom-signal stage))))
    (lispm-ui-frp-batch
      (when (and zoom (not (equal (lispm-ui-stage-zoom-subject zoom) subject)))
        (lispm-ui-signal-set (lispm-ui-stage-zoom-signal stage) nil))
      (lispm-ui-signal-set (lispm-ui-stage-subject-signal stage) subject))))

(defun lispm-ui-stage--put-entry (subjects subject entry)
  "Return a fresh alist binding SUBJECT to ENTRY in SUBJECTS."
  (cons (cons subject entry)
        (seq-remove (pcase-lambda (`(,k . ,_)) (equal k subject)) subjects)))

(defun lispm-ui-stage-set-busy (stage subject busy-p &optional clear-when-starting)
  "Set SUBJECT's busy flag to BUSY-P in STAGE.
When BUSY-P becomes nil while zoomed on SUBJECT, pin zoom to that single
buffer; when BUSY-P becomes non-nil and CLEAR-WHEN-STARTING holds, reset
stale tiles and idle-pinned zoom so fresh tiles take the stage."
  (when subject
    (let* ((subjects (lispm-ui-signal-get (lispm-ui-stage-subjects-signal stage)))
           (prev (cdr (assoc subject subjects #'equal)))
           (was-busy (and prev (lispm-ui-stage-entry-busy-p prev)))
           (tiles (if (and busy-p (not was-busy) clear-when-starting)
                      nil
                    (and prev (lispm-ui-stage-entry-tiles prev))))
           (entry (lispm-ui-stage-entry-create :busy-p (and busy-p t)
                                               :tiles tiles))
           (zoom (lispm-ui-signal-get (lispm-ui-stage-zoom-signal stage))))
      (lispm-ui-frp-batch
        (cond
         ((and zoom (equal (lispm-ui-stage-zoom-subject zoom) subject))
          (cond
           ((not busy-p)
            (lispm-ui-signal-set
             (lispm-ui-stage-zoom-signal stage)
             (lispm-ui-stage-zoom-create
              :subject subject
              :buffer (lispm-ui-stage-zoom-buffer zoom)
              :kind (lispm-ui-stage-zoom-kind zoom)
              :pinned-idle-p t
              :last-active (lispm-ui-stage-zoom-last-active zoom))))
           ((and busy-p (lispm-ui-stage-zoom-pinned-idle-p zoom))
            (let* ((frame (lispm-ui-stage--target-frame stage subject))
                   (sel (and (frame-live-p frame) (frame-selected-window frame))))
              (unless (and (window-live-p sel)
                           (eq (window-buffer sel)
                               (lispm-ui-stage-zoom-buffer zoom)))
                (lispm-ui-signal-set (lispm-ui-stage-zoom-signal stage) nil))))))
         ((and (not busy-p) (equal (lispm-ui-stage-subject stage) subject))
          (let* ((frame (lispm-ui-stage--target-frame stage subject))
                 (sel (and (frame-live-p frame) (frame-selected-window frame))))
            (when (memq sel (lispm-ui-stage-windows stage frame))
              (lispm-ui-signal-set
               (lispm-ui-stage-zoom-signal stage)
               (lispm-ui-stage-zoom-create
                :subject subject
                :buffer (window-buffer sel)
                :kind (or (window-parameter sel 'lispm-stage-kind) 'code)
                :pinned-idle-p t
                :last-active (float-time)))))))
        (lispm-ui-signal-set
         (lispm-ui-stage-subjects-signal stage)
         (lispm-ui-stage--put-entry subjects subject entry))))))

(defun lispm-ui-stage-remove-subject (stage subject)
  "Remove SUBJECT and any zoom bound to it from STAGE."
  (when subject
    (let ((subjects (lispm-ui-signal-get (lispm-ui-stage-subjects-signal stage)))
          (zoom (lispm-ui-signal-get (lispm-ui-stage-zoom-signal stage))))
      (lispm-ui-frp-batch
        (when (and zoom (equal (lispm-ui-stage-zoom-subject zoom) subject))
          (lispm-ui-signal-set (lispm-ui-stage-zoom-signal stage) nil))
        (when (equal (lispm-ui-stage-subject stage) subject)
          (lispm-ui-signal-set (lispm-ui-stage-subject-signal stage) nil))
        (lispm-ui-signal-set
         (lispm-ui-stage-subjects-signal stage)
         (seq-remove (pcase-lambda (`(,k . ,_)) (equal k subject)) subjects))))))

(defun lispm-ui-stage-present (stage subject buffer &optional kind busy-p reveal-pos)
  "Record BUFFER of KIND for SUBJECT in STAGE and return its visible window.
When BUSY-P is provided, update SUBJECT's busy state in the same batch.
When REVEAL-POS is an integer, emit `(WINDOW . REVEAL-POS)' on
`reveal-stream' if BUFFER is displayed."
  (when (and subject (buffer-live-p buffer))
    (let* ((kind (or kind 'code))
           (subjects (lispm-ui-signal-get (lispm-ui-stage-subjects-signal stage)))
           (prev (cdr (assoc subject subjects #'equal)))
           (seq-no (cl-incf (lispm-ui-stage-seq stage)))
           (rest (seq-remove
                  (lambda (t0)
                    (or (eq (lispm-ui-stage-tile-buffer t0) buffer)
                        (not (buffer-live-p (lispm-ui-stage-tile-buffer t0)))))
                  (and prev (lispm-ui-stage-entry-tiles prev))))
           (tiles (seq-take
                   (cons (lispm-ui-stage-tile-create
                          :buffer buffer :kind kind :seq seq-no)
                         rest)
                   (max 1 (lispm-ui-stage-max-tiles stage))))
           (entry (lispm-ui-stage-entry-create
                   :busy-p (if busy-p t (and prev (lispm-ui-stage-entry-busy-p prev)))
                   :tiles tiles)))
      (lispm-ui-frp-batch
        (lispm-ui-signal-set
         (lispm-ui-stage-subjects-signal stage)
         (lispm-ui-stage--put-entry subjects subject entry)))
      (let* ((frame (lispm-ui-stage--target-frame stage subject))
             (win (and (frame-live-p frame) (get-buffer-window buffer frame))))
        (when (and (window-live-p win) (integerp reveal-pos))
          (lispm-ui-stream-emit (lispm-ui-stage-reveal-stream stage)
                                (cons win reveal-pos)))
        win))))

(defun lispm-ui-stage--refresh-zoom-time (stage zoom)
  "Update STAGE zoom signal timestamp using a fresh copy of ZOOM."
  (lispm-ui-signal-set
   (lispm-ui-stage-zoom-signal stage)
   (lispm-ui-stage-zoom-create
    :subject (lispm-ui-stage-zoom-subject zoom)
    :buffer (lispm-ui-stage-zoom-buffer zoom)
    :kind (lispm-ui-stage-zoom-kind zoom)
    :pinned-idle-p (lispm-ui-stage-zoom-pinned-idle-p zoom)
    :last-active (float-time))))

(defun lispm-ui-stage-zoom (stage window)
  "Zoom stage WINDOW across STAGE's column and freeze background switches."
  (when (and (window-live-p window)
             (lispm-ui-stage--owns-window-p stage window))
    (let* ((subj (lispm-ui-stage-subject stage))
           (busy (lispm-ui-stage-subject-busy-p stage subj))
           (buf (window-buffer window))
           (kind (or (window-parameter window 'lispm-stage-kind) 'code)))
      (when subj
        (lispm-ui-frp-batch
          (lispm-ui-stage-present stage subj buf kind busy)
          (lispm-ui-signal-set
           (lispm-ui-stage-zoom-signal stage)
           (lispm-ui-stage-zoom-create
            :subject subj
            :buffer buf
            :kind kind
            :pinned-idle-p (not busy)
            :last-active (float-time))))))))

(defun lispm-ui-stage-unzoom (stage &optional move-focus-p)
  "Leave focus-zoom on STAGE when its subject is still actively working.
If the subject is idle and `pinned-idle-p' holds, keep the single focused
buffer without re-splitting.  When MOVE-FOCUS-P is non-nil, move selection
back to the primary window first."
  (let* ((zoom (lispm-ui-signal-get (lispm-ui-stage-zoom-signal stage)))
         (subj (and zoom (lispm-ui-stage-zoom-subject zoom)))
         (busy (lispm-ui-stage-subject-busy-p stage subj)))
    (when zoom
      (when move-focus-p
        (let* ((frame (lispm-ui-stage--target-frame stage subj))
               (primary (and (frame-live-p frame)
                             (seq-find (lambda (w)
                                         (lispm-ui-stage--primary-p stage w))
                                       (window-list frame 'nomini)))))
          (when (window-live-p primary)
            (setf (lispm-ui-stage-inhibit-hooks stage) t)
            (unwind-protect
                (select-window primary t)
              (setf (lispm-ui-stage-inhibit-hooks stage) nil)))))
      (when (or busy (not (lispm-ui-stage-zoom-pinned-idle-p zoom)))
        (lispm-ui-signal-set (lispm-ui-stage-zoom-signal stage) nil))
      (lispm-ui-stage--sync-zoom-timer stage))))

(defun lispm-ui-stage-touch-zoom (stage &optional window)
  "Refresh zoom activity timestamp when WINDOW holds STAGE's zoomed buffer."
  (unless (lispm-ui-stage-inhibit-hooks stage)
    (let ((zoom (lispm-ui-signal-peek (lispm-ui-stage-zoom-signal stage)))
          (win (or window (selected-window))))
      (when (and (lispm-ui-stage-zoom-p zoom)
                 (window-live-p win)
                 (eq (window-buffer win) (lispm-ui-stage-zoom-buffer zoom)))
        (lispm-ui-stage--refresh-zoom-time stage zoom)))))

(defun lispm-ui-stage-on-selection-change (stage frame)
  "React to window selection changes on FRAME for STAGE."
  (unless (or (lispm-ui-stage-inhibit-hooks stage)
              (not (lispm-ui-stage-enabled-p stage))
              (not (frame-live-p frame)))
    (let ((win (frame-selected-window frame))
          (zoom (lispm-ui-signal-get (lispm-ui-stage-zoom-signal stage))))
      (cond
       ((and zoom (lispm-ui-stage--primary-p stage win))
        (lispm-ui-stage-unzoom stage nil))
       ((and (window-live-p win)
             (lispm-ui-stage--owns-window-p stage win)
             (not (lispm-ui-stage--primary-p stage win)))
        (if (and zoom (eq (window-buffer win) (lispm-ui-stage-zoom-buffer zoom)))
            (lispm-ui-stage--refresh-zoom-time stage zoom)
          (lispm-ui-stage-zoom stage win)))))))

(provide 'lispm-ui-stage)
;;; lispm-ui-stage.el ends here
