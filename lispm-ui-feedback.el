;;; lispm-ui-feedback.el --- Progress, spinner, skeleton, badge, avatar, banner, toast -*- lexical-binding: t; -*-

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

;; Modern feedback & status display primitives (GUI SVG + TTY fallback):
;;
;; - `lispm-ui-progress' (determinate / indeterminate with eighth-block TTY).
;; - `lispm-ui-spinner-frame' & shared-timer `lispm-ui-spinner-start'/`-stop'.
;; - `lispm-ui-skeleton' (`text', `card', `avatar-row' placeholders).
;; - `lispm-ui-badge' (semantic tones + optional icon/dot).
;; - `lispm-ui-avatar' (initials SVG/TTY badge with optional status indicator).
;; - `lispm-ui-empty-state' & `lispm-ui-banner'.
;; - Non-blocking toast queue (`lispm-ui-toast', `-active', `-dismiss', `-render').

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'lispm-ui-base)
(require 'lispm-ui-frp)
(require 'lispm-ui-palette)
(require 'lispm-ui-tokens)
(require 'lispm-ui-text)
(require 'lispm-ui-draw)
(require 'lispm-ui-icons)
(require 'lispm-ui-layout)
(require 'lispm-ui-controls-extra)

;;;; Progress bar

(defconst lispm-ui-feedback--blocks " ▏▎▍▌▋▊▉█"
  "Sub-cell horizontal bar blocks for smooth TTY progress bars.")

(defun lispm-ui-feedback--tone-colors (tone)
  "Return `(:fg :bg :border)' semantic colors for TONE."
  (pcase tone
    ('success (list :fg (or (lispm-ui-token :success-on-bg) "#15803d")
                    :bg (or (lispm-ui-token :success-bg) "#dcfce7")
                    :border (or (lispm-ui-token :success-border) "#22c55e")))
    ('warning (list :fg (or (lispm-ui-token :warning-on-bg) "#b45309")
                    :bg (or (lispm-ui-token :warning-bg) "#fef3c7")
                    :border (or (lispm-ui-token :warning-border) "#f59e0b")))
    ('info    (list :fg (or (lispm-ui-token :info-on-bg) "#1d4ed8")
                    :bg (or (lispm-ui-token :info-bg) "#dbeafe")
                    :border (or (lispm-ui-token :info-border) "#3b82f6")))
    ('danger  (list :fg (or (lispm-ui-token :danger-on-bg) "#b91c1c")
                    :bg (or (lispm-ui-token :danger-bg) "#fee2e2")
                    :border (or (lispm-ui-token :danger-border) "#ef4444")))
    ('accent
     (let ((bg (or (lispm-ui-color :accent) "#2563eb")))
       (list :fg (lispm-ui-wcag-legible bg "#ffffff" "#000000")
             :bg bg
             :border bg)))
    (_ (list :fg (or (lispm-ui-color :text) "#111111")
             :bg (or (lispm-ui-token :surface-2) "#e4e4e7")
             :border (or (lispm-ui-token :border-strong) "#a1a1aa")))))

(defun lispm-ui-feedback--draw-progress-svg (width ratio indeterminate phase tone)
  "Draw progress bar SVG WIDTH pixels wide."
  (let* ((w (max 24 width))
         (h 10)
         (svg (svg-create w h))
         (track (or (lispm-ui-token :surface-3) "#e4e4e7"))
         (fill (if (eq tone 'accent)
                   (or (lispm-ui-color :accent) "#2563eb")
                 (or (plist-get (lispm-ui-feedback--tone-colors tone) :border) "#2563eb"))))
    (svg-rectangle svg 0 1 w (- h 2) :rx 4 :fill track)
    (if indeterminate
        (let* ((seg (max 16 (/ w 3)))
               (x (mod (* (max 0 phase) 12) (max 1 (- w seg)))))
          (svg-rectangle svg x 1 seg (- h 2) :rx 4 :fill fill))
      (let ((fw (round (* w ratio))))
        (when (> fw 0)
          (svg-rectangle svg 0 1 (max 4 fw) (- h 2) :rx 4 :fill fill))))
    svg))

(defun lispm-ui-feedback--tty-progress (cells ratio indeterminate phase tone)
  "Render precision eighth-block TTY progress bar of CELLS columns."
  (let* ((fill-color (if (eq tone 'accent)
                         (or (lispm-ui-color :accent) "#2563eb")
                       (plist-get (lispm-ui-feedback--tone-colors tone) :fg)))
         (bar
          (if indeterminate
              (let* ((seg (max 2 (/ cells 4)))
                     (pos (mod (max 0 phase) (max 1 (- cells seg)))))
                (concat (make-string pos ?░)
                        (make-string seg ?█)
                        (make-string (max 0 (- cells pos seg)) ?░)))
            (let* ((eighths (round (* cells 8 ratio)))
                   (full (/ eighths 8))
                   (rem (mod eighths 8))
                   (part (if (> rem 0)
                             (char-to-string (aref lispm-ui-feedback--blocks rem))
                           ""))
                   (empty (max 0 (- cells full (length part)))))
              (concat (make-string full ?█) part (make-string empty ?░))))))
    (propertize bar 'face (when fill-color `(:foreground ,fill-color)))))

(cl-defun lispm-ui-progress (value &key (max 100) (width 180) label indeterminate
                                   (phase 0) (tone 'accent))
  "Return a rendered progress bar string for VALUE/MAX.
Supports determinate values and INDETERMINATE sliding phase animations."
  (let* ((m (max 1.0 (float (or max 100))))
         (ratio (max 0.0 (min 1.0 (/ (float (or value 0)) m))))
         (bar (if (lispm-ui-tty-p)
                  (let ((cells (max 8 (if (<= width 60) width (lispm-ui-px width)))))
                    (lispm-ui-feedback--tty-progress cells ratio indeterminate phase tone))
                (let ((q (round (* 100 ratio))))
                  (propertize " " 'display
                              (lispm-ui-look-image
                               (list 'progress width (if indeterminate (list 'ind phase) q) tone)
                               (lambda ()
                                 (lispm-ui-feedback--draw-progress-svg
                                  width ratio indeterminate phase tone))))))))
    (if label
        (lispm-ui-hstack (list bar (propertize label 'face 'lispm-ui-caption)) :gap 'sm)
      bar)))

;;;; Spinner (single shared timer)

(defconst lispm-ui-spinner-frames ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"]
  "Smooth braille frames for `lispm-ui-spinner-frame'.")

(defvar lispm-ui-spinner--clients nil
  "Active list of spinner callback entries.")

(defvar lispm-ui-spinner--timer nil
  "Single shared timer driving all active spinners.")

(defvar lispm-ui-spinner--step 0
  "Monotonic spinner animation tick count.")

(cl-defun lispm-ui-spinner-frame (&optional (step 0) label (tone 'accent))
  "Return rendered spinner frame STEP with optional LABEL in TONE."
  (let* ((glyph (aref lispm-ui-spinner-frames (mod (max 0 step) (length lispm-ui-spinner-frames))))
         (color (if (eq tone 'accent)
                    (or (lispm-ui-color :accent) "#2563eb")
                  (plist-get (lispm-ui-feedback--tone-colors tone) :fg)))
         (spin (propertize glyph 'face (if color `(:foreground ,color :weight bold) 'bold))))
    (if label
        (concat spin " " (propertize label 'face 'lispm-ui-body))
      spin)))

(defun lispm-ui-spinner--tick ()
  "Advance shared spinner frame and invoke active callbacks safely."
  (setq lispm-ui-spinner--step (1+ lispm-ui-spinner--step))
  (dolist (entry (copy-sequence lispm-ui-spinner--clients))
    (ignore-errors (funcall (cdr entry) lispm-ui-spinner--step))))

(defun lispm-ui-spinner-stop (handle)
  "Stop spinner HANDLE and cancel shared timer when idle."
  (setq lispm-ui-spinner--clients
        (assq-delete-all (car-safe handle) lispm-ui-spinner--clients))
  (when (and (null lispm-ui-spinner--clients)
             (timerp lispm-ui-spinner--timer))
    (cancel-timer lispm-ui-spinner--timer)
    (setq lispm-ui-spinner--timer nil))
  nil)

(cl-defun lispm-ui-spinner-start (callback &key (interval 0.1) scope)
  "Register spinner animation CALLBACK and return stoppable handle."
  (let* ((id (gensym "spin-"))
         (entry (cons id callback)))
    (push entry lispm-ui-spinner--clients)
    (unless (timerp lispm-ui-spinner--timer)
      (setq lispm-ui-spinner--timer
            (run-with-timer interval interval #'lispm-ui-spinner--tick)))
    (when scope
      (lispm-ui-scope-add scope (lambda () (lispm-ui-spinner-stop entry))))
    entry))

;;;; Skeleton placeholders

(cl-defun lispm-ui-skeleton (&key (lines 3) (width 32) (variant 'text))
  "Return skeleton loading placeholder block (`text', `card', or `avatar-row')."
  (let* ((bg (or (lispm-ui-token :surface-2) "#e4e4e7"))
         (fg (or (lispm-ui-token :muted) "#a1a1aa"))
         (face `(:background ,bg :foreground ,fg))
         (row (lambda (len)
                (propertize (make-string (max 4 len) ?░) 'face face))))
    (pcase variant
      ('avatar-row
       (lispm-ui-hstack
        (list (propertize "   " 'face face)
              (lispm-ui-vstack (list (funcall row (max 10 (/ width 2)))
                                     (funcall row (max 8 (/ width 3))))
                               :gap 'xs))
        :gap 'sm))
      ('card
       (lispm-ui-card
        (lispm-ui-skeleton :lines (max 2 lines) :width (max 16 (- width 4)) :variant 'text)
        :width width))
      (_
       (let (out)
         (dotimes (i (max 1 lines))
           (let ((len (if (= i (1- lines)) (max 6 (/ (* width 2) 3)) width)))
             (push (funcall row len) out)))
         (lispm-ui-vstack (nreverse out) :gap 'xs))))))

;;;; Badge / Pill & Avatar

(cl-defun lispm-ui-badge (label &key (tone 'neutral) icon dot)
  "Return a compact colored pill badge reading LABEL in TONE."
  (let* ((colors (lispm-ui-feedback--tone-colors tone))
         (bg (plist-get colors :bg))
         (fg (plist-get colors :fg))
         (dot-str (when dot (propertize "●" 'face `(:foreground ,(plist-get colors :border)))))
         (icon-str (when icon (lispm-ui-icon icon :size 12 :tint fg)))
         (parts (delq nil (list dot-str icon-str (or label ""))))
         (text (concat " " (mapconcat #'identity parts " ") " ")))
    (propertize text 'face `(:inherit lispm-ui-caption :background ,bg :foreground ,fg
                            :box (:line-width 1 :color ,(plist-get colors :border))))))

(defun lispm-ui-feedback--initials (name)
  "Extract 1-2 uppercase initials from NAME."
  (let* ((words (split-string (string-trim (or name "?")) "[ \t\n_-]+" t)))
    (cond
     ((>= (length words) 2)
      (upcase (concat (substring (car words) 0 1)
                      (substring (cadr words) 0 1))))
     ((car words)
      (upcase (substring (car words) 0 (min 2 (length (car words))))))
     (t "?"))))

(defun lispm-ui-feedback--avatar-color (name)
  "Pick deterministic accent hex color from NAME hash."
  (let ((palette '("#2563eb" "#7c3aed" "#059669" "#d97706" "#dc2626" "#0891b2")))
    (nth (mod (abs (sxhash (or name ""))) (length palette)) palette)))

(defun lispm-ui-feedback--draw-avatar-svg (name size status)
  "Return SIZE x SIZE circular initials avatar SVG for NAME with STATUS dot."
  (let* ((s (max 16 size))
         (r (/ s 2.0))
         (svg (svg-create s s))
         (bg (lispm-ui-feedback--avatar-color name))
         (fg (lispm-ui-wcag-legible bg "#ffffff" "#000000"))
         (init (lispm-ui-feedback--initials name)))
    (svg-circle svg r r (1- r) :fill bg)
    (lispm-ui-svg-content svg init (/ (- s (lispm-ui-text-width init 'lispm-ui-small)) 2)
                          1 (- s 2) fg 'lispm-ui-small)
    (when status
      (let ((dot-c (pcase status
                     ('online "#22c55e") ('busy "#ef4444") ('away "#f59e0b") (_ "#9ca3af"))))
        (svg-circle svg (- s 4) (- s 4) 3.5 :fill dot-c :stroke "#ffffff" :stroke-width 1)))
    svg))

(cl-defun lispm-ui-avatar (name &key image (size 28) status)
  "Return a circular initials or image avatar string for NAME."
  (let ((init (lispm-ui-feedback--initials name)))
    (if (lispm-ui-tty-p)
        (let* ((bg (lispm-ui-feedback--avatar-color name))
               (fg (lispm-ui-wcag-legible bg "#ffffff" "#000000")))
          (propertize (format "[%s]" init)
                      'face `(:background ,bg :foreground ,fg :weight bold)))
      (if (and image (file-readable-p image) (display-images-p)
               (or (not (string-suffix-p ".svg" image t))
                   (image-type-available-p 'svg)))
          (propertize " " 'display (create-image image nil nil :width size :height size :ascent 'center))
        (propertize " " 'display
                    (lispm-ui-look-image
                     (list 'avatar init size status)
                     (lambda () (lispm-ui-feedback--draw-avatar-svg name size status))))))))

;;;; EmptyState & Banner

(cl-defun lispm-ui-empty-state (title &key description icon action action-label (width 44))
  "Return a structured empty-state block reading TITLE with optional action."
  (let* ((icon-s (when icon (lispm-ui-icon icon :size 24 :tint :muted)))
         (head (concat (when icon-s (concat icon-s "  "))
                       (propertize (or title "") 'face 'lispm-ui-h3)))
         (desc (when description (propertize description 'face 'lispm-ui-muted)))
         (btn (when action
                (let ((b nil))
                  (with-temp-buffer
                    (lispm-ui-insert-button (or action-label "Action") action :variant 'primary)
                    (setq b (buffer-string)))
                  b))))
    (lispm-ui-card (lispm-ui-vstack (delq nil (list head desc btn)) :gap 'sm)
                   :width width)))

(cl-defun lispm-ui-banner (message &key (tone 'info) title icon action action-label
                                 dismiss-action (width 56))
  "Return an inline alert banner / callout in TONE with optional action."
  (let* ((colors (lispm-ui-feedback--tone-colors tone))
         (fg (plist-get colors :fg))
         (bg (plist-get colors :bg))
         (ic (lispm-ui-icon (or icon (pcase tone
                                       ('success 'success)
                                       ('warning 'warning)
                                       ('danger 'error)
                                       (_ 'info)))
                            :tint fg))
         (body (concat ic " "
                       (when title (propertize (concat title ": ") 'face `(:foreground ,fg :weight bold)))
                       (propertize (or message "") 'face `(:foreground ,fg))))
         (act-btn (when action
                    (with-temp-buffer
                      (lispm-ui-insert-button (or action-label "Retry") action :variant 'ghost)
                      (buffer-string))))
         (close-btn (when dismiss-action
                      (with-temp-buffer
                        (lispm-ui-insert-button "×" dismiss-action :variant 'ghost)
                        (buffer-string)))))
    (lispm-ui-card (lispm-ui-hstack (delq nil (list body 'flex act-btn close-btn)) :gap 'xs)
                   :width width
                   :surface bg
                   :border (plist-get colors :border))))

;;;; Non-blocking Toast queue

(defvar lispm-ui-toast--items nil
  "List of currently active toast notification plists.")

(defvar lispm-ui-toast-max-visible 5
  "Maximum number of active toast notifications retained in the queue.")

(defun lispm-ui-toast-active ()
  "Return shallow copy list of active toast notification plists."
  (copy-sequence lispm-ui-toast--items))

(defun lispm-ui-toast-dismiss (id)
  "Dismiss toast identified by ID and cancel its expiry timer."
  (let ((kept nil))
    (dolist (toast lispm-ui-toast--items)
      (if (equal (plist-get toast :id) id)
          (when-let* ((tm (plist-get toast :timer)))
            (when (timerp tm) (cancel-timer tm)))
        (push toast kept)))
    (setq lispm-ui-toast--items (nreverse kept)))
  nil)

(defun lispm-ui-toast-clear ()
  "Dismiss all queued toast notifications and cancel their timers."
  (dolist (toast lispm-ui-toast--items)
    (when-let* ((tm (plist-get toast :timer)))
      (when (timerp tm) (cancel-timer tm))))
  (setq lispm-ui-toast--items nil))

(cl-defun lispm-ui-toast (message &key (tone 'info) (duration 3.0) title id)
  "Enqueue a non-blocking toast reading MESSAGE in TONE for DURATION seconds."
  (let* ((tid (or id (gensym "toast-")))
         (_ (lispm-ui-toast-dismiss tid))
         (timer (when (and (numberp duration) (> duration 0))
                  (run-with-timer duration nil #'lispm-ui-toast-dismiss tid)))
         (entry (list :id tid :message message :tone tone :title title :timer timer)))
    (setq lispm-ui-toast--items (cons entry lispm-ui-toast--items))
    (when (> (length lispm-ui-toast--items) lispm-ui-toast-max-visible)
      (let ((tail (nthcdr lispm-ui-toast-max-visible lispm-ui-toast--items)))
        (dolist (old tail)
          (when-let* ((tm (plist-get old :timer)))
            (when (timerp tm) (cancel-timer tm))))
        (setq lispm-ui-toast--items
              (seq-take lispm-ui-toast--items lispm-ui-toast-max-visible))))
    tid))

(defun lispm-ui-toast-render (&optional width)
  "Render active toasts stacked vertically as banner cards."
  (if (null lispm-ui-toast--items)
      ""
    (lispm-ui-vstack
     (mapcar (lambda (toast)
               (let ((tid (plist-get toast :id)))
                 (lispm-ui-banner (plist-get toast :message)
                                  :tone (plist-get toast :tone)
                                  :title (plist-get toast :title)
                                  :width (or width 44)
                                  :dismiss-action (lambda () (lispm-ui-toast-dismiss tid)))))
             lispm-ui-toast--items)
     :gap 'xs)))

(provide 'lispm-ui-feedback)
;;; lispm-ui-feedback.el ends here
