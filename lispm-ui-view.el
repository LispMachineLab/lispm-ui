;;; lispm-ui-view.el --- UI buffer hygiene base mode and scroll preservation -*- lexical-binding: t; -*-

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

;; Base major mode (`lispm-ui-view-mode') and scroll-preserving refresh
;; (`lispm-ui-with-preserved-scroll') for interactive LispM UI application
;; buffers:
;;
;; - Suppresses line numbers, hl-line, bidi reordering overhead,
;;   trailing whitespace highlights, empty-line fringe markers, Evil
;;   visual cursors, and noisy `$' truncation markers.
;; - Wires `TAB'/`S-TAB' focus traversal across buttons, widgets, and
;;   `lispm-ui-focusable' controls, clean `<down-mouse-1>' activation
;;   without stray text selection, and `lispm-ui-show-focus' tracking.
;; - Attaches a buffer-owned `lispm-ui-scope' (`lispm-ui-view-scope')
;;   disposed automatically on `kill-buffer' or major-mode change.
;; - Preserves point and window viewport alignment by `lispm-ui-item-key'
;;   and visual row offset across incremental buffer redraws.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'button)
(require 'wid-edit)
(require 'lispm-ui-base)
(require 'lispm-ui-frp)
(require 'lispm-ui-palette)
(require 'lispm-ui-tokens)
(require 'lispm-ui-controls)

(declare-function global-hl-line-unhighlight "hl-line" ())
(defvar global-hl-line-mode)
(defvar hl-line-mode)
(defvar display-line-numbers-mode)
(defvar evil-normal-state-cursor)
(defvar evil-motion-state-cursor)
(defvar evil-emacs-state-cursor)

(defvar-local lispm-ui-view-scope nil
  "Buffer-owned `lispm-ui-scope' disposed on buffer kill or mode switch.")

(defvar-local lispm-ui-view-show-cursor nil
  "When non-nil, keep cursor visible in GUI frames inside `lispm-ui-view-mode'.")

(defvar-local lispm-ui-view-render-function nil
  "Zero-argument refresh callback invoked by `lispm-ui-view-refresh' (`g').")

;;;; Interactive focus traversal & mouse hygiene

(defun lispm-ui-view--control-start (pos)
  "Return start position of control at POS, or nil when POS is non-interactive."
  (when (<= (point-min) pos (point-max))
    (cond
     ((when-let* ((btn (button-at pos)))
        (button-start btn)))
     ((when-let* ((wid (widget-at pos)))
        (or (when-let* ((ov (widget-get wid :field-overlay))
                        ((overlayp ov)))
              (overlay-start ov))
            (widget-get wid :from))))
     ((get-text-property pos 'lispm-ui-focusable)
      (or (previous-single-property-change (min (point-max) (1+ pos))
                                           'lispm-ui-focusable)
          pos)))))

(defun lispm-ui-view--control-positions ()
  "Return sorted start positions of all focusable controls in current buffer."
  (let ((pos (point-min))
        (result nil))
    (while (< pos (point-max))
      (let ((start (lispm-ui-view--control-start pos)))
        (when (and start (not (memq start result)))
          (push start result)))
      (let ((next (next-char-property-change pos)))
        (setq pos (if (> next pos) next (1+ pos)))))
    (nreverse result)))

(defun lispm-ui-view-next-control (&optional count)
  "Move point forward COUNT focusable controls in current UI view."
  (interactive "p")
  (let* ((step (or count 1))
         (positions (lispm-ui-view--control-positions)))
    (if (null positions)
        nil
      (let* ((here (lispm-ui-view--control-start (point)))
             (idx (and here (seq-position positions here)))
             (next (cond
                    (idx (nth (mod (+ idx step) (length positions)) positions))
                    ((>= step 0)
                     (or (seq-find (lambda (p) (> p (point))) positions)
                         (car positions)))
                    (t
                     (or (car (last (seq-filter (lambda (p) (< p (point))) positions)))
                         (car (last positions)))))))
        (when next
          (goto-char next))
        next))))

(defun lispm-ui-view-prev-control (&optional count)
  "Move point backward COUNT focusable controls in current UI view."
  (interactive "p")
  (lispm-ui-view-next-control (- (or count 1))))

(defun lispm-ui-view-mouse-down (event)
  "Handle `<down-mouse-1>' EVENT cleanly without initiating canvas drag selection."
  (interactive "e")
  (let* ((posn (event-start event))
         (win (posn-window posn))
         (pt (posn-point posn)))
    (when (window-live-p win)
      (select-window win)
      (when (integerp pt)
        (goto-char (min (point-max) (max (point-min) pt))))
      (let ((btn (and (integerp pt) (button-at pt))))
        (cond
         (btn (push-button pt))
         ((and (integerp pt) (widget-at pt))
          (widget-button-press pt)))))))

(defun lispm-ui-view-refresh (&rest _)
  "Refresh current `lispm-ui-view-mode' buffer via `lispm-ui-view-render-function'."
  (interactive)
  (when (functionp lispm-ui-view-render-function)
    (funcall lispm-ui-view-render-function)))

(defvar-keymap lispm-ui-view-mode-map
  :doc "Base keymap for `lispm-ui-view-mode' interactive UI buffers."
  :parent special-mode-map
  "TAB" #'lispm-ui-view-next-control
  "<backtab>" #'lispm-ui-view-prev-control
  "S-<tab>" #'lispm-ui-view-prev-control
  "<down-mouse-1>" #'lispm-ui-view-mouse-down
  "<mouse-1>" #'ignore
  "<drag-mouse-1>" #'ignore
  "g" #'lispm-ui-view-refresh)

;;;; Buffer hygiene base mode

(defun lispm-ui-view--dispose-scope ()
  "Dispose `lispm-ui-view-scope' for the current buffer if present."
  (when (lispm-ui-scope-p lispm-ui-view-scope)
    (lispm-ui-scope-dispose lispm-ui-view-scope)
    (setq lispm-ui-view-scope nil)))

(defun lispm-ui-view--apply-hygiene ()
  "Apply UI buffer hygiene settings cleanly for `lispm-ui-view-mode'."
  (when (fboundp 'display-line-numbers-mode)
    (when (bound-and-true-p display-line-numbers-mode)
      (display-line-numbers-mode -1)))
  (when (bound-and-true-p hl-line-mode)
    (hl-line-mode -1))
  (setq-local global-hl-line-mode nil
              truncate-lines t
              bidi-paragraph-direction 'left-to-right
              bidi-inhibit-bpa t
              show-trailing-whitespace nil
              indicate-empty-lines nil
              buffer-undo-list t)
  (when (fboundp 'global-hl-line-unhighlight)
    (global-hl-line-unhighlight))
  (when (or (boundp 'evil-normal-state-cursor) (featurep 'evil))
    (setq-local evil-normal-state-cursor '(nil)
                evil-motion-state-cursor '(nil)
                evil-emacs-state-cursor '(nil)))
  (unless (or (lispm-ui-tty-p) lispm-ui-view-show-cursor)
    (setq-local cursor-type nil))
  (let ((table (make-display-table)))
    (set-display-table-slot table 'truncation (make-glyph-code ?… 'shadow))
    (setq-local buffer-display-table table)))

(define-derived-mode lispm-ui-view-mode special-mode "LispM-View"
  "Base major mode applying clean UI buffer hygiene and reactive scope ownership.
\\{lispm-ui-view-mode-map}"
  (lispm-ui-view--apply-hygiene)
  (lispm-ui-view--dispose-scope)
  (setq lispm-ui-view-scope
        (lispm-ui-scope-attach-buffer (lispm-ui-scope-create) (current-buffer)))
  (add-hook 'change-major-mode-hook #'lispm-ui-view--dispose-scope nil t)
  (add-hook 'post-command-hook #'lispm-ui-show-focus nil t)
  (setq-local revert-buffer-function #'lispm-ui-view-refresh))

(defmacro lispm-ui-view-with-scope (&rest body)
  "Evaluate BODY inside the current buffer's `lispm-ui-view-scope'."
  (declare (indent 0) (debug t))
  `(lispm-ui-with-scope lispm-ui-view-scope
     ,@body))

;;;; Scroll-preserving incremental refresh

(defun lispm-ui-view--find-key-pos (key key-prop)
  "Return first position in current buffer where KEY-PROP equals KEY, or nil."
  (when key
    (let ((pos (point-min))
          (found nil))
      (while (and (not found) (< pos (point-max)))
        (if (equal (get-text-property pos key-prop) key)
            (setq found pos)
          (setq pos (next-single-property-change pos key-prop nil (point-max)))))
      found)))

(defun lispm-ui-view--capture-scroll (&optional key-prop)
  "Capture buffer point and window scroll state keyed by KEY-PROP."
  (let* ((prop (or key-prop 'lispm-ui-item-key))
         (buf (current-buffer))
         (pt (point))
         (pt-key (and (< pt (point-max)) (get-text-property pt prop)))
         (pt-line (line-number-at-pos pt t))
         (pt-col (- pt (line-beginning-position)))
         (wins nil))
    (dolist (win (get-buffer-window-list buf nil t))
      (when (window-live-p win)
        (let* ((wpt (window-point win))
               (wkey (and (< wpt (point-max)) (get-text-property wpt prop)))
               (wstart (window-start win))
               (wline (line-number-at-pos wpt t))
               (row-off (max 0 (- wline (line-number-at-pos wstart t)))))
          (save-excursion
            (goto-char wpt)
            (let ((wcol (- wpt (line-beginning-position))))
              (push (list win wkey row-off wline wcol (window-hscroll win)) wins))))))
    (list :prop prop :pt-key pt-key :pt-line pt-line :pt-col pt-col :wins wins)))

(defun lispm-ui-view--fallback-pos (line col)
  "Return clamped position at LINE and COL in current buffer."
  (save-excursion
    (goto-char (point-min))
    (forward-line (max 0 (1- line)))
    (min (line-end-position) (+ (point) (max 0 col)))))

(defun lispm-ui-view--restore-scroll (state)
  "Restore buffer point and window scroll alignment from captured STATE."
  (let* ((prop (plist-get state :prop))
         (pt-target (or (lispm-ui-view--find-key-pos (plist-get state :pt-key) prop)
                        (lispm-ui-view--fallback-pos (plist-get state :pt-line)
                                                     (plist-get state :pt-col)))))
    (goto-char pt-target)
    (pcase-dolist (`(,win ,wkey ,row-off ,wline ,wcol ,hscroll)
                   (plist-get state :wins))
      (when (window-live-p win)
        (let ((target (or (lispm-ui-view--find-key-pos wkey prop)
                          (lispm-ui-view--fallback-pos wline wcol))))
          (set-window-point win target)
          (set-window-start
           win
           (save-excursion
             (goto-char target)
             (forward-line (- (max 0 row-off)))
             (line-beginning-position))
           t)
          (set-window-hscroll win (max 0 (or hscroll 0))))))))

(defmacro lispm-ui-with-preserved-scroll (options &rest body)
  "Execute BODY while preserving point and window scroll positions.
OPTIONS may be nil or a plist accepting `:key-prop' (default
`lispm-ui-item-key')."
  (declare (indent 1) (debug (sexp body)))
  (let* ((raw-prop (plist-get options :key-prop))
         (prop-expr (cond
                     ((null raw-prop) ''lispm-ui-item-key)
                     ((and (consp raw-prop) (eq (car raw-prop) 'quote)) raw-prop)
                     ((symbolp raw-prop) `',raw-prop)
                     (t raw-prop)))
         (state-sym (make-symbol "scroll-state")))
    `(let ((,state-sym (lispm-ui-view--capture-scroll ,prop-expr)))
       (unwind-protect
           (progn ,@body)
         (lispm-ui-view--restore-scroll ,state-sym)))))

(provide 'lispm-ui-view)
;;; lispm-ui-view.el ends here
