;;; lispm-ui-sheet.el --- Classic property-sheet headers, tabs, and button rows -*- lexical-binding: t; -*-

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

;; Selectable property-sheet buffer primitives rendered with LispM UI's
;; palette, text metrics, tabs, beveled portrait frames, and wrapping
;; push-button rows in both graphical frames and text terminals.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'button)
(require 'lispm-ui-base)
(require 'lispm-ui-palette)
(require 'lispm-ui-text)
(require 'lispm-ui-draw)
(require 'lispm-ui-tabs)
(require 'lispm-ui-controls)
(require 'lispm-ui-scene-sprites)
(require 'lispm-ui-scene)

;;;; Button flow cursor

(cl-defstruct (lispm-ui-sheet-flow
               (:constructor lispm-ui-sheet-flow-create)
               (:copier nil))
  "Tracks horizontal button-row wrapping across sequential insertions."
  (width 0 :type integer)
  end)

;;;; Inert text & width measurement

(defun lispm-ui-sheet-oneline (value)
  "Format VALUE as a single trimmed line of inert text."
  (string-trim
   (replace-regexp-in-string
    "[[:space:][:cntrl:]]+" " "
    (substring-no-properties (format "%s" (or value ""))))))

(defun lispm-ui-sheet-inert (value)
  "Render VALUE as inert text without inherited display or key properties."
  (substring-no-properties (if (stringp value) value (format "%s" value))))

(defun lispm-ui-sheet-measure (text)
  "Return visible width of TEXT in pixels on GUI or cells on TTY."
  (if (lispm-ui-tty-p) (string-width text) (lispm-ui-text-width text)))

(defun lispm-ui-sheet-fit (text width)
  "Fit TEXT to WIDTH preserving faces and setting full TEXT in help-echo."
  (let* ((width (max 1 (floor width)))
         (fitted (if (lispm-ui-tty-p)
                     (truncate-string-to-width text width nil nil "…")
                   (lispm-ui-text-fit text width))))
    (when (> (length fitted) 0)
      (put-text-property 0 (length fitted) 'help-echo
                         (or (get-text-property 0 'help-echo text)
                             (substring-no-properties text))
                         fitted))
    fitted))

(defun lispm-ui-sheet-available-width (&optional margin-tty margin-px)
  "Return drawable width of current buffer's window minus margins."
  (let* ((window (or (get-buffer-window (current-buffer) 'visible)
                     (selected-window)))
         (tty (lispm-ui-tty-p)))
    (max 1 (- (window-body-width window (not tty))
              (if tty 1 0)
              (if tty (or margin-tty 0) (or margin-px 0))))))

(defun lispm-ui-sheet-center (text width)
  "Insert one horizontally centered line of TEXT within WIDTH."
  (let* ((fitted (lispm-ui-sheet-fit text width))
         (padding (max 0 (/ (- width (lispm-ui-sheet-measure fitted)) 2))))
    (insert (if (lispm-ui-tty-p)
                (make-string (floor padding) ?\s)
              (lispm-ui-text-space padding))
            fitted "\n")))

;;;; Visual header components

(defun lispm-ui-sheet-status-chip (status)
  "Return a passive, selectable state pill string for STATUS."
  (unless lispm-ui-current-palette
    (lispm-ui-palette-follow-theme))
  (let* ((palette lispm-ui-current-palette)
         (chrome (plist-get palette :chrome))
         (info (lispm-ui-scene-state-info status))
         (accent (or (and (plist-get info :accent-key)
                          (plist-get palette (plist-get info :accent-key)))
                     (plist-get info :accent-color)
                     (plist-get palette :muted)))
         (ground (or (lispm-ui-blend chrome accent 0.17) chrome))
         (ink (lispm-ui-legible ground
                                (plist-get palette :text)
                                (plist-get palette :light)
                                (plist-get palette :outline))))
    (propertize (concat " " (plist-get info :chip-label) " ")
                'face `(:inherit lispm-ui-small :foreground ,ink
                        :background ,ground
                        :box (:line-width 1 :color ,(lispm-ui-grey palette #xA0)))
                'lispm-ui-sheet-status (plist-get info :state))))

(defun lispm-ui-sheet-portrait-image (node &optional size)
  "Return NODE's inset beveled portrait image SVG with side SIZE (default 32)."
  (unless lispm-ui-current-palette
    (lispm-ui-palette-follow-theme))
  (let* ((size (or size 32))
         (palette lispm-ui-current-palette)
         (svg (svg-create size size))
         (inside (- size 8))
         (avatar (plist-get node :avatar)))
    (lispm-ui-svg-fill svg 0 0 size size (plist-get palette :chrome))
    (lispm-ui-svg-edges svg 0 0 size size
                        (list (lispm-ui-grey palette #xFF))
                        (list (lispm-ui-grey palette #x88)))
    (lispm-ui-svg-fill svg 3 3 (- size 6) (- size 6)
                       (plist-get palette :content))
    (lispm-ui-svg-edges svg 2 2 (- size 4) (- size 4)
                        (list (lispm-ui-grey palette #x88))
                        (list (lispm-ui-grey palette #xFF)))
    (unless (lispm-ui-scene-draw-avatar svg avatar 4 4 inside)
      (lispm-ui-scene-draw-pixel-portrait
       svg (lispm-ui-scene-portrait-for-node node) 4 4 (/ inside 16.0)))
    (lispm-ui-svg-image svg)))

(defun lispm-ui-sheet-header (name identity status activity width)
  "Insert property-sheet header for NAME, IDENTITY, STATUS, ACTIVITY within WIDTH."
  (unless lispm-ui-current-palette
    (lispm-ui-palette-follow-theme))
  (let* ((width (max 1 (floor width)))
         (tty (lispm-ui-tty-p))
         (padding (if tty 2 16))
         (content-width (max 1 (- width padding)))
         (title (lispm-ui-sheet-oneline (or name "Agent")))
         (summary (lispm-ui-sheet-oneline activity)))
    (unless (bolp) (insert "\n"))
    (lispm-ui-sheet-center (propertize title 'face 'lispm-ui-title) width)
    (let* ((chip (lispm-ui-sheet-status-chip status))
           (portrait (and (not tty) (image-type-available-p 'svg)
                          (>= width (+ 48 (lispm-ui-sheet-measure chip)))
                          (propertize " " 'display
                                      (lispm-ui-sheet-portrait-image identity 32)
                                      'help-echo (concat title " portrait")))))
      (lispm-ui-sheet-center
       (if portrait (concat portrait (lispm-ui-text-space 8) chip) chip)
       width))
    (unless (string-empty-p summary)
      (lispm-ui-sheet-center
       (lispm-ui-sheet-fit
        (propertize summary 'face 'shadow 'help-echo summary) content-width)
       width))))

(defun lispm-ui-sheet-heading (text width)
  "Insert an engraved property-sheet section heading TEXT within WIDTH."
  (unless lispm-ui-current-palette
    (lispm-ui-palette-follow-theme))
  (unless (bolp) (insert "\n"))
  (let* ((width (max 1 (floor width)))
         (padding (if (lispm-ui-tty-p) 5 32))
         (caption (lispm-ui-sheet-fit
                   (propertize (lispm-ui-sheet-oneline text) 'face 'lispm-ui-title)
                   (max 1 (- width padding)))))
    (if (< width padding)
        (insert (lispm-ui-sheet-fit caption width))
      (insert (lispm-ui-group-edge width caption)))
    (insert "\n")))

(defun lispm-ui-sheet-empty (text)
  "Insert a quiet, inert empty-state message TEXT."
  (insert (propertize (lispm-ui-sheet-inert text) 'face 'shadow) "\n"))

(defun lispm-ui-sheet-tabs (specs active width on-select)
  "Insert a native tab row from SPECS alist, highlighting ACTIVE within WIDTH.
ON-SELECT is called with the chosen tab key inside the owning buffer."
  (let* ((owner (current-buffer))
         (tabs (mapcar (lambda (entry)
                         (list (cdr entry) (eq (car entry) active)
                               'button (make-symbol "sheet-tab")
                               'category 'default-button
                               'keymap button-map
                               'follow-link t
                               'mouse-face 'highlight
                               'help-echo (cdr entry)
                               'lispm-ui-label (cdr entry)
                               'action (lambda (_)
                                         (when (buffer-live-p owner)
                                           (with-current-buffer owner
                                             (funcall on-select (car entry)))))))
                       specs)))
    (unless (lispm-ui-tty-p)
      (dolist (tab tabs) (nconc tab (list 'face 'default))))
    (insert (lispm-ui-tabs tabs width) "\n")))

(defun lispm-ui-sheet-insert-button
    (label function arguments available-width &optional flow on-complete)
  "Insert push button LABEL invoking FUNCTION on ARGUMENTS, wrapping in FLOW.
AVAILABLE-WIDTH bounds row length.  If ON-COMPLETE is non-nil, it is
called with zero arguments inside the owning buffer after FUNCTION runs."
  (when flow
    (unless (equal (lispm-ui-sheet-flow-end flow) (point))
      (setf (lispm-ui-sheet-flow-width flow) 0))
    (let* ((tty (lispm-ui-tty-p))
           (width (if tty (+ (string-width label) 4)
                    (+ (lispm-ui-push-button-width label) 4)))
           (gap (if tty 1 (string-pixel-width " "))))
      (when (and (> (lispm-ui-sheet-flow-width flow) 0)
                 (> (+ (lispm-ui-sheet-flow-width flow) width gap)
                    available-width))
        (insert "\n")
        (setf (lispm-ui-sheet-flow-width flow) 0))
      (cl-incf (lispm-ui-sheet-flow-width flow) (+ width gap))))
  (let ((owner (current-buffer)))
    (lispm-ui-insert-push-button
     label
     (lambda ()
       (apply function arguments)
       (when (and on-complete (buffer-live-p owner))
         (with-current-buffer owner (funcall on-complete))))))
  (insert " ")
  (when flow
    (setf (lispm-ui-sheet-flow-end flow) (point))))

(provide 'lispm-ui-sheet)
;;; lispm-ui-sheet.el ends here
