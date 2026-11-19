;;; lispm-finder-render.el --- Header strip, icon grid and TTY rendering for Finder  -*- lexical-binding: t; -*-

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

;; Header strips, bevel header buttons, 32x32 SVG icon darkening and
;; caching, text-terminal block rendering, and buffer layout for
;; `lispm-finder'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'lispm-ui)
(require 'lispm-finder-i18n)

(defvar lispm-finder-buffer-name)
(defvar lispm-finder-choose-buffer-name)
(defvar lispm-finder-cell-width)
(defvar lispm-finder-icon-size)
(defvar lispm-finder--directory)
(defvar lispm-finder--items)
(defvar lispm-finder--selected)
(defvar lispm-finder--columns)
(defvar lispm-finder--choosing)
(defvar lispm-finder-tty-p-function)
(defvar lispm-finder-tty-images-p-function)
(defvar lispm-finder-tty-images-show-function)
(defvar lispm-finder-text-items-function)
(defvar lispm-finder-press-function)
(defvar lispm-finder-translate-function)
(defvar lispm-finder-rendered-hook)
(defvar lispm-finder-extra-item-properties)
(defvar lispm-finder-extra-target-properties)
(defvar lispm-finder-item-map)

(declare-function lispm-finder--folder "lispm-finder" (directory))
(declare-function lispm-finder--parent "lispm-finder" (directory))
(declare-function lispm-finder--icon-name "lispm-finder" (directory item))
(declare-function lispm-finder--read-again "lispm-finder" ())
(declare-function lispm-finder--item-position "lispm-finder" (name &optional label))
(declare-function lispm-finder--choose-header "lispm-finder-choose" ())
(declare-function lispm-finder-up "lispm-finder" ())

;;;; Target & item property helpers

(defun lispm-finder--target-plist (target)
  "Return text properties identifying press TARGET."
  (let ((props (list 'lispm-ui-target target 'lispm-finder-target target)))
    (dolist (prop lispm-finder-extra-target-properties props)
      (setq props (plist-put props prop target)))))

(defun lispm-finder--item-plist (name)
  "Return text properties identifying item NAME."
  (let ((props (list 'lispm-finder-item name
                     'keymap lispm-finder-item-map
                     'pointer 'arrow
                     'help-echo name)))
    (dolist (prop lispm-finder-extra-item-properties props)
      (setq props (plist-put props prop name)))))

;;;; Faces

(defface lispm-finder-label
  '((t :inherit lispm-ui-list))
  "Face of an item's name in the Finder window."
  :group 'lispm-finder)

(defface lispm-finder-label-selected
  '((t :inherit lispm-ui-list :inverse-video t))
  "Face of a selected item's name in the Finder window."
  :group 'lispm-finder)

(defface lispm-finder-toolbar
  '((t :inherit lispm-ui-list))
  "Face of the Finder header strip on a text terminal."
  :group 'lispm-finder)

(defun lispm-finder--sync-faces ()
  "Update Finder faces from `lispm-ui-current-palette'."
  (when lispm-ui-current-palette
    (let ((content (lispm-ui-color :content))
          (text (lispm-ui-color :text))
          (header (lispm-ui-color :header)))
      (set-face-attribute 'lispm-finder-label nil
                          :inherit 'lispm-ui-list :background 'unspecified :foreground text)
      (set-face-attribute 'lispm-finder-label-selected nil
                          :inherit 'lispm-ui-list :background text :foreground content
                          :inverse-video nil)
      (set-face-attribute 'lispm-finder-toolbar nil
                          :inherit 'lispm-ui-list :background header :foreground text))))

(add-hook 'lispm-ui-palette-changed-hook #'lispm-finder--sync-faces 60)
(lispm-finder--sync-faces)

;;;; Header strips, buttons, and press tracking

(defconst lispm-finder-header-strip-height 22
  "Height in pixels of a Finder window's header strip.")

(defconst lispm-finder-header-button-height 17
  "Height in pixels of a bevel button in a Finder header strip.")

(defun lispm-finder--tty-p (&optional frame)
  "Return non-nil when FRAME (by default the selected frame) is a text terminal."
  (if lispm-finder-tty-p-function
      (funcall lispm-finder-tty-p-function (or frame (selected-frame)))
    (not (display-graphic-p (or frame (selected-frame))))))

(defun lispm-finder--fit (string width)
  "Return STRING shortened with an ellipsis to fit WIDTH pixels."
  (lispm-ui-text-fit string width))

(defun lispm-finder--draw-header-ground (svg width palette)
  "Draw on SVG the ground of a header strip WIDTH pixels wide, in PALETTE."
  (let ((h lispm-finder-header-strip-height)
        (grey (lambda (level) (lispm-ui-grey palette level))))
    (lispm-ui-svg-fill svg 0 0 width h (funcall grey #xDD))
    (lispm-ui-svg-row svg 0 (1- width) 0 (funcall grey #xFF))
    (lispm-ui-svg-row svg 0 (1- width) (- h 2) (funcall grey #xAA))
    (lispm-ui-svg-row svg 0 (1- width) (1- h) (plist-get palette :outline))))

(defun lispm-finder-draw-header-strip (palette width &optional label left)
  "Return an SVG of a window's header strip WIDTH pixels wide, in PALETTE.
LABEL is centered in the small system font; LEFT is the width of the
opening drawn before it."
  (let* ((h lispm-finder-header-strip-height)
         (left (or left 0))
         (w (max 1 width))
         (svg (svg-create w h)))
    (lispm-finder--draw-header-ground svg w palette)
    (when label
      (let ((x (- (/ (- (+ left w) (lispm-ui-text-width label 'lispm-ui-small)) 2)
                  left)))
        (lispm-ui-svg-content
         svg label (if (> left 0) (max 8 x) x)
         1 (- h 3) (plist-get palette :text) 'lispm-ui-small)))
    svg))

(defun lispm-finder-header-strip (label &optional left right-width)
  "Return the header strip of the window being drawn, showing LABEL.
LEFT is the pixel width of buttons before the strip; RIGHT-WIDTH is the
pixel width of buttons after the strip."
  (let* ((left (or left 0))
         (right-width (or right-width 0))
         (width (max 1 (- (window-pixel-width) (window-right-divider-width)
                          (window-scroll-bar-width) left right-width))))
    (if (lispm-finder--tty-p)
        (let* ((cols (max 1 (- (window-body-width) left right-width
                               (if (window-at-side-p nil 'right) 0 1))))
               (label (truncate-string-to-width (or label "") (max 0 (- cols 2)) nil nil "…"))
               (before (max 1 (- (/ (- (+ left cols) (string-width label)) 2) left))))
          (propertize (truncate-string-to-width
                       (concat (make-string before ?\s) label (make-string cols ?\s)) cols)
                      'face 'lispm-finder-toolbar))
      (propertize " " 'display
                  (lispm-ui-image
                   (list 'finder-header-strip label width left)
                   (lambda () (lispm-finder-draw-header-strip lispm-ui-current-palette
                                                              width label left)))))))

(defun lispm-finder-header-button-width (label &optional trailing-pad)
  "Return the width of a header strip button reading LABEL.
When TRAILING-PAD is non-nil, include trailing margin."
  (if (lispm-finder--tty-p)
      (string-width (lispm-finder-header-button label nil trailing-pad))
    (+ 6 (lispm-ui-text-width label 'lispm-ui-small)
       (* 2 lispm-ui-bevel-padding)
       (if trailing-pad 6 0))))

(defun lispm-finder-draw-header-button (palette label &optional state trailing-pad)
  "Return an SVG of a header strip bevel button reading LABEL in PALETTE."
  (let* ((pad-right (if trailing-pad 6 0))
         (width (+ 6 (lispm-ui-text-width label 'lispm-ui-small)
                   (* 2 lispm-ui-bevel-padding)
                   pad-right))
         (svg (svg-create width lispm-finder-header-strip-height)))
    (lispm-finder--draw-header-ground svg width palette)
    (lispm-ui-svg-bevel-button svg 6 2 palette (- width 6 pad-right)
                               lispm-finder-header-button-height
                               :state state :label label
                               :face 'lispm-ui-small)
    svg))

(defun lispm-finder-header-button (label &optional state trailing-pad)
  "Return a header strip bevel button reading LABEL in STATE."
  (if (lispm-finder--tty-p)
      (concat (propertize " " 'face 'lispm-finder-toolbar)
              (lispm-ui-text-button label state)
              (if trailing-pad (propertize " " 'face 'lispm-finder-toolbar) ""))
    (propertize " " 'display
                (lispm-ui-image
                 (list 'finder-header-button (lispm-ui-content-key label) state trailing-pad)
                 (lambda () (lispm-finder-draw-header-button lispm-ui-current-palette
                                                             label state trailing-pad))))))

(defun lispm-finder--press-target (position)
  "Return the press target at POSITION."
  (let ((lispm-ui-target-properties
         (append '(lispm-ui-target lispm-finder-target)
                 lispm-finder-extra-target-properties
                 lispm-ui-target-properties)))
    (lispm-ui-press-target position)))

(defun lispm-finder--same-target-p (position)
  "Return a predicate true of positions on the same target as POSITION."
  (let ((lispm-ui-target-properties
         (append '(lispm-ui-target lispm-finder-target)
                 lispm-finder-extra-target-properties
                 lispm-ui-target-properties)))
    (lispm-ui-same-target-p position)))

(defalias 'lispm-finder--pressed-p #'lispm-ui-pressed-p)

(defun lispm-finder--press (position pressed-p command)
  "Hold a button down from POSITION until mouse release; run COMMAND if on target."
  (let ((wrapped (lambda ()
                   (let ((w (posn-window position)))
                     (when (window-live-p w)
                       (with-selected-window w
                         (funcall command)))))))
    (if lispm-finder-press-function
        (funcall lispm-finder-press-function position pressed-p wrapped)
      (let ((lispm-ui-target-properties
             (append '(lispm-ui-target lispm-finder-target)
                     lispm-finder-extra-target-properties
                     lispm-ui-target-properties)))
        (lispm-ui-press position pressed-p wrapped (lispm-finder--press-target position))))))

;;;; Icons & Grid Drawing

(defun lispm-finder--darken (svg)
  "Return SVG with every 6-digit hex color halved, darkening a selected icon."
  (replace-regexp-in-string
   "#\\([[:xdigit:]]\\{6\\}\\)"
   (lambda (match)
     (apply #'format "#%02X%02X%02X"
            (mapcar (lambda (i) (/ (string-to-number (substring match i (+ i 2)) 16) 2))
                    '(1 3 5))))
   svg t t))

(defvar lispm-finder--icons (make-hash-table :test #'equal)
  "Large icon images by (FILE . SELECTED).")

(defun lispm-finder--icon (name selected)
  "Return the large icon NAME as a string showing it, darkened when SELECTED."
  (let ((file (or (and name (lispm-ui-icon-file name))
                  (lispm-ui-icon-file "document-32"))))
    (if (not (and (display-images-p) (image-type-available-p 'svg)
                  file (file-readable-p file)))
        (propertize " " 'display `(space :width (,lispm-finder-icon-size)))
      (when (> (hash-table-count lispm-finder--icons) 256)
        (clrhash lispm-finder--icons))
      (propertize
       " " 'display
       (or (gethash (cons file selected) lispm-finder--icons)
           (puthash (cons file selected)
                    (let ((svg (with-temp-buffer
                                 (insert-file-contents file)
                                 (buffer-string))))
                      (create-image (if selected (lispm-finder--darken svg) svg) 'svg t
                                    :height lispm-finder-icon-size :ascent 'center))
                    lispm-finder--icons))))))

(defun lispm-finder--header ()
  "Return the window's header: the enclosing-folder button and item count strip."
  (if lispm-finder--choosing
      (lispm-finder--choose-header)
    (let ((label (lispm-finder--up-label)))
      (concat (lispm-finder--up-button label)
              (lispm-finder-header-strip (lispm-finder--info)
                                         (lispm-finder-header-button-width label))))))

(defconst lispm-finder--up-target '(header-line lispm-finder-up)
  "Target identifier for the button going to the enclosing folder.")

(defun lispm-finder--press-up (event)
  "Hold the button EVENT pressed, going to the enclosing folder if released on it."
  (interactive "e")
  (let ((start (event-start event)))
    (lispm-finder--press start (lispm-finder--same-target-p start)
                         #'lispm-finder-up)))

(defvar-keymap lispm-finder--up-button-map
  :doc "Keymap of the button going to the enclosing folder."
  "<header-line> <down-mouse-1>" #'lispm-finder--press-up
  "<header-line> <mouse-1>" #'ignore
  "<header-line> <drag-mouse-1>" #'ignore)

(defun lispm-finder--up-label ()
  "Return what the button going to the enclosing folder reads."
  (if-let* ((parent (lispm-finder--parent lispm-finder--directory)))
      (concat "▲ " (lispm-finder--fit
                    (propertize (lispm-finder--title parent) 'face 'lispm-ui-small)
                    160))
    "▲"))

(defun lispm-finder--up-button (label)
  "Return the button reading LABEL that goes to the enclosing folder."
  (if (not (lispm-finder--parent lispm-finder--directory))
      (lispm-finder-header-button label 'disabled)
    (apply #'propertize
           (lispm-finder-header-button
            label (and (lispm-finder--pressed-p lispm-finder--up-target) 'pressed))
           'help-echo (lispm-finder-tr "Go to the enclosing folder (Delete)")
           'keymap lispm-finder--up-button-map
           (lispm-finder--target-plist lispm-finder--up-target))))

(defun lispm-finder--info ()
  "Return what the window's header says: its items, and the room left on disk."
  (let ((count (length lispm-finder--items))
        (available (and (stringp lispm-finder--directory)
                        (nth 2 (ignore-errors (file-system-info lispm-finder--directory))))))
    (if available
        (lispm-finder-tr-n "%d item, %s available" "%d items, %s available" count
                           count (file-size-human-readable available 'si " " "B"))
      (lispm-finder-tr-n "%d item" "%d items" count))))

(defun lispm-finder--title (directory)
  "Return the window's title for DIRECTORY."
  (if-let* ((folder (lispm-finder--folder directory)))
      (funcall (or lispm-finder-translate-function #'lispm-finder-tr)
               (plist-get folder :title))
    (let ((name (file-name-nondirectory (directory-file-name directory))))
      (if (string-empty-p name) directory name))))

(defun lispm-finder--folder-icon (directory)
  "Return the small 16x16 icon string for DIRECTORY."
  (if-let* ((folder (lispm-finder--folder directory)))
      (lispm-ui-icon (plist-get folder :icon))
    (lispm-ui-icon (if (and (stringp directory)
                            (string-empty-p (file-name-nondirectory (directory-file-name directory))))
                       "disk"
                     "folder"))))

(defun lispm-finder--cell-left (column)
  "Return the pixel left of the window's cell COLUMN."
  (+ 6 (* column lispm-finder-cell-width)))

(defconst lispm-finder-text-cell-width 16
  "Width in cells of an item's cell in the Finder window on a text terminal.")

(defconst lispm-finder-text-icon-size 16
  "Size in pixels the Finder window's icons are drawn at on a text terminal.")

(defun lispm-finder--sanitize-label (name)
  "Return NAME with control characters replaced by spaces for single-line display."
  (replace-regexp-in-string "[[:cntrl:]]+" " " name))

(defun lispm-finder--text-items (window props)
  "Insert the folder's items for WINDOW on a text terminal.
PROPS returns the text properties of an item, by name.  Return the columns."
  (let* ((cell lispm-finder-text-cell-width)
         (size lispm-finder-text-icon-size)
         (picture-size (lispm-ui-text-size size))
         (lines (cdr picture-size))
         (images (and lispm-finder-tty-images-p-function
                      (funcall lispm-finder-tty-images-p-function)))
         (icons nil)
         (background (face-background 'lispm-ui-list nil t))
         (columns (max 1 (/ (- (window-body-width window) 3) cell)))
         (width (* columns cell)))
    (insert "\n")
    (seq-do-indexed
     (lambda (row block)
       (let ((pieces nil))
         (seq-do-indexed
          (lambda (item column)
            (let* ((selected (equal (car item) lispm-finder--selected))
                   (icon (lispm-finder--icon-name lispm-finder--directory item))
                   (left (* column cell))
                   (picture (if images
                                (progn (push (list (car item) icon selected (+ 1 (* block (+ lines 2)))
                                                   (+ 1 left (/ (- cell (car picture-size)) 2))
                                                   (car picture-size) lines)
                                             icons)
                                       (make-list lines (make-string (car picture-size) ?\s)))
                              (or (lispm-ui-icon-picture icon size selected background)
                                  (list (substring-no-properties (lispm-ui-icon icon))))))
                   (clean (lispm-finder--sanitize-label (car item)))
                   (name (concat " " (truncate-string-to-width clean (- cell 4) nil nil "…") " ")))
              (seq-do-indexed (lambda (line index)
                                (push (list (+ index (- lines (length picture)))
                                            (+ left (/ (- cell (string-width line)) 2))
                                            (apply #'propertize line (funcall props (car item))))
                                      pieces))
                              picture)
              (push (list lines (+ left (max 0 (/ (- cell (string-width name)) 2)))
                          (apply #'propertize name
                                 'face (if selected 'lispm-finder-label-selected 'lispm-finder-label)
                                 (funcall props (car item))))
                    pieces)))
          row)
         (dolist (line (lispm-ui-text-compose width (+ lines 2) pieces))
           (insert " " line "\n"))))
     (seq-partition lispm-finder--items columns))
    (when lispm-finder-tty-images-show-function
      (funcall lispm-finder-tty-images-show-function window icons))
    columns))

(defun lispm-finder--target-buffers ()
  "Return active Finder buffers to render."
  (if (or (derived-mode-p 'lispm-finder-mode)
          (local-variable-p 'lispm-finder--directory))
      (list (current-buffer))
    (delq nil (list (get-buffer lispm-finder-choose-buffer-name)
                    (get-buffer lispm-finder-buffer-name)))))

(defun lispm-finder--render (&rest _)
  "Draw the folder's items as rows of large icons over their names."
  (dolist (buffer (lispm-finder--target-buffers))
    (when-let* ((window (get-buffer-window buffer t)))
      (with-current-buffer buffer
        (lispm-finder--read-again)
        (let* ((inhibit-read-only t)
               (cell lispm-finder-cell-width)
               (columns (max 1 (/ (- (window-body-width window t) 12) cell)))
               (props #'lispm-finder--item-plist))
          (setq lispm-finder--columns columns)
          (erase-buffer)
          (if (lispm-finder--tty-p (window-frame window))
              (setq lispm-finder--columns
                    (funcall (or lispm-finder-text-items-function
                                 #'lispm-finder--text-items)
                             window props))
            (insert (propertize " " 'display '(space :height (12))) "\n"))
          (dolist (row (unless (lispm-finder--tty-p (window-frame window))
                         (seq-partition lispm-finder--items columns)))
            (seq-do-indexed
             (lambda (item column)
               (insert (propertize " " 'display
                                   `(space :align-to (,(+ (lispm-finder--cell-left column)
                                                          (/ (- cell lispm-finder-icon-size) 2)))))
                       (apply #'propertize
                              (lispm-finder--icon (lispm-finder--icon-name lispm-finder--directory item)
                                                  (equal (car item) lispm-finder--selected))
                              (funcall props (car item)))))
             row)
            (insert "\n")
            (seq-do-indexed
             (lambda (item column)
               (let ((label (lispm-finder--fit
                             (propertize (concat " " (lispm-finder--sanitize-label (car item)) " ")
                                         'face (if (equal (car item) lispm-finder--selected)
                                                   'lispm-finder-label-selected
                                                 'lispm-finder-label))
                             (- cell 6))))
                 (insert (propertize " " 'display
                                     `(space :align-to (,(+ (lispm-finder--cell-left column)
                                                            (/ (- cell (string-pixel-width label))
                                                               2)))))
                         (apply #'propertize label (funcall props (car item))))))
             row)
            (insert "\n" (propertize " " 'display '(space :height (14))) "\n"))
          (run-hooks 'lispm-finder-rendered-hook)
          (let ((at (or (and lispm-finder--selected
                             (lispm-finder--item-position lispm-finder--selected))
                        (point-min))))
            (goto-char at)
            (set-window-point window at))
          (force-mode-line-update))))))

(provide 'lispm-finder-render)
;;; lispm-finder-render.el ends here
