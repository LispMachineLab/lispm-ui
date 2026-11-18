;;; lispm-finder.el --- Graphical Finder file manager and file chooser  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
;; URL: https://github.com/LispMachineLab/lispm-ui
;; Version: 0.1.1
;; Package-Requires: ((emacs "30.1"))
;; Keywords: files, convenience
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

;; A graphical file manager and file/folder chooser inspired by Mac OS 9's
;; Finder: a folder's items laid out as large 32x32 icons over their names,
;; with a classic header strip counting the items and available disk space,
;; an enclosing-folder button at the top left, path navigation, keyboard
;; type-select, and context menus.
;;
;; It serves both as an interactive file manager (`lispm-finder',
;; `lispm-finder-open-directory', `lispm-finder-home') and as the modal
;; file and directory chooser (`lispm-finder-choose-file',
;; `lispm-finder-choose-directory') for `lispm-ui-choose-file-function'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'lispm-ui)
(require 'lispm-finder-i18n)

(declare-function image-size "image.c" (spec &optional pixels frame))

(defgroup lispm-finder nil
  "Graphical Finder file manager and file chooser."
  :group 'files)

(defconst lispm-finder-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "The directory LispM Finder is loaded from.")

(defconst lispm-finder-icon-directory
  (expand-file-name "icons/" lispm-finder-directory)
  "Directory of LispM Finder's SVG icons.")

(defun lispm-finder-icon-file (name)
  "Return the SVG file for icon NAME in `lispm-finder-icon-directory', or nil."
  (let ((file (expand-file-name (concat name ".svg") lispm-finder-icon-directory)))
    (and (file-readable-p file) file)))

(add-hook 'lispm-ui-icon-functions #'lispm-finder-icon-file 90)

(defconst lispm-finder-buffer-name " *Finder*"
  "Name of the Finder window's buffer.")

(defconst lispm-finder-choose-buffer-name " *Finder Choose*"
  "Name of the modal Finder chooser buffer.")

(defconst lispm-finder-cell-width 100
  "Width in pixels of an item's cell in the window: its icon and its name.")

(defconst lispm-finder-icon-size 32
  "Side in pixels of an item's icon.")

(defvar lispm-finder--had-focus nil
  "Non-nil while the Finder window was the last focused window.")

(defvar-local lispm-finder--directory nil
  "The folder the window shows.")

(defvar-local lispm-finder--back nil
  "Folders shown before this one, the last first.")

(defvar-local lispm-finder--forward nil
  "Folders gone back from, the nearest first.")

(defvar-local lispm-finder--items nil
  "The folder's items in the order shown: (NAME . FOLDER-P) for a
directory on disk, (NAME ICON APP) for a folder of
`lispm-finder-folders'.")

(defvar-local lispm-finder--selected nil
  "Name of the selected item, or nil.")

(defvar-local lispm-finder--selected-app nil
  "Key of the application the selected item is, or nil.
A folder of `lispm-finder-folders' names its items in the language
LispM speaks, so an item is found again by the application it is,
see `lispm-finder--read-again'.")

(defvar-local lispm-finder--columns 1
  "Items a row of the window holds, as last drawn.")

(defvar-local lispm-finder--typed nil
  "(TEXT . TIME): the letters typed to select an item, and when.")

(defvar-local lispm-finder--choosing nil
  "Non-nil in a Finder buffer acting as a modal file or folder chooser.")

(defvar-local lispm-finder--predicate nil
  "Predicate filtering files shown in chooser mode, or nil for all.")

(defvar-local lispm-finder--choose-title nil
  "Title of the modal chooser window.")

(defvar-local lispm-finder--choose-result nil
  "Result chosen in modal chooser mode: a path string or `cancel'.")

;;;; Host integration hooks and callbacks

(defvar lispm-finder-show-function nil
  "Function called with (BUFFER DIRECTORY) to present the Finder window.
When nil, `pop-to-buffer' displays BUFFER.")

(defvar lispm-finder-close-function nil
  "Function called with no arguments to close the Finder window.
When nil, the window showing `lispm-finder-buffer-name' is deleted or buried.")

(defvar lispm-finder-shown-function nil
  "Function called with no arguments returning non-nil when Finder is open.
When nil, checks whether `lispm-finder-buffer-name' has a live window.")

(defvar lispm-finder-open-file-function nil
  "Function called with FILE to open a regular file from Finder.
When nil, `find-file' is used.")

(defvar lispm-finder-modal-show-function nil
  "Function called with (BUFFER TITLE WIDTH HEIGHT) to show the modal chooser.
Must return a cleanup function of no arguments.  When nil,
`lispm-finder--modal-show-default' is used.")

(defvar lispm-finder-drag-to-trash-function nil
  "Function called with (ICON NAME) to drag an item to the Trash.
Returns non-nil when dropped on the Trash.")

(defvar lispm-finder-trash-changed-function nil
  "Function called with no arguments after moving a file to the system Trash.")

(defvar lispm-finder-alias-possible-function nil
  "Function called with APP key returning non-nil if an alias can be made.")

(defvar lispm-finder-make-alias-function nil
  "Function called with APP key to create an alias.")

(defvar lispm-finder-tty-p-function nil
  "Optional function `(FRAME)` returning non-nil when FRAME is a text terminal.")

(defvar lispm-finder-tty-images-p-function nil
  "Function returning non-nil when the text terminal supports inline images.")

(defvar lispm-finder-tty-images-show-function nil
  "Function called with (WINDOW ICONS) to place inline images on a text terminal.")

(defvar lispm-finder-text-items-function nil
  "Optional function `(WINDOW PROPS)` rendering text-terminal items in WINDOW.")

(defvar lispm-finder-render-function nil
  "Optional function rendering the Finder buffer.")

(defvar lispm-finder-press-function nil
  "Optional function `(START PRESSED-P COMMAND)` tracking header button presses.")

(defvar lispm-finder-translate-function nil
  "Optional function `(MESSAGE)` translating virtual folder titles and labels.")

(defvar lispm-finder-frame-xy-function nil
  "Function called with EVENT or buffer position returning (X . Y) in frame pixels.")

(defvar lispm-finder-menu-augment-function nil
  "Function called with (ITEMS WINDOW) to augment context menu items.")

(defvar lispm-finder-rendered-hook nil
  "Hook run in the Finder buffer at the end of `lispm-finder--render'.")

(defvar lispm-finder-extra-item-properties nil
  "Additional text property symbols set to an item's name on rendered items.")

(defvar lispm-finder-extra-target-properties nil
  "Additional text property symbols set on header and footer press targets.")

(defvar lispm-finder-item-map)

(defvar lispm-ui-target-properties '(lispm-ui-target))
(cl-pushnew 'lispm-finder-target lispm-ui-target-properties)

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

(eval-and-compile
  (unless (fboundp 'lispm-ui-press-target)
    (defun lispm-ui-press-target (position)
      "Return the press target at mouse POSITION, or nil."
      (when-let* ((string (posn-string position))
                  ((stringp (car string)))
                  (index (cdr string))
                  ((and (integerp index) (<= 0 index) (< index (length (car string))))))
        (seq-some (lambda (prop)
                    (get-text-property index prop (car string)))
                  lispm-ui-target-properties))))

  (unless (fboundp 'lispm-ui-same-target-p)
    (defun lispm-ui-same-target-p (position)
      "Return a predicate true of mouse positions on the same target as POSITION."
      (let ((window (posn-window position))
            (area (posn-area position))
            (target (lispm-ui-press-target position)))
        (lambda (release)
          (and target
               (eq (posn-window release) window)
               (eq (posn-area release) area)
               (equal (lispm-ui-press-target release) target))))))

  (unless (fboundp 'lispm-ui-pressed-p)
    (defun lispm-ui-pressed-p (target &optional window)
      "Return non-nil while TARGET is held down in WINDOW."
      (and target
           (equal (window-parameter (or window (selected-window)) 'lispm-ui-pressed) target))))

  (unless (fboundp 'lispm-ui-press)
    (defun lispm-ui-press (position pressed-p command &optional target)
      "Hold a control at POSITION pressed until the mouse button rises."
      (let ((window (posn-window position))
            (target (or target (lispm-ui-press-target position)))
            (holding t))
        (when (window-live-p window)
          (when (unwind-protect
                    (lispm-ui-modal-hold
                     (lambda (event)
                       (let ((over (and (window-live-p window)
                                        (funcall pressed-p (event-end event)))))
                         (when (lispm-ui-modal-mouse-up-p event)
                           (setq holding nil))
                         over))
                     (lambda (in)
                       (when (and holding (window-live-p window))
                         (set-window-parameter window 'lispm-ui-pressed (and in target))
                         (with-current-buffer (window-buffer window)
                           (force-mode-line-update))
                         (redisplay t))))
                  (when (window-live-p window)
                    (set-window-parameter window 'lispm-ui-pressed nil)
                    (with-current-buffer (window-buffer window)
                      (force-mode-line-update))))
            (when (window-live-p window)
              (let ((lispm-ui-in-gui-action t))
                (with-selected-window window
                  (funcall command))))))))))

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

;;;; Items

(defvar lispm-finder-folders nil
  "Virtual folders shown as Finder shows a directory on disk: (KEY . PROPERTIES).
KEY, a symbol, stands for the folder where a directory's name would.
:title names the folder, in English, translated where shown, see
`lispm-finder--title'; :icon gives its small icon as `lispm-ui-icon-file'
takes it, and :items is a function returning its items, each
\(NAME ICON ACTION TRASH APP): NAME as shown, ICON its large icon,
ACTION a function opening it, TRASH, when given, a function throwing it
away, returning non-nil once it is gone, and APP, when given, the key of
the application it is; :trash names the menu item doing so, in English,
by default \"Move to Trash\".  Nothing is made or renamed in such a folder.")

(defun lispm-finder--folder (directory)
  "Return the properties of DIRECTORY when it is a virtual folder."
  (and directory (symbolp directory) (alist-get directory lispm-finder-folders)))

(defun lispm-finder--folder-items (directory)
  "Return the items of DIRECTORY, one of `lispm-finder-folders'."
  (funcall (plist-get (lispm-finder--folder directory) :items)))

(defun lispm-finder--child-path (name directory)
  "Return the path of child entry NAME inside DIRECTORY without `~' expansion."
  (expand-file-name (concat "./" name) directory))

(defun lispm-finder--read (directory &optional predicate)
  "Return DIRECTORY's items by name, hidden ones left out.
An item of a directory on disk is (NAME . FOLDER-P); an item of a folder
of `lispm-finder-folders' is (NAME ICON APP).  When PREDICATE is non-nil
\(or `lispm-finder--predicate' is set in the current buffer), non-folder
files must satisfy it."
  (let ((pred (or predicate lispm-finder--predicate)))
    (sort (if (lispm-finder--folder directory)
              (mapcar (lambda (item) (list (car item) (nth 1 item) (nth 4 item)))
                      (lispm-finder--folder-items directory))
            (delq nil
                  (mapcar (lambda (name)
                            (let* ((full (lispm-finder--child-path name directory))
                                   (dir-p (file-directory-p full)))
                              (when (or dir-p (null pred) (funcall pred full))
                                (cons name dir-p))))
                          (ignore-errors (directory-files directory nil "\\`[^.]" t)))))
          (lambda (a b) (string-collate-lessp (car a) (car b) nil t)))))

(defun lispm-finder--icon-name (directory item)
  "Return the name of the large icon of ITEM of DIRECTORY."
  (if (lispm-finder--folder directory)
      (or (nth 1 item) "document-32")
    (lispm-finder--file-icon-name directory item)))

(defun lispm-finder--file-icon-name (directory item)
  "Return the name of the large icon of ITEM of DIRECTORY, a directory."
  (let ((path (lispm-finder--child-path (car item) directory)))
    (if (cdr item)
        (if (file-exists-p (lispm-finder--child-path ".git" path)) "folder-git-32" "folder-32")
      (let* ((name (concat (lispm-ui-file-icon-name path) "-32"))
             (file (lispm-ui-icon-file name)))
        (if (and file (file-exists-p file)) name "document-32")))))

(defun lispm-finder--read-again ()
  "Read the items of the folder shown again when it is a virtual folder."
  (when-let* ((directory lispm-finder--directory)
              ((lispm-finder--folder directory)))
    (setq lispm-finder--items (lispm-finder--read directory))
    (let ((item (or (and lispm-finder--selected-app
                         (seq-find (lambda (other)
                                     (equal (nth 2 other) lispm-finder--selected-app))
                                   lispm-finder--items))
                    (assoc lispm-finder--selected lispm-finder--items))))
      (setq lispm-finder--selected (car-safe item)
            lispm-finder--selected-app (nth 2 item)))))

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

;;;; Drawing

(defvar-keymap lispm-finder-item-map
  :doc "Keymap of an item: a press selects it, a double click opens it."
  "<down-mouse-1>" #'lispm-finder-mouse-select
  "<mouse-1>" #'ignore
  "<double-down-mouse-1>" #'ignore
  "<double-mouse-1>" #'lispm-finder-mouse-open
  "<drag-mouse-1>" #'ignore
  "<down-mouse-3>" #'lispm-finder-context-menu
  "<mouse-3>" #'ignore)

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

;;;; Going places

(defun lispm-finder--buffer ()
  "Return the current Finder buffer if inside one, else the main Finder buffer."
  (if (or (derived-mode-p 'lispm-finder-mode)
          (local-variable-p 'lispm-finder--directory))
      (current-buffer)
    (if-let* ((choose-buf (get-buffer lispm-finder-choose-buffer-name))
              ((get-buffer-window choose-buf t)))
        choose-buf
      (or (get-buffer lispm-finder-buffer-name)
          (with-current-buffer (get-buffer-create lispm-finder-buffer-name)
            (lispm-finder-mode)
            (current-buffer))))))

(defmacro lispm-finder--in (&rest body)
  "Run BODY in the active Finder buffer."
  (declare (indent 0))
  `(with-current-buffer (lispm-finder--buffer) ,@body))

(defun lispm-finder-real-p ()
  "Return non-nil when the Finder window shows a directory on disk."
  (lispm-finder--in (and lispm-finder--directory (stringp lispm-finder--directory))))

(defun lispm-finder--parent (directory)
  "Return the folder enclosing DIRECTORY, or nil at the top."
  (when (stringp directory)
    (let ((parent (file-name-directory (directory-file-name directory))))
      (unless (or (null parent) (equal parent directory)) parent))))

(defun lispm-finder--path-items (_window)
  "Return the menu of the shown folder's path: the folder and each parent."
  (let ((folders nil)
        (directory (lispm-finder--in lispm-finder--directory)))
    (while directory
      (push directory folders)
      (setq directory (lispm-finder--parent directory)))
    (mapcar (lambda (folder)
              (list :label (lispm-finder--title folder)
                    :action (lambda () (lispm-finder--go folder t))))
            (nreverse folders))))

(defun lispm-finder--call-render (&rest args)
  "Call `lispm-finder-render-function' or `lispm-finder--render' with ARGS."
  (apply (or lispm-finder-render-function #'lispm-finder--render) args))

(defun lispm-finder--go (directory &optional record select)
  "Show DIRECTORY, with SELECT selected; RECORD keeps the folder left in history."
  (lispm-finder--in
    (let* ((target-dir (if (lispm-finder--folder directory)
                           directory
                         (file-name-as-directory (expand-file-name directory))))
           (items (lispm-finder--read target-dir))
           (valid-select (and select (car (assoc select items)))))
      (when (and record lispm-finder--directory)
        (push lispm-finder--directory lispm-finder--back)
        (setq lispm-finder--forward nil))
      (setq lispm-finder--directory target-dir
            lispm-finder--items items
            lispm-finder--selected valid-select
            lispm-finder--selected-app (lispm-finder--app valid-select))
      (when (stringp lispm-finder--directory)
        (setq default-directory lispm-finder--directory))
      (lispm-finder--call-render))))

(defun lispm-finder-back ()
  "Show the folder shown before this one."
  (interactive)
  (lispm-finder--in
    (when-let* ((previous (pop lispm-finder--back)))
      (push lispm-finder--directory lispm-finder--forward)
      (lispm-finder--go previous))))

(defun lispm-finder-forward ()
  "Show again the folder gone back from."
  (interactive)
  (lispm-finder--in
    (when-let* ((next (pop lispm-finder--forward)))
      (push lispm-finder--directory lispm-finder--back)
      (lispm-finder--go next))))

(defun lispm-finder-up ()
  "Show the enclosing folder, the folder left selected in it."
  (interactive)
  (lispm-finder--in
    (when-let* ((parent (lispm-finder--parent lispm-finder--directory)))
      (lispm-finder--go parent t
                        (unless (and lispm-finder--choosing
                                     (lispm-finder--directory-only-p))
                          (lispm-finder--title lispm-finder--directory))))))

(defun lispm-finder-refresh ()
  "Read the folder again."
  (interactive)
  (lispm-finder--in
    (let ((app lispm-finder--selected-app)
          (sel lispm-finder--selected))
      (lispm-finder--go lispm-finder--directory nil sel)
      (when (and app (not lispm-finder--selected-app))
        (setq lispm-finder--selected-app app)
        (lispm-finder--read-again)
        (lispm-finder--call-render)))))

(defun lispm-finder-folder-changed (key)
  "Read the virtual folder KEY of `lispm-finder-folders' again if shown."
  (when-let* ((buffer (get-buffer lispm-finder-buffer-name))
              ((eq (buffer-local-value 'lispm-finder--directory buffer) key)))
    (with-current-buffer buffer
      (lispm-finder--call-render))))

(defun lispm-finder--edit (file)
  "Open FILE in the editor."
  (if lispm-finder-open-file-function
      (funcall lispm-finder-open-file-function file)
    (find-file file)))

(defun lispm-finder-open ()
  "Open the selected item: a folder in the window, a file in the editor."
  (interactive)
  (if (lispm-finder--in lispm-finder--choosing)
      (lispm-finder-choose-open-or-confirm)
    (let ((action nil))
      (lispm-finder--in
        (when-let* ((name lispm-finder--selected))
          (if (lispm-finder--folder lispm-finder--directory)
              (setq action (nth 2 (assoc name (lispm-finder--folder-items lispm-finder--directory))))
            (let ((path (lispm-finder--child-path name lispm-finder--directory)))
              (if (file-directory-p path)
                  (lispm-finder--go path t)
                (lispm-finder--edit path))))))
      (when action
        (funcall action)))))

(defun lispm-finder--real-only ()
  "Refuse to change the folder shown unless it is a directory on disk."
  (unless (lispm-finder-real-p)
    (user-error (lispm-finder-tr "Nothing can be made or renamed in “%s”")
                (lispm-finder--in (lispm-finder--title lispm-finder--directory)))))

(defun lispm-finder--validate-item-name (name)
  "Validate that NAME is a single file or folder name within the current directory."
  (let ((trimmed (and (stringp name) (string-trim name))))
    (when (or (null trimmed)
              (string-empty-p trimmed)
              (member trimmed '("." ".."))
              (string-match-p "[/\\]" trimmed))
      (user-error (lispm-finder-tr "Nothing can be made or renamed in “%s”") (or name "")))
    trimmed))

(defvar lispm-ui-intercepting nil)
(defvar lispm-ui--intercepting nil)
(declare-function lispm-ui-form-prompt "lispm-ui-form" (prompt &rest args))
(declare-function lispm-ui-choose-file-in-minibuffer "lispm-ui-ask" (title file directory predicate))
(declare-function lispm-ui-choose-remember "lispm-ui-file-dialog" (file))

(defun lispm-finder-directory (&optional buffer)
  "Return the directory or virtual folder key shown in BUFFER."
  (with-current-buffer (or buffer (lispm-finder--buffer))
    lispm-finder--directory))

(defun lispm-finder--read-string (prompt &optional initial)
  "Read a string with PROMPT and INITIAL input, using a modal form in GUI Finder."
  (if (and (not noninteractive)
           (fboundp 'lispm-ui-form-prompt)
           (lispm-ui-popup-available-p)
           (or lispm-finder--choosing (lispm-ui-gui-action-p)))
      (lispm-ui-form-prompt prompt :initial initial)
    (let ((lispm-ui-intercepting nil)
          (lispm-ui--intercepting nil))
      (read-string prompt initial))))

(defun lispm-finder-new-folder (name)
  "Make a folder called NAME in the folder shown, and select it."
  (interactive
   (let ((buf (lispm-finder--buffer)))
     (with-current-buffer buf
       (lispm-finder--real-only)
       (list (lispm-finder--read-string
              (lispm-finder-tr "Name of the new folder: ")
              (lispm-finder-tr "untitled folder"))))))
  (lispm-finder--real-only)
  (let ((clean (lispm-finder--validate-item-name name)))
    (lispm-finder--in
      (let ((path (lispm-finder--child-path clean lispm-finder--directory)))
        (when (file-exists-p path)
          (user-error (lispm-finder-tr "There is already an item called “%s” here") clean))
        (make-directory path)
        (lispm-finder--go lispm-finder--directory nil clean)))))

(defun lispm-finder--trash-action (name)
  "Return the function throwing the item NAME of the folder shown away, or nil."
  (lispm-finder--in
    (cond ((null name) nil)
          ((stringp lispm-finder--directory)
           (let ((file (lispm-finder--child-path name lispm-finder--directory)))
             (lambda ()
               (move-file-to-trash file)
               (when lispm-finder-trash-changed-function
                 (funcall lispm-finder-trash-changed-function))
               (message (lispm-finder-tr "“%s” was moved to the Trash") name)
               t)))
          (t (nth 3 (assoc name (lispm-finder--folder-items lispm-finder--directory)))))))

(defun lispm-finder--app (name)
  "Return the key of the application the item NAME of the folder shown is, or nil."
  (lispm-finder--in
    (and name (lispm-finder--folder lispm-finder--directory)
         (nth 2 (assoc name lispm-finder--items)))))

(defun lispm-finder-alias-p ()
  "Return non-nil when an alias of the item selected can be made on the desktop."
  (when-let* ((app (lispm-finder--app (lispm-finder--in lispm-finder--selected))))
    (and lispm-finder-alias-possible-function
         (funcall lispm-finder-alias-possible-function app))))

(defun lispm-finder-make-alias ()
  "Make an alias of the application selected on the desktop."
  (interactive)
  (let ((name (or (lispm-finder--in lispm-finder--selected)
                  (user-error (lispm-finder-tr "Select an item first")))))
    (let ((app (or (lispm-finder--app name)
                   (user-error (lispm-finder-tr "“%s” cannot have an alias on the desktop") name))))
      (if lispm-finder-make-alias-function
          (funcall lispm-finder-make-alias-function app)
        (user-error (lispm-finder-tr "“%s” cannot have an alias on the desktop") name)))))

(defun lispm-finder-trash-label ()
  "Return what the menu item throwing the selected item away reads."
  (if-let* ((label (plist-get (lispm-finder--folder (lispm-finder--in lispm-finder--directory))
                              :trash)))
      (funcall (or lispm-finder-translate-function #'lispm-finder-tr) label)
    (lispm-finder-tr "Move to Trash")))

(defun lispm-finder-trash-p ()
  "Return non-nil when the item selected can be thrown away."
  (and (lispm-finder--trash-action (lispm-finder--in lispm-finder--selected)) t))

(defun lispm-finder--throw-away (name action)
  "Throw the item NAME away with ACTION; select the item after it."
  (pcase-let ((`(,directory . ,next)
               (lispm-finder--in
                 (let* ((names (mapcar #'car lispm-finder--items))
                        (index (or (seq-position names name) 0)))
                   (cons lispm-finder--directory
                         (or (nth (1+ index) names) (and (> index 0) (nth (1- index) names))))))))
    (condition-case err
        (when (funcall action)
          (lispm-finder--go directory nil next))
      (error
       (lispm-ui-alert 'stop (lispm-finder-tr "“%s” could not be thrown away." name)
                       `((,(lispm-finder-tr "OK") ok default))
                       (error-message-string err))))))

(defun lispm-finder-move-to-trash ()
  "Throw the selected item away."
  (interactive)
  (lispm-finder--in
    (when-let* ((name lispm-finder--selected))
      (lispm-finder--throw-away
       name (or (lispm-finder--trash-action name)
                (user-error (lispm-finder-tr "“%s” cannot be thrown away") name))))))

(defun lispm-finder-rename (name)
  "Rename the selected item to NAME."
  (interactive
   (let ((buf (lispm-finder--buffer)))
     (with-current-buffer buf
       (lispm-finder--real-only)
       (list (lispm-finder--read-string
              (lispm-finder-tr "Rename to: ")
              (lispm-finder--in lispm-finder--selected))))))
  (lispm-finder--real-only)
  (let ((clean (lispm-finder--validate-item-name name)))
    (lispm-finder--in
      (when lispm-finder--selected
        (rename-file (lispm-finder--child-path lispm-finder--selected lispm-finder--directory)
                     (lispm-finder--child-path clean lispm-finder--directory))
        (lispm-finder--go lispm-finder--directory nil clean)))))

;;;; Selecting

(defun lispm-finder--select (name)
  "Select the item NAME, or none when NAME is nil, and redraw."
  (lispm-finder--in
    (unless (equal name lispm-finder--selected)
      (setq lispm-finder--selected name
            lispm-finder--selected-app (lispm-finder--app name))
      (lispm-finder--call-render))))

(defun lispm-finder--step (step)
  "Select the item STEP places from the selected one."
  (lispm-finder--in
    (let* ((names (mapcar #'car lispm-finder--items))
           (index (seq-position names lispm-finder--selected)))
      (when names
        (lispm-finder--select
         (nth (max 0 (min (1- (length names)) (if index (+ index step) 0))) names))))))

(defun lispm-finder-select-right () "Select the next item." (interactive) (lispm-finder--step 1))
(defun lispm-finder-select-left () "Select the item before." (interactive) (lispm-finder--step -1))
(defun lispm-finder-select-down ()
  "Select the item below."
  (interactive)
  (lispm-finder--step (lispm-finder--in lispm-finder--columns)))
(defun lispm-finder-select-up ()
  "Select the item above."
  (interactive)
  (lispm-finder--step (- (lispm-finder--in lispm-finder--columns))))

(defun lispm-finder-type-select ()
  "Select the first item whose name begins with the letters just typed."
  (interactive)
  (lispm-finder--in
    (let* ((now (float-time))
           (text (concat (if (and lispm-finder--typed (< (- now (cdr lispm-finder--typed)) 1.0))
                             (car lispm-finder--typed)
                           "")
                         (string last-command-event))))
      (setq lispm-finder--typed (cons text now))
      (when-let* ((item (seq-find (lambda (item) (string-prefix-p text (car item) t))
                                  lispm-finder--items)))
        (lispm-finder--select (car item))))))

(defun lispm-finder--item-at (event)
  "Return the name of the item mouse EVENT is over, or nil."
  (let ((position (event-start event)))
    (when-let* ((point (posn-point position))
                (buf (window-buffer (posn-window position))))
      (or (get-text-property point 'lispm-finder-item buf)
          (seq-some (lambda (prop) (get-text-property point prop buf))
                    lispm-finder-extra-item-properties)))))

(defun lispm-finder-mouse-select (event)
  "Select the item pressed by EVENT."
  (interactive "e")
  (select-window (posn-window (event-start event)))
  (let ((name (lispm-finder--item-at event)))
    (lispm-finder--select name)
    (when (and name
               (not lispm-finder--choosing)
               (eq (car-safe event) 'down-mouse-1))
      (lispm-finder--drag name))))

(defun lispm-finder--drag (name)
  "Drag the item NAME out of the window while the mouse button is held."
  (when-let* ((lispm-finder-drag-to-trash-function)
              (action (lispm-finder--trash-action name))
              (icon (lispm-finder--in
                      (lispm-finder--icon-name lispm-finder--directory
                                               (assoc name lispm-finder--items))))
              ((funcall lispm-finder-drag-to-trash-function icon name)))
    (lispm-finder--throw-away name action)))

(defun lispm-finder-mouse-open (event)
  "Open the item double-clicked by EVENT."
  (interactive "e")
  (lispm-finder-mouse-select event)
  (if lispm-finder--choosing
      (lispm-finder-choose-open-or-confirm t)
    (lispm-finder-open)))

(defun lispm-finder-deselect (&optional event)
  "Select nothing: a click between the items."
  (interactive (list last-nonmenu-event))
  (when-let* (((mouse-event-p event))
              (win (posn-window (event-start event)))
              ((window-live-p win)))
    (select-window win))
  (lispm-finder--select nil))

(defun lispm-finder--popup-xy (target)
  "Return (X . Y) in frame pixels for TARGET (an event or buffer position)."
  (if lispm-finder-frame-xy-function
      (funcall lispm-finder-frame-xy-function target)
    (let* ((posn (if (integerp target) (posn-at-point target) (event-start target)))
           (win (or (and posn (posn-window posn)) (selected-window)))
           (edges (window-inside-pixel-edges win))
           (xy (or (and posn (posn-x-y posn)) '(0 . 0))))
      (cons (+ (car edges) (car xy))
            (+ (cadr edges) (cdr xy))))))

(defun lispm-finder-context-menu (event)
  "Pop up the menu of the item right-clicked by EVENT, or of the folder."
  (interactive "e")
  (lispm-finder-mouse-select event)
  (lispm-finder--menu (lispm-finder--popup-xy event)))

(defun lispm-finder--item-position (name &optional label)
  "Return where the item NAME is drawn in the window's buffer: its icon, or
with LABEL its name under it; nil where it is not."
  (let ((position (point-min))
        (found nil))
    (while (and position (< position (point-max)) (or label (not found)))
      (when (equal (or (get-text-property position 'lispm-finder-item)
                       (seq-some (lambda (prop) (get-text-property position prop))
                                 lispm-finder-extra-item-properties))
                   name)
        (setq found position))
      (let ((next (next-single-property-change position 'lispm-finder-item)))
        (dolist (prop lispm-finder-extra-item-properties)
          (when-let* ((p (next-single-property-change position prop)))
            (setq next (if next (min next p) p))))
        (setq position next)))
    found))

(defun lispm-finder--keyboard-menu ()
  "Pop up the menu of the item selected, under its name, or of the folder."
  (lispm-finder--in
    (let* ((label (and lispm-finder--selected
                       (lispm-finder--item-position lispm-finder--selected t)))
           (xy (lispm-finder--popup-xy (or label (window-start)))))
      (lispm-finder--menu
       (if-let* ((posn (and label (posn-at-point label))))
           (cons (car xy) (+ (cdr xy) (cdr (posn-object-width-height posn))))
         xy)))))

(defun lispm-finder--menu (xy)
  "Pop up the menu of the item selected, or of the folder, at XY of the frame."
  (let* ((selected (lispm-finder--in lispm-finder--selected))
         (real (lispm-finder-real-p))
         (file (and selected real t))
         (items
          `((:label ,(lispm-finder-tr "Open") :action lispm-finder-open :enabled ,(and selected t))
            separator
            (:label ,(lispm-finder-tr "New Folder…") :action lispm-finder-new-folder :enabled ,real)
            (:label ,(lispm-finder-tr "Rename…") :action lispm-finder-rename :enabled ,file)
            ,@(when lispm-finder-make-alias-function
                `((:label ,(lispm-finder-tr "Make Alias on Desktop")
                          :action lispm-finder-make-alias :key "M-m"
                          :enabled ,(lispm-finder-alias-p))))
            (:label ,(lispm-finder-trash-label) :action lispm-finder-move-to-trash :key "M-DEL"
                    :enabled ,(lispm-finder-trash-p))
            (:label ,(lispm-finder-tr "Copy Path") :enabled ,file
                    :action ,(lambda ()
                               (lispm-finder--in
                                 (kill-new (expand-file-name lispm-finder--selected
                                                             lispm-finder--directory))))))))
    (lispm-ui-menu
     (if lispm-finder-menu-augment-function
         (funcall lispm-finder-menu-augment-function items (selected-window))
       items)
     (car xy) (cdr xy))))

(defun lispm-finder--graphic-binding (command)
  "Return a conditional key binding running COMMAND only on a graphic display."
  `(menu-item "" ,command :filter ,(lambda (cmd) (and (display-graphic-p) cmd))))

(defvar-keymap lispm-finder-mode-map
  :doc "Keymap of the Finder window."
  :parent (make-sparse-keymap)
  "<down-mouse-1>" #'lispm-finder-deselect
  "<mouse-1>" #'ignore
  "<double-mouse-1>" #'ignore
  "<drag-mouse-1>" #'ignore
  "<down-mouse-3>" #'lispm-finder-context-menu
  "<mouse-3>" #'ignore
  "<right>" #'lispm-finder-select-right
  "<left>" #'lispm-finder-select-left
  "<down>" #'lispm-finder-select-down
  "<up>" #'lispm-finder-select-up
  "C-f" #'lispm-finder-select-right
  "C-b" #'lispm-finder-select-left
  "C-n" #'lispm-finder-select-down
  "C-p" #'lispm-finder-select-up
  "TAB" #'lispm-finder-select-right
  "<backtab>" #'lispm-finder-select-left
  "RET" #'lispm-finder-open
  "M-o" #'lispm-finder-open
  "M-<down>" #'lispm-finder-open
  "M-<up>" #'lispm-finder-up
  "DEL" #'lispm-finder-up
  "M-[" (lispm-finder--graphic-binding #'lispm-finder-back)
  "M-]" (lispm-finder--graphic-binding #'lispm-finder-forward)
  "M-N" #'lispm-finder-new-folder
  "M-m" #'lispm-finder-make-alias
  "M-DEL" #'lispm-finder-move-to-trash
  "M-<backspace>" #'lispm-finder-move-to-trash
  "M-r" #'lispm-finder-refresh
  "M-w" #'lispm-finder-close
  "<escape>" #'lispm-finder-deselect
  "C-g" #'lispm-finder-deselect
  "<remap> <self-insert-command>" #'lispm-finder-type-select)

(defvar evil-emacs-state-cursor)
(defvar evil-normal-state-cursor)

(define-derived-mode lispm-finder-mode special-mode "Finder"
  "Major mode of the Finder window: a folder's items as large icons.
\\{lispm-finder-mode-map}"
  (setq-local header-line-format '(:eval (lispm-finder--header))
              cursor-type nil
              cursor-in-non-selected-windows nil
              evil-emacs-state-cursor '(nil)
              evil-normal-state-cursor '(nil)
              truncate-lines t
              left-fringe-width 0
              right-fringe-width 0
              show-trailing-whitespace nil)
  (when (fboundp 'display-line-numbers-mode)
    (display-line-numbers-mode -1))
  (face-remap-add-relative 'default 'lispm-ui-list)
  (face-remap-add-relative 'header-line '(:box nil :underline nil :overline nil))
  (add-hook 'window-size-change-functions #'lispm-finder--call-render nil t))

;;;; Finder window commands

;;;###autoload
(defun lispm-finder-open-directory (directory)
  "Show DIRECTORY in the Finder window.
DIRECTORY is a directory's name, or a key of `lispm-finder-folders'."
  (interactive (list (read-directory-name (lispm-finder-tr "Folder: ") nil default-directory 'lambda)))
  (let ((buffer (lispm-finder--buffer)))
    (if lispm-finder-show-function
        (funcall lispm-finder-show-function buffer directory)
      (with-current-buffer buffer
        (lispm-finder--go directory (and lispm-finder--directory t)))
      (pop-to-buffer buffer)
      (setq lispm-finder--had-focus t)
      (with-current-buffer buffer
        (lispm-finder--call-render)))))

;;;###autoload
(defalias 'lispm-finder #'lispm-finder-open-directory)

;;;###autoload
(defun lispm-finder-home ()
  "Show the home folder in the Finder window."
  (interactive)
  (lispm-finder-open-directory "~"))

(defun lispm-finder-close ()
  "Close the Finder window."
  (interactive)
  (if (lispm-finder--in lispm-finder--choosing)
      (lispm-finder-choose-cancel)
    (if lispm-finder-close-function
        (funcall lispm-finder-close-function)
      (when-let* ((buffer (get-buffer lispm-finder-buffer-name)))
        (when-let* ((win (get-buffer-window buffer t)))
          (if (> (length (window-list (window-frame win))) 1)
              (delete-window win)
            (quit-window nil win)))
        (kill-buffer buffer)))))

(defun lispm-finder-shown-p ()
  "Return non-nil while the Finder window is open."
  (if lispm-finder-shown-function
      (funcall lispm-finder-shown-function)
    (and (get-buffer lispm-finder-buffer-name)
         (get-buffer-window (get-buffer lispm-finder-buffer-name) t)
         t)))

;;;; Modal File and Folder Chooser

(defconst lispm-finder-choose-default-width 560
  "Default width in pixels of the modal Finder chooser window.")

(defconst lispm-finder-choose-default-height 380
  "Default height in pixels of the modal Finder chooser window.")

(defconst lispm-finder--choose-footer-height 44
  "Height in pixels of the modal Finder chooser's bottom action bar.")

(defconst lispm-finder--path-target '(header-line lispm-finder-choose-path-menu)
  "Target identifier for the folder path/places popup button in chooser header.")

(defconst lispm-finder--new-folder-target '(header-line lispm-finder-new-folder)
  "Target identifier for the New Folder button in chooser header.")

(defconst lispm-finder--footer-new-folder-target '(mode-line lispm-finder-new-folder)
  "Target identifier for the New Folder button in chooser footer.")

(defconst lispm-finder--footer-open-target '(mode-line lispm-finder-choose-open)
  "Target identifier for the Open button in chooser footer.")

(defconst lispm-finder--footer-cancel-target '(mode-line lispm-finder-choose-cancel)
  "Target identifier for the Cancel button in chooser footer.")

(defconst lispm-finder--footer-choose-target '(mode-line lispm-finder-choose-confirm)
  "Target identifier for the Choose button in chooser footer.")

(defun lispm-finder--directory-only-p ()
  "Return non-nil when the chooser selects a folder rather than a file."
  (eq lispm-finder--predicate #'file-directory-p))

(defun lispm-finder--selected-item ()
  "Return the currently selected (NAME . FOLDER-P) entry, or nil."
  (and lispm-finder--selected
       (assoc lispm-finder--selected lispm-finder--items)))

(defun lispm-finder--selected-folder-p ()
  "Return non-nil when a subfolder is currently selected."
  (cdr (lispm-finder--selected-item)))

(defun lispm-finder--choose-target-path ()
  "Return the path that `Choose' will currently pick, or nil if none."
  (let ((item (lispm-finder--selected-item)))
    (cond
     ((lispm-finder--directory-only-p)
      (let ((path (if (and item (cdr item))
                      (file-name-as-directory (lispm-finder--child-path (car item) lispm-finder--directory))
                    lispm-finder--directory)))
        (and (stringp path) (file-directory-p path) path)))
     ((and item (not (cdr item)))
      (let ((path (lispm-finder--child-path (car item) lispm-finder--directory)))
        (and (file-exists-p path)
             (not (file-directory-p path))
             (or (null lispm-finder--predicate)
                 (funcall lispm-finder--predicate path))
             path)))
     (t nil))))

(defun lispm-finder--places ()
  "Return common places as (LABEL . DIRECTORY) pairs."
  (seq-filter (lambda (place) (file-directory-p (cdr place)))
              (list (cons (lispm-finder-tr "Home") "~/")
                    (cons (lispm-finder-tr "Desktop") "~/Desktop/")
                    (cons (lispm-finder-tr "Documents") "~/Documents/")
                    (cons (lispm-finder-tr "Downloads") "~/Downloads/")
                    (cons (lispm-finder-tr "Pictures") "~/Pictures/"))))

(defun lispm-finder-choose-path-menu (&optional event)
  "Pop up the menu of enclosing folders and standard places."
  (interactive (list last-nonmenu-event))
  (lispm-finder--in
    (let* ((path-items (lispm-finder--path-items (selected-window)))
           (places (mapcar (pcase-lambda (`(,label . ,dir))
                             (list :label label
                                   :action (lambda () (lispm-finder--go dir t))))
                           (lispm-finder--places)))
           (items (append path-items
                          (when places (cons 'separator places))))
           (xy (if (and event (listp event))
                   (lispm-finder--popup-xy event)
                 (cons 80 24))))
      (lispm-ui-menu items (car xy) (cdr xy)))))

(defun lispm-finder--press-path (event)
  "Open the path/places menu when the header path button is pressed."
  (interactive "e")
  (let ((start (event-start event)))
    (lispm-finder--press start (lispm-finder--same-target-p start)
                         (lambda () (lispm-finder-choose-path-menu event)))))

(defvar-keymap lispm-finder--path-button-map
  :doc "Keymap of the path/places button in the chooser header."
  "<header-line> <down-mouse-1>" #'lispm-finder--press-path
  "<header-line> <mouse-1>" #'ignore
  "<header-line> <drag-mouse-1>" #'ignore)

(defun lispm-finder--press-new-folder (event)
  "Run `lispm-finder-new-folder' when the New Folder header button is pressed."
  (interactive "e")
  (let ((start (event-start event)))
    (lispm-finder--press start (lispm-finder--same-target-p start)
                         (lambda () (call-interactively #'lispm-finder-new-folder)))))

(defvar-keymap lispm-finder--new-folder-button-map
  :doc "Keymap of the New Folder button in the chooser header."
  "<header-line> <down-mouse-1>" #'lispm-finder--press-new-folder
  "<header-line> <mouse-1>" #'ignore
  "<header-line> <drag-mouse-1>" #'ignore)

(defun lispm-finder--choose-header ()
  "Return the header line of the modal Finder chooser."
  (let* ((up-label (lispm-finder--up-label))
         (up-btn (lispm-finder--up-button up-label))
         (up-w (lispm-finder-header-button-width up-label))
         (cur-title (lispm-finder--fit
                     (propertize (concat (lispm-finder--title lispm-finder--directory) " ▾")
                                 'face 'lispm-ui-small)
                     150))
         (path-btn (apply #'propertize
                          (lispm-finder-header-button
                           cur-title
                           (and (lispm-finder--pressed-p lispm-finder--path-target) 'pressed))
                          'help-echo (lispm-finder-tr "Go to enclosing folder or standard place (/)")
                          'keymap lispm-finder--path-button-map
                          (lispm-finder--target-plist lispm-finder--path-target)))
         (path-w (lispm-finder-header-button-width cur-title))
         (new-label (propertize (lispm-finder-tr "New Folder…") 'face 'lispm-ui-small))
         (new-btn (apply #'propertize
                         (lispm-finder-header-button
                          new-label
                          (and (lispm-finder--pressed-p lispm-finder--new-folder-target) 'pressed)
                          t)
                         'help-echo (lispm-finder-tr "Create a new folder (M-N)")
                         'keymap lispm-finder--new-folder-button-map
                         (lispm-finder--target-plist lispm-finder--new-folder-target)))
         (new-w (lispm-finder-header-button-width new-label t))
         (left-w (+ up-w path-w)))
    (concat up-btn
            path-btn
            (lispm-finder-header-strip (lispm-finder--info) left-w new-w)
            new-btn)))

(defun lispm-finder--draw-footer-ground (svg width height palette)
  "Draw the classic footer ground on SVG of WIDTH by HEIGHT in PALETTE."
  (let ((grey (lambda (level) (lispm-ui-grey palette level))))
    (lispm-ui-svg-fill svg 0 0 width height (funcall grey #xDD))
    (lispm-ui-svg-row svg 0 (1- width) 0 (plist-get palette :outline))
    (lispm-ui-svg-row svg 0 (1- width) 1 (funcall grey #xFF))))

(defun lispm-finder--draw-footer-left (palette width path-label)
  "Draw the left stretch of the chooser footer showing PATH-LABEL."
  (let* ((h lispm-finder--choose-footer-height)
         (w (max 1 width))
         (svg (svg-create w h)))
    (lispm-finder--draw-footer-ground svg w h palette)
    (when (and path-label (not (string-empty-p path-label)))
      (lispm-ui-svg-content
       svg path-label 12 (/ (- h 16) 2) 18
       (plist-get palette :text) 'lispm-ui-small))
    svg))

(cl-defun lispm-finder--draw-footer-button (palette width label &key default state trailing)
  "Draw a footer slice of WIDTH containing a push button reading LABEL."
  (let* ((h lispm-finder--choose-footer-height)
         (w (max 1 width))
         (svg (svg-create w h))
         (btn-w (max 68 (lispm-ui-push-button-width label)))
         (btn-y 8)
         (btn-x (if default 4 2)))
    (lispm-finder--draw-footer-ground svg w h palette)
    (lispm-ui-svg-push-button svg btn-x btn-y palette label btn-w
                              :default default :state state)
    (ignore trailing)
    svg))

(cl-defun lispm-finder--footer-button-slice (label target action &key default enabled trailing)
  "Return a propertized mode-line button slice for LABEL."
  (let* ((btn-w (max 68 (lispm-ui-push-button-width label)))
         (slice-w (+ btn-w (if default 8 4) (if trailing 16 4)))
         (state (cond ((not enabled) 'disabled)
                      ((lispm-finder--pressed-p target) 'pressed)
                      (t nil)))
         (map (let ((m (make-sparse-keymap)))
                (when enabled
                  (keymap-set m "<mode-line> <down-mouse-1>"
                              (lambda (event)
                                (interactive "e")
                                (let ((start (event-start event)))
                                  (lispm-finder--press start (lispm-finder--same-target-p start)
                                                       action)))))
                (keymap-set m "<mode-line> <mouse-1>" #'ignore)
                (keymap-set m "<mode-line> <drag-mouse-1>" #'ignore)
                m)))
    (cons slice-w
          (if (lispm-finder--tty-p)
              (propertize (concat " " (lispm-ui-text-button label (or state (and default 'default))) " ")
                          'lispm-finder-target target
                          'keymap map)
            (apply #'propertize " "
                   'display (lispm-ui-image
                             (list 'finder-footer-btn (lispm-ui-content-key label)
                                   slice-w default state trailing)
                             (lambda ()
                               (lispm-finder--draw-footer-button
                                lispm-ui-current-palette slice-w label
                                :default default :state state :trailing trailing)))
                   'pointer (if enabled 'hand 'arrow)
                   'keymap map
                   (lispm-finder--target-plist target))))))

(defun lispm-finder--choose-footer ()
  "Return the mode-line footer of the modal Finder chooser."
  (let* ((dir-only (lispm-finder--directory-only-p))
         (item (lispm-finder--selected-item))
         (sel-folder (and item (cdr item)))
         (target-path (lispm-finder--choose-target-path))
         (new-folder-slice (when (lispm-finder-real-p)
                             (lispm-finder--footer-button-slice
                              (lispm-finder-tr "New Folder…")
                              lispm-finder--footer-new-folder-target
                              (lambda () (call-interactively #'lispm-finder-new-folder))
                              :enabled t)))
         (open-slice (when (and dir-only sel-folder)
                       (lispm-finder--footer-button-slice
                        (lispm-finder-tr "Open")
                        lispm-finder--footer-open-target
                        #'lispm-finder-choose-open-selected
                        :enabled t)))
         (cancel-slice (lispm-finder--footer-button-slice
                        (lispm-finder-tr "Cancel")
                        lispm-finder--footer-cancel-target
                        #'lispm-finder-choose-cancel
                        :enabled t))
         (choose-label (if (and (not dir-only) sel-folder)
                           (lispm-finder-tr "Open")
                         (lispm-finder-tr "Choose")))
         (choose-enabled (if dir-only t (and item t)))
         (choose-slice (lispm-finder--footer-button-slice
                        choose-label
                        lispm-finder--footer-choose-target
                        #'lispm-finder-choose-confirm
                        :default t
                        :enabled choose-enabled
                        :trailing t))
         (right-w (+ (or (car new-folder-slice) 0)
                     (or (car open-slice) 0)
                     (car cancel-slice)
                     (car choose-slice)))
         (total-w (- (window-pixel-width) (window-right-divider-width)
                     (window-scroll-bar-width)))
         (left-w (max 1 (- total-w right-w)))
         (shown-path (or target-path lispm-finder--directory ""))
         (abbrev (abbreviate-file-name shown-path))
         (icon (if (or dir-only sel-folder (null target-path))
                   (lispm-finder--folder-icon shown-path)
                 (lispm-ui-icon (lispm-ui-file-icon-name shown-path))))
         (path-text (lispm-finder--fit
                     (concat icon " "
                             (propertize abbrev 'face 'lispm-ui-small))
                     (max 40 (- left-w 20))))
         (left-str (if (lispm-finder--tty-p)
                       (propertize (truncate-string-to-width (concat " " abbrev)
                                                             (max 1 (- (window-body-width) 36))
                                                             nil nil "…")
                                   'face 'lispm-finder-toolbar)
                     (propertize " "
                                 'display (lispm-ui-image
                                           (list 'finder-footer-left left-w
                                                 (lispm-ui-content-key path-text))
                                           (lambda ()
                                             (lispm-finder--draw-footer-left
                                              lispm-ui-current-palette left-w path-text)))))))
    (concat left-str
            (cdr new-folder-slice)
            (cdr open-slice)
            (cdr cancel-slice)
            (cdr choose-slice))))

(defvar-local lispm-finder--choose-depth nil
  "Recursion depth when this modal chooser entered `recursive-edit'.")

(defun lispm-finder-choose-open-selected ()
  "Enter the selected subfolder in chooser mode."
  (interactive)
  (lispm-finder--in
    (when-let* ((item (lispm-finder--selected-item))
                ((cdr item)))
      (lispm-finder--go (lispm-finder--child-path (car item) lispm-finder--directory) t))))

(defun lispm-finder-choose-open-or-confirm (&optional double-click)
  "Open a selected folder on DOUBLE-CLICK (or in file mode), else confirm choice."
  (interactive)
  (lispm-finder--in
    (let ((item (lispm-finder--selected-item)))
      (cond
       ((and item (cdr item) (or double-click (not (lispm-finder--directory-only-p))))
        (lispm-finder--go (lispm-finder--child-path (car item) lispm-finder--directory) t))
       (t
        (lispm-finder-choose-confirm))))))

(defun lispm-finder-choose-confirm ()
  "Confirm the current selection in the modal Finder chooser."
  (interactive)
  (lispm-finder--in
    (when (or (null lispm-finder--choose-depth)
              (<= (recursion-depth) (1+ lispm-finder--choose-depth)))
      (let ((item (lispm-finder--selected-item)))
        (cond
         ((and (not (lispm-finder--directory-only-p)) item (cdr item))
          (lispm-finder--go (lispm-finder--child-path (car item) lispm-finder--directory) t))
         (t
          (when-let* ((target (lispm-finder--choose-target-path)))
            (setq lispm-finder--choose-result target)
            (when (and lispm-finder--choose-depth
                       (= (recursion-depth) (1+ lispm-finder--choose-depth)))
              (exit-recursive-edit)))))))))

(defun lispm-finder-choose-cancel ()
  "Cancel the modal Finder chooser."
  (interactive)
  (lispm-finder--in
    (when (or (null lispm-finder--choose-depth)
              (<= (recursion-depth) (1+ lispm-finder--choose-depth)))
      (setq lispm-finder--choose-result 'cancel)
      (when (and lispm-finder--choose-depth
                 (= (recursion-depth) (1+ lispm-finder--choose-depth)))
        (exit-recursive-edit)))))

(defun lispm-finder-choose-go-home ()
  "Navigate to the user's home directory in the modal chooser."
  (interactive)
  (lispm-finder--go "~/" t))

(defvar-keymap lispm-finder-choose-mode-map
  :doc "Keymap of the modal Finder chooser."
  :parent lispm-finder-mode-map
  "RET" #'lispm-finder-choose-confirm
  "C-c C-c" #'lispm-finder-choose-confirm
  "C-c C-k" #'lispm-finder-choose-cancel
  "M-o" #'lispm-finder-choose-open-selected
  "M-<down>" #'lispm-finder-choose-open-selected
  "/" #'lispm-finder-choose-path-menu
  "~" #'lispm-finder-choose-go-home
  "<escape>" #'lispm-finder-choose-cancel
  "C-g" #'lispm-finder-choose-cancel)

(define-derived-mode lispm-finder-choose-mode lispm-finder-mode "Finder-Choose"
  "Major mode for choosing a file or folder in a modal Finder window.
\\{lispm-finder-choose-mode-map}"
  (setq-local lispm-finder--choosing t
              header-line-format '(:eval (lispm-finder--choose-header))
              mode-line-format '(:eval (lispm-finder--choose-footer)))
  (face-remap-add-relative 'mode-line '(:box nil :underline nil :overline nil))
  (face-remap-add-relative 'mode-line-inactive '(:box nil :underline nil :overline nil)))

(defun lispm-finder--modal-title-bar (title)
  "Render a standalone classic modal title bar reading TITLE."
  (let* ((width (max 80 (- (window-pixel-width) (window-right-divider-width)
                           (window-scroll-bar-width))))
         (label (propertize title 'face 'lispm-ui-title))
         (layout (lispm-ui-title-layout
                  width (lispm-ui-text-width label 'lispm-ui-title)
                  '(nil nil nil)))
         (fitted (lispm-ui-text-fit label (plist-get layout :title))))
    (propertize " " 'display
                (lispm-ui-image
                 (list 'finder-modal-title width (lispm-ui-content-key fitted))
                 (lambda ()
                   (lispm-ui-draw-title-part
                    lispm-ui-current-palette width
                    :active t :lead t :trail t
                    :ridges (plist-get layout :ridges)
                    :label fitted :label-x (plist-get layout :label-x)))))))

(defun lispm-finder--modal-show-default (buffer title width height)
  "Show modal chooser BUFFER in a centered child frame of WIDTH by HEIGHT."
  (let* ((parent (selected-frame))
         (prev-window (selected-window))
         (pw (frame-pixel-width parent))
         (ph (frame-pixel-height parent))
         (left (max 0 (/ (- pw width) 2)))
         (top (max 0 (/ (- ph height) 3)))
         (after-make-frame-functions nil)
         (before-make-frame-hook nil)
         (frame nil)
         (ok nil)
         (cleanup
          (lambda ()
            (when (frame-live-p frame)
              (delete-frame frame t))
            (when (frame-live-p parent)
              (select-frame-set-input-focus parent))
            (when (window-live-p prev-window)
              (select-window prev-window)))))
    (unwind-protect
        (progn
          (setq frame (make-frame
                       `((parent-frame . ,parent)
                         (minibuffer . ,(minibuffer-window parent))
                         (name . ,title)
                         (undecorated . t)
                         (user-position . t)
                         (left . ,left)
                         (top . ,top)
                         (width . (text-pixels . ,width))
                         (height . (text-pixels . ,height))
                         (internal-border-width . 1)
                         (child-frame-border-width . 1)
                         (menu-bar-lines . 0)
                         (tool-bar-lines . 0)
                         (tab-bar-lines . 0)
                         (no-other-frame . t)
                         (skip-taskbar . t)
                         (desktop-dont-save . t)
                         (visibility . t))))
          (when-let* ((outline (lispm-ui-color :outline)))
            (set-face-attribute 'child-frame-border frame :background outline)
            (set-face-attribute 'internal-border frame :background outline))
          (let ((win (frame-root-window frame)))
            (set-window-buffer win buffer)
            (set-window-dedicated-p win t)
            (set-window-fringes win 0 0)
            (with-current-buffer buffer
              (setq-local tab-line-format `(:eval (lispm-finder--modal-title-bar ,title)))
              (face-remap-add-relative 'tab-line '(:box nil :underline nil :overline nil))))
          (raise-frame frame)
          (select-frame-set-input-focus frame)
          (select-window (frame-root-window frame))
          (setq ok t)
          cleanup)
      (unless ok
        (funcall cleanup)))))

;;;###autoload
(cl-defun lispm-finder-choose-file (&key title file directory predicate preview)
  "Choose a file or folder using a modal Finder icon-grid window.
TITLE titles the window (by default \"Choose a File\", or \"Choose a Folder\"
when PREDICATE is `file-directory-p').  Starts in FILE's directory with FILE
selected, else in DIRECTORY (by default `default-directory').  When not on a
graphic display with a palette, falls back to the minibuffer."
  (ignore preview)
  (let* ((dir-only (eq predicate #'file-directory-p))
         (title (or title (if dir-only
                              (lispm-finder-tr "Choose a Folder")
                            (lispm-finder-tr "Choose a File"))))
         (directory (file-name-as-directory
                     (expand-file-name
                      (or (and file (file-name-directory file))
                          directory
                          default-directory))))
         (initial-select (and file (not (string-empty-p (file-name-nondirectory file)))
                              (file-name-nondirectory file))))
    (if (not (lispm-ui-popup-graphic-p))
        (if (fboundp 'lispm-ui-choose-file-in-minibuffer)
            (lispm-ui-choose-file-in-minibuffer title file directory predicate)
          (lispm-ui--choose-file-in-minibuffer title file directory predicate))
      (let ((buffer nil)
            (cleanup nil)
            (closing nil)
            (result nil))
        (unwind-protect
            (progn
              (setq buffer (let ((existing (get-buffer lispm-finder-choose-buffer-name)))
                             (if (and existing
                                      (buffer-local-value 'lispm-finder--choosing existing)
                                      (get-buffer-window existing t))
                                 (generate-new-buffer lispm-finder-choose-buffer-name)
                               (get-buffer-create lispm-finder-choose-buffer-name))))
              (with-current-buffer buffer
                (lispm-finder-choose-mode)
                (setq lispm-finder--predicate predicate
                      lispm-finder--choose-title title
                      lispm-finder--choose-result nil
                      lispm-finder--choose-depth (recursion-depth)
                      lispm-finder--back nil
                      lispm-finder--forward nil)
                (add-hook 'kill-buffer-hook
                          (lambda ()
                            (unless closing
                              (setq lispm-finder--choose-result 'cancel)
                              (when (and lispm-finder--choose-depth
                                         (= (recursion-depth) (1+ lispm-finder--choose-depth)))
                                (exit-recursive-edit))))
                          nil t)
                (lispm-finder--go directory nil initial-select))
              (setq cleanup
                    (funcall (or lispm-finder-modal-show-function
                                 #'lispm-finder--modal-show-default)
                             buffer title
                             lispm-finder-choose-default-width
                             lispm-finder-choose-default-height))
              (with-current-buffer buffer
                (lispm-finder--call-render))
              (redisplay t)
              (let ((inhibit-quit nil))
                (condition-case nil
                    (recursive-edit)
                  (quit nil)))
              (when (buffer-live-p buffer)
                (setq result (buffer-local-value 'lispm-finder--choose-result buffer))))
          (setq closing t)
          (when (functionp cleanup)
            (ignore-errors (funcall cleanup)))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))
        (and (stringp result)
             (progn
               (when (not dir-only)
                 (cond ((fboundp 'lispm-ui-choose-remember)
                        (lispm-ui-choose-remember result))
                       ((fboundp 'lispm-ui-file-dialog--remember)
                        (lispm-ui-file-dialog--remember result))))
               result))))))

(defalias 'lispm-finder-path-items #'lispm-finder--path-items)
(defalias 'lispm-finder-keyboard-menu #'lispm-finder--keyboard-menu)
(defalias 'lispm-finder-choose-header #'lispm-finder--choose-header)
(defalias 'lispm-finder-choose-footer #'lispm-finder--choose-footer)
(defalias 'lispm-finder-modal-show-default #'lispm-finder--modal-show-default)

;;;###autoload
(cl-defun lispm-finder-choose-directory (&key title directory)
  "Choose a directory using a modal Finder icon-grid window."
  (lispm-finder-choose-file :title (or title (lispm-finder-tr "Choose a Folder"))
                            :directory directory
                            :predicate #'file-directory-p))

(setq lispm-ui-choose-file-function #'lispm-finder-choose-file)

(provide 'lispm-finder)
;;; lispm-finder.el ends here
