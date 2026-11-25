;;; lispm-finder-choose.el --- Modal file and folder chooser for LispM Finder  -*- lexical-binding: t; -*-

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

;; Modal file and directory chooser (`lispm-finder-choose-file',
;; `lispm-finder-choose-directory', `lispm-finder-choose-mode') for
;; `lispm-finder'.  Separated from `lispm-finder.el' so each module stays
;; bounded and domain-focused.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'lispm-ui)
(require 'lispm-finder-i18n)
(require 'lispm-finder-render)

(defvar lispm-finder-choose-buffer-name)
(defvar lispm-finder--directory)
(defvar lispm-finder--items)
(defvar lispm-finder--selected)
(defvar lispm-finder--choosing)
(defvar lispm-finder--predicate)
(defvar lispm-finder--choose-title)
(defvar lispm-finder--choose-result)
(defvar lispm-finder--back)
(defvar lispm-finder--forward)
(defvar lispm-finder-modal-show-function)
(require 'lispm-finder)

(declare-function lispm-finder--buffer "lispm-finder" ())
(declare-function lispm-finder--tty-p "lispm-finder-render" (&optional frame))
(declare-function lispm-finder--fit "lispm-finder-render" (string width))
(declare-function lispm-finder--title "lispm-finder-render" (directory))
(declare-function lispm-finder--info "lispm-finder-render" ())
(declare-function lispm-finder--up-label "lispm-finder-render" ())
(declare-function lispm-finder--up-button "lispm-finder-render" (label))
(declare-function lispm-finder-header-button-width "lispm-finder-render" (label &optional trailing-pad))
(declare-function lispm-finder-header-button "lispm-finder-render" (label &optional state trailing-pad))
(declare-function lispm-finder-header-strip "lispm-finder-render" (label &optional left right-width))
(declare-function lispm-finder--pressed-p "lispm-finder-render" (target &optional window))
(declare-function lispm-finder--target-plist "lispm-finder-render" (target))
(declare-function lispm-finder--press "lispm-finder-render" (position pressed-p command))
(declare-function lispm-finder--same-target-p "lispm-finder-render" (position))
(declare-function lispm-finder--path-items "lispm-finder" (window))
(declare-function lispm-finder--popup-xy "lispm-finder" (target))
(declare-function lispm-finder--go "lispm-finder" (directory &optional record select))
(declare-function lispm-finder--child-path "lispm-finder" (name directory))
(declare-function lispm-finder-real-p "lispm-finder" ())
(declare-function lispm-finder-new-folder "lispm-finder" (name))
(declare-function lispm-finder--folder-icon "lispm-finder-render" (directory))
(declare-function lispm-finder--call-render "lispm-finder" (&rest args))
(declare-function lispm-finder-mode "lispm-finder" ())

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
  (with-current-buffer (lispm-finder--buffer)
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
                   'display (lispm-ui-look-image
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
                                 'display (lispm-ui-look-image
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
  (with-current-buffer (lispm-finder--buffer)
    (when-let* ((item (lispm-finder--selected-item))
                ((cdr item)))
      (lispm-finder--go (lispm-finder--child-path (car item) lispm-finder--directory) t))))

(defun lispm-finder-choose-open-or-confirm (&optional double-click)
  "Open a selected folder on DOUBLE-CLICK (or in file mode), else confirm choice."
  (interactive)
  (with-current-buffer (lispm-finder--buffer)
    (let ((item (lispm-finder--selected-item)))
      (cond
       ((and item (cdr item) (or double-click (not (lispm-finder--directory-only-p))))
        (lispm-finder--go (lispm-finder--child-path (car item) lispm-finder--directory) t))
       (t
        (lispm-finder-choose-confirm))))))

(defun lispm-finder-choose-confirm ()
  "Confirm the current selection in the modal Finder chooser."
  (interactive)
  (with-current-buffer (lispm-finder--buffer)
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
  (with-current-buffer (lispm-finder--buffer)
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
  "^" #'lispm-finder-up
  "/" #'lispm-finder-choose-path-menu
  "~" #'lispm-finder-choose-go-home
  "q" #'lispm-finder-choose-cancel
  "M-w" #'lispm-finder-choose-cancel
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
                (lispm-ui-look-image
                 (list 'finder-modal-title width (lispm-ui-content-key fitted))
                 (lambda ()
                   (lispm-ui-draw-title-part
                    lispm-ui-current-palette width
                    :active t :lead t :trail t
                    :ridges (plist-get layout :ridges)
                    :label fitted :label-x (plist-get layout :label-x)))))))

(defun lispm-finder--modal-show-default (buffer title width height)
  "Show modal chooser BUFFER in a centered child frame of WIDTH by HEIGHT."
  (let* ((prev-frame (selected-frame))
         (parent prev-frame)
         (prev-window (selected-window)))
    (while (frame-parent parent)
      (setq parent (frame-parent parent)))
    (let* ((pw (frame-pixel-width parent))
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
              (cond ((frame-live-p prev-frame)
                     (select-frame-set-input-focus prev-frame))
                    ((frame-live-p parent)
                     (select-frame-set-input-focus parent)))
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
        (funcall cleanup))))))

(defun lispm-finder--init-choose-buffer (directory title predicate initial-select)
  "Create and initialize the modal chooser buffer at DIRECTORY."
  (let* ((existing (get-buffer lispm-finder-choose-buffer-name))
         (buffer (if (and existing
                          (buffer-local-value 'lispm-finder--choosing existing)
                          (get-buffer-window existing t))
                     (generate-new-buffer lispm-finder-choose-buffer-name)
                   (get-buffer-create lispm-finder-choose-buffer-name)))
         (ok nil))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (lispm-finder-choose-mode)
            (setq lispm-finder--predicate predicate
                  lispm-finder--choose-title title
                  lispm-finder--choose-result nil
                  lispm-finder--choose-depth (recursion-depth)
                  lispm-finder--back nil
                  lispm-finder--forward nil)
            (lispm-finder--go directory nil initial-select))
          (setq ok t)
          buffer)
      (unless ok
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

;;;###autoload
(cl-defun lispm-finder-choose-file (&key title file directory predicate preview)
  "Choose a file or folder using a modal Finder icon-grid window.
TITLE titles the window (by default \"Choose a File\", or \"Choose a Folder\"
when PREDICATE is `file-directory-p').  Starts in FILE's directory with FILE
selected, else in DIRECTORY (by default `default-directory').  When PREVIEW
is non-nil on a graphic display, delegates to `lispm-ui-file-dialog'; when
not on a graphic display with a palette, falls back to the minibuffer."
  (require 'lispm-finder)
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
    (cond
     ((and preview (lispm-ui-popup-graphic-p) (fboundp 'lispm-ui-file-dialog))
      (lispm-ui-file-dialog :title title :file file :directory directory
                            :predicate predicate :preview preview))
     ((not (lispm-ui-popup-graphic-p))
      (lispm-ui-choose-file-in-minibuffer title file directory predicate))
     (t
      (let ((buffer nil)
            (cleanup nil)
            (closing nil)
            (result nil))
        (unwind-protect
            (progn
              (setq buffer (lispm-finder--init-choose-buffer
                            directory title predicate initial-select))
              (with-current-buffer buffer
                (add-hook 'kill-buffer-hook
                          (lambda ()
                            (unless closing
                              (setq lispm-finder--choose-result 'cancel)
                              (when (and lispm-finder--choose-depth
                                         (= (recursion-depth) (1+ lispm-finder--choose-depth)))
                                (exit-recursive-edit))))
                          nil t))
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
               (unless dir-only
                 (lispm-ui-choose-remember result))
               result)))))))

(defalias 'lispm-finder-choose-header #'lispm-finder--choose-header)
(defalias 'lispm-finder-choose-footer #'lispm-finder--choose-footer)
(defalias 'lispm-finder-modal-show-default #'lispm-finder--modal-show-default)

;;;###autoload
(cl-defun lispm-finder-choose-directory (&key title directory)
  "Choose a directory using a modal Finder icon-grid window."
  (require 'lispm-finder)
  (lispm-finder-choose-file :title (or title (lispm-finder-tr "Choose a Folder"))
                            :directory directory
                            :predicate #'file-directory-p))

(provide 'lispm-finder-choose)
;;; lispm-finder-choose.el ends here
