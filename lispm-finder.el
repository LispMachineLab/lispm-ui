;;; lispm-finder.el --- Graphical Finder file manager and file chooser  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
;; URL: https://github.com/LispMachineLab/lispm-ui
;; Version: 0.2.0
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
;; `lispm-finder-choose-directory', defined in `lispm-finder-choose.el')
;; for `lispm-ui-choose-file-function'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'lispm-ui)
(require 'lispm-finder-i18n)

(declare-function image-size "image.c" (spec &optional pixels frame))
(declare-function lispm-finder--choose-header "lispm-finder-choose" ())
(declare-function lispm-finder--directory-only-p "lispm-finder-choose" ())
(declare-function lispm-finder-choose-open-or-confirm "lispm-finder-choose" (&optional double-click))
(declare-function lispm-finder-choose-cancel "lispm-finder-choose" ())

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
  (when (or (stringp name) (symbolp name))
    (let ((file (expand-file-name (format "%s.svg" name) lispm-finder-icon-directory)))
      (and (file-readable-p file) file))))

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

(defvar-local lispm-finder--state-signal nil
  "Reactive `lispm-ui-signal' holding `(DIRECTORY ITEMS SELECTED)' for this buffer.")

(defvar-local lispm-finder--icon-names nil
  "Hash table caching resolved icon names by `(DIRECTORY . ITEM)'.")

(defvar-local lispm-finder--rendered-key nil
  "Signature of the last rendered state in this Finder buffer.")

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

(cl-pushnew 'lispm-finder-target lispm-ui-target-properties)

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
    (unless (hash-table-p lispm-finder--icon-names)
      (setq lispm-finder--icon-names (make-hash-table :test #'equal)))
    (when (> (hash-table-count lispm-finder--icon-names) 1024)
      (clrhash lispm-finder--icon-names))
    (let ((key (cons directory item)))
      (or (gethash key lispm-finder--icon-names)
          (puthash key (lispm-finder--file-icon-name directory item)
                   lispm-finder--icon-names)))))

(defun lispm-finder--file-icon-name (directory item)
  "Return the name of the large icon of ITEM of DIRECTORY, a directory."
  (let ((path (lispm-finder--child-path (car item) directory)))
    (if (cdr item)
        (if (file-exists-p (lispm-finder--child-path ".git" path)) "folder-git-32" "folder-32")
      (let* ((name (concat (lispm-ui-file-icon-name path) "-32"))
             (file (lispm-ui-icon-file name)))
        (if (and file (file-exists-p file)) name "document-32")))))

(defun lispm-finder--sync-state-signal ()
  "Synchronize the buffer-local reactive `lispm-finder--state-signal'."
  (unless (lispm-ui-signal-p lispm-finder--state-signal)
    (setq lispm-finder--state-signal (lispm-ui-signal nil)))
  (lispm-ui-signal-set lispm-finder--state-signal
                       (list lispm-finder--directory
                             lispm-finder--items
                             lispm-finder--selected)))

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
            lispm-finder--selected-app (nth 2 item)))
    (lispm-finder--sync-state-signal)))

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

(require 'lispm-finder-render)

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
  (declare (indent 0) (debug t))
  `(with-current-buffer (lispm-finder--buffer) ,@body))

(defun lispm-finder-state-signal ()
  "Return the reactive `lispm-ui-signal' of the Finder buffer.
Its value is `(DIRECTORY ITEMS SELECTED)'."
  (lispm-finder--in
    (unless (lispm-ui-signal-p lispm-finder--state-signal)
      (lispm-finder--sync-state-signal))
    lispm-finder--state-signal))

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
            lispm-finder--selected-app (lispm-finder--app valid-select)
            lispm-finder--rendered-key nil)
      (when (hash-table-p lispm-finder--icon-names)
        (clrhash lispm-finder--icon-names))
      (when (stringp lispm-finder--directory)
        (setq default-directory lispm-finder--directory))
      (lispm-finder--sync-state-signal)
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
(declare-function lispm-ui-form-prompt "lispm-ui-form-dress" (prompt &rest args))
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
      (lispm-finder--sync-state-signal)
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
    (let* ((next (lispm-ui-frp-type-select-step lispm-finder--typed last-command-event 1.0))
           (text (car next)))
      (setq lispm-finder--typed next)
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
  "M-<left>" #'lispm-finder-back
  "M-<right>" #'lispm-finder-forward
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

(defalias 'lispm-finder-path-items #'lispm-finder--path-items)
(defalias 'lispm-finder-keyboard-menu #'lispm-finder--keyboard-menu)

(provide 'lispm-finder)
(require 'lispm-finder-choose)

(setq lispm-ui-choose-file-function #'lispm-finder-choose-file)

;;; lispm-finder.el ends here
