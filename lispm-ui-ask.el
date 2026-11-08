;;; lispm-ui-ask.el --- Asking a person: alerts, choices, files  -*- lexical-binding: t; -*-

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

;; What an application asks a person, it asks here, and the person
;; answers as the interface showing it lets them:
;;
;;   (lispm-ui-alert ICON MESSAGE BUTTONS DETAIL)
;;                    an alert, the value of the button pressed
;;   (lispm-ui-confirm QUESTION DETAIL ACTION)
;;                    a question to agree to, non-nil when agreed
;;   (lispm-ui-choose CHOICES VALUE POSITION LABEL)
;;                    one of CHOICES, as a pop-up menu offers them
;;   (lispm-ui-choose-file :title :file :directory :predicate :preview)
;;                    a file, as a dialog shows a folder's
;;
;; Each is answered in the minibuffer, and never in the system's own
;; dialogs, unless something draws it: `lispm-ui-alert-function',
;; `lispm-ui-choose-function' and `lispm-ui-choose-file-function' are
;; called instead when set, with the same arguments, and return what
;; these do.  An alert, choices and a file are drawn by LispM UI itself,
;; by default, once it has a palette to draw in, see lispm-ui-alert.el,
;; lispm-ui-menu.el and lispm-ui-file-dialog.el.  The keyboard answers
;; each, whatever draws it.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-ui-base)
(require 'lispm-ui-i18n)
(require 'lispm-ui-palette)
(require 'lispm-ui-popup)

(autoload 'lispm-ui-form-prompt "lispm-ui-form")

;;;; Alerts

(autoload 'lispm-ui-alert-default "lispm-ui-alert")

(defvar lispm-ui-alert-function #'lispm-ui-alert-default
  "Function showing an alert, or nil for the minibuffer.
It is called with the arguments of `lispm-ui-alert', and returns the
value of the button chosen.  The default, `lispm-ui-alert-default',
draws it where a panel can pop up, and asks in the minibuffer
elsewhere.")

(defun lispm-ui--alert-keys (buttons)
  "Return a key for each of BUTTONS, as an alert takes them, as (KEY
LABEL VALUE): a letter of its label not taken yet, else of its value's
name, else a digit.  Each is typed without an input method, as a label
in Chinese or Japanese has no letter to type."
  (let ((used nil))
    (mapcar (pcase-lambda (`(,label ,value . ,_))
              (let ((key (seq-find (lambda (c) (and (or (<= ?a c ?z) (<= ?1 c ?9)) (not (memq c used))))
                                   (concat (downcase label)
                                           (and (symbolp value) (downcase (symbol-name value)))
                                           "123456789"))))
                (push key used)
                (list key label value)))
            buttons)))

(defun lispm-ui--alert-in-minibuffer (message buttons detail cancel)
  "Ask MESSAGE in the minibuffer, offering BUTTONS; return the value chosen.
DETAIL follows MESSAGE; quitting returns CANCEL.  Never in a dialog box,
which `read-multiple-choice' would pop up when asked from a timer: a key
answers in the minibuffer."
  (condition-case nil
      (let* ((keys (lispm-ui--alert-keys buttons))
             (use-dialog-box nil)
             (answer (read-multiple-choice
                      (concat message (when detail (concat "  " detail)))
                      (mapcar (pcase-lambda (`(,key ,label ,_)) (list key label)) keys))))
        (nth 2 (assq (car answer) keys)))
    (quit cancel)))

(defun lispm-ui-alert (icon message buttons &optional detail)
  "Show an alert of MESSAGE and return the value of the button chosen.
ICON is `caution', `stop' or `note'; DETAIL is a line or two under
MESSAGE.  BUTTONS is a list of (LABEL VALUE ROLE KEYS...): ROLE
`default' marks the button Return presses, `cancel' the one Escape
does, its VALUE not nil, as an alert may take a Cancel worth nil for
none; KEYS are events pressing a button too.  Shown by
`lispm-ui-alert-function', else asked in the minibuffer, a letter a
button, quitting choosing the cancel button."
  (if lispm-ui-alert-function
      (funcall lispm-ui-alert-function icon message buttons detail)
    (lispm-ui--alert-in-minibuffer
     message buttons detail
     (nth 1 (seq-find (lambda (button) (eq (nth 2 button) 'cancel)) buttons)))))

(defun lispm-ui--alert-shown-p ()
  "Return non-nil when an alert is shown rather than asked in the
minibuffer: by a `lispm-ui-alert-function' of its own, or by LispM UI's
once it has a palette to draw in."
  (and lispm-ui-alert-function
       (or (not (eq lispm-ui-alert-function #'lispm-ui-alert-default))
           lispm-ui-current-palette)))

(defun lispm-ui-confirm (question &optional detail action)
  "Ask QUESTION, DETAIL under it, and return non-nil when the person agrees.
ACTION names what agreeing does, as \"Delete\": the label of the
default button of the alert asking, by default OK, beside Cancel.
Where no alert is shown, see `lispm-ui--alert-shown-p', `yes-or-no-p'
asks."
  (if (lispm-ui--alert-shown-p)
      ;; Cancel is worth something: an alert may take a Cancel worth nil
      ;; for none, and Escape then would press the default.
      (eq (lispm-ui-alert 'caution question
                          `((,(lispm-ui-tr "Cancel") cancel cancel)
                            (,(or action (lispm-ui-tr "OK")) agree default))
                          detail)
          'agree)
    (let ((use-dialog-box nil))
      (yes-or-no-p (concat question (and detail (concat " " detail)) " ")))))

;;;; Choices

(autoload 'lispm-ui-choose-default "lispm-ui-menu")

(defvar lispm-ui-choose-function #'lispm-ui-choose-default
  "Function showing choices, as a pop-up menu does, or nil for the minibuffer.
It is called with the arguments of `lispm-ui-choose', and returns what
it returns.  The default, `lispm-ui-choose-default', shows them in a
menu where one can pop up, and reads them in the minibuffer elsewhere.")

(defun lispm-ui-choose (choices &optional value position label)
  "Show CHOICES, a list of (VALUE . TEXT), VALUE's checked; return (VALUE)
of the one chosen, or nil when none was.  POSITION, a buffer position
or a mouse event, says where they show, by default point; LABEL names
what is chosen.  Shown by `lispm-ui-choose-function', else read in the
minibuffer after LABEL."
  (if lispm-ui-choose-function
      (funcall lispm-ui-choose-function choices value position label)
    (let* ((current (cdr (assoc value choices)))
           (text (completing-read (format-prompt (or label "") current)
                                  (mapcar #'cdr choices) nil t nil nil current)))
      (when-let* ((choice (rassoc text choices)))
        (list (car choice))))))

;;;; Files

(autoload 'lispm-ui-file-dialog "lispm-ui-file-dialog")

(defvar lispm-ui-choose-file-function #'lispm-ui-file-dialog
  "Function showing files to choose one of, as a dialog does, or nil for
the minibuffer.  It is called with the keyword arguments of
`lispm-ui-choose-file', and returns what it returns.  By default it is
LispM UI's own dialog, which reads the file in the minibuffer where it
cannot be drawn.")

(defun lispm-ui--choose-file-in-minibuffer (title file directory predicate)
  "Read a file in the minibuffer, as the dialog of TITLE would choose one:
from FILE's folder or DIRECTORY, among those PREDICATE takes; never in
the system's own panel."
  (condition-case nil
      (let* ((use-dialog-box nil)
             (chosen (read-file-name (format-prompt title nil)
                                     (if file (file-name-directory file) directory)
                                     nil t (and file (file-name-nondirectory file))
                                     (lambda (name)
                                       (or (file-directory-p name) (null predicate) (funcall predicate name))))))
        (and (or (file-regular-p chosen)
                 (and (eq predicate #'file-directory-p)
                      (file-directory-p chosen)))
             (expand-file-name chosen)))
    (quit nil)))

(cl-defun lispm-ui-choose-file (&key title file directory predicate preview)
  "Show files to choose one of; return the file chosen, or nil.
TITLE says what is chosen, by default \"Choose a File\".  It starts at
FILE's folder, FILE selected, else at DIRECTORY, by default
`default-directory'.  It offers the folders, and the files PREDICATE, a
function of a file's name, takes, by default all; PREVIEW asks for the
picture selected to be shown.  Shown by `lispm-ui-choose-file-function',
else read in the minibuffer, never in the system's own panel."
  (let ((title (or title (lispm-ui-tr "Choose a File")))
        (directory (or (and file (file-name-directory file)) directory default-directory)))
    (if lispm-ui-choose-file-function
        (funcall lispm-ui-choose-file-function :title title :file file :directory directory
                 :predicate predicate :preview preview)
      (lispm-ui--choose-file-in-minibuffer title file directory predicate))))

;;;; Intercepting GUI prompts

(defvar lispm-ui--intercepting nil
  "Non-nil while an intercepted prompt is already showing a dialog.")

(defun lispm-ui--clean-prompt (prompt &optional fallback)
  "Return PROMPT trimmed of trailing colons and whitespace, or FALLBACK."
  (let ((clean (replace-regexp-in-string "[ \t]*:?[ \t]*\\'" "" (or prompt ""))))
    (if (string-empty-p clean)
        (or fallback (lispm-ui-tr "Choose"))
      clean)))

(defun lispm-ui--intercept-yes-or-no (orig-fn prompt)
  "Route `y-or-n-p' and `yes-or-no-p' to `lispm-ui-alert' for GUI actions."
  (if (and (not lispm-ui--intercepting)
           (lispm-ui-gui-action-p)
           (lispm-ui--alert-shown-p))
      (let ((lispm-ui--intercepting t)
            (lispm-ui-in-gui-action t))
        (eq (lispm-ui-alert 'caution (string-trim (or prompt ""))
                            `((,(lispm-ui-tr "Cancel") cancel cancel ?n)
                              (,(lispm-ui-tr "OK") agree default ?y)))
            'agree))
    (funcall orig-fn prompt)))

(defun lispm-ui--intercept-multiple-choice (orig-fn prompt choices &rest args)
  "Route `read-multiple-choice' to `lispm-ui-alert' for GUI actions."
  (if (and (not lispm-ui--intercepting)
           (lispm-ui-gui-action-p)
           (lispm-ui--alert-shown-p)
           choices)
      (let* ((lispm-ui--intercepting t)
             (lispm-ui-in-gui-action t)
             (buttons
              (cons (list (lispm-ui-tr "Cancel") 'cancel 'cancel)
                    (seq-map-indexed
                     (lambda (entry index)
                       (let ((key (nth 0 entry))
                             (label (capitalize (or (nth 1 entry) ""))))
                         (list label key (and (= index 0) 'default) key)))
                     choices)))
             (detail (mapconcat (lambda (entry) (or (nth 2 entry) ""))
                                (seq-filter (lambda (entry) (nth 2 entry)) choices)
                                "\n"))
             (chosen (lispm-ui-alert 'caution (lispm-ui--clean-prompt prompt)
                                     buttons
                                     (and (not (string-empty-p detail)) detail))))
        (if (or (null chosen) (eq chosen 'cancel))
            (signal 'quit nil)
          (assq chosen choices)))
    (apply orig-fn prompt choices args)))

(defun lispm-ui--intercept-read-string (orig-fn prompt &optional initial-input
                                                history default-value
                                                inherit-input-method)
  "Route `read-string' to `lispm-ui-form-prompt' for GUI actions."
  (if (and (not lispm-ui--intercepting)
           (lispm-ui-gui-action-p)
           (lispm-ui-popup-available-p))
      (let ((lispm-ui--intercepting t)
            (lispm-ui-in-gui-action t)
            (init (if (consp initial-input) (car initial-input) initial-input))
            (def (if (consp default-value) (car default-value) default-value)))
        (lispm-ui-form-prompt prompt :initial init :default def))
    (funcall orig-fn prompt initial-input history default-value inherit-input-method)))

(defun lispm-ui--intercept-read-passwd (orig-fn prompt &optional confirm default)
  "Route `read-passwd' to `lispm-ui-form-prompt' for GUI actions."
  (if (and (not lispm-ui--intercepting)
           (lispm-ui-gui-action-p)
           (lispm-ui-popup-available-p))
      (let ((lispm-ui--intercepting t)
            (lispm-ui-in-gui-action t))
        (lispm-ui-form-prompt prompt :default default :password t))
    (funcall orig-fn prompt confirm default)))

(defun lispm-ui--intercept-read-number (orig-fn prompt &optional default hist)
  "Route `read-number' to `lispm-ui-form-prompt' for GUI actions."
  (if (and (not lispm-ui--intercepting)
           (lispm-ui-gui-action-p)
           (lispm-ui-popup-available-p))
      (let* ((lispm-ui--intercepting t)
             (lispm-ui-in-gui-action t)
             (def-str (and default (format "%s" (if (consp default) (car default) default))))
             (text (lispm-ui-form-prompt
                    prompt
                    :default def-str
                    :validate (lambda (val)
                                (let ((s (or val def-str "")))
                                  (unless (string-match-p
                                           "\\`[ \t]*[-+]?[0-9]*\\.?[0-9]+\\([eE][-+]?[0-9]+\\)?[ \t]*\\'"
                                           s)
                                    (lispm-ui-tr "%s: fill this in"
                                                 (lispm-ui--clean-prompt prompt))))))))
        (string-to-number text))
    (funcall orig-fn prompt default hist)))

(defun lispm-ui--completion-candidates (collection predicate)
  "Return candidate strings from COLLECTION satisfying PREDICATE."
  (cond
   ((and (listp collection) (null predicate)
         (seq-every-p (lambda (x) (or (stringp x) (symbolp x) (consp x))) collection))
    (mapcar (lambda (x)
              (let ((k (if (consp x) (car x) x)))
                (if (symbolp k) (symbol-name k) (format "%s" k))))
            collection))
   (t
    (ignore-errors (all-completions "" collection predicate)))))

(defun lispm-ui--intercept-completing-read (orig-fn prompt collection
                                                    &optional predicate require-match
                                                    initial-input hist def
                                                    inherit-input-method)
  "Route `completing-read' to a menu or form dialog for GUI actions."
  (if (and (not lispm-ui--intercepting)
           (not (eq collection #'read-file-name-internal))
           (lispm-ui-gui-action-p)
           (lispm-ui-popup-available-p))
      (let* ((lispm-ui--intercepting t)
             (lispm-ui-in-gui-action t)
             (init (if (consp initial-input) (car initial-input) initial-input))
             (def-val (if (consp def) (car def) def))
             (candidates (lispm-ui--completion-candidates collection predicate))
             (count (length candidates)))
        (if (and require-match (>= count 1) (<= count 30))
            (let ((chosen (lispm-ui-choose (mapcar (lambda (c) (cons c c)) candidates)
                                           (or def-val init)
                                           nil
                                           (lispm-ui--clean-prompt prompt))))
              (if chosen
                  (car chosen)
                (signal 'quit nil)))
          (lispm-ui-form-prompt
           prompt
           :initial init
           :default def-val
           :choices (and (<= count 60) candidates)
           :require-match (and require-match (<= count 60) (> count 0) t)
           :validate (and require-match (> count 60)
                          (lambda (val)
                            (let ((s (or val def-val "")))
                              (unless (test-completion s collection predicate)
                                (lispm-ui-tr "%s: fill this in"
                                             (lispm-ui--clean-prompt prompt)))))))))
    (funcall orig-fn prompt collection predicate require-match
             initial-input hist def inherit-input-method)))

(defun lispm-ui--intercept-read-file-name (orig-fn prompt &optional dir
                                                   default-filename mustmatch
                                                   initial predicate)
  "Route `read-file-name' to a file or form dialog for GUI actions."
  (if (and (not lispm-ui--intercepting)
           (lispm-ui-gui-action-p)
           (lispm-ui-popup-graphic-p))
      (let* ((lispm-ui--intercepting t)
             (lispm-ui-in-gui-action t)
             (title (lispm-ui--clean-prompt prompt (lispm-ui-tr "Choose a File")))
             (directory (or dir default-directory)))
        (if (and (null mustmatch) (stringp initial) (not (string-empty-p initial))
                 (not (eq predicate #'file-directory-p)))
            (lispm-ui-form-prompt
             title
             :initial (if dir (expand-file-name initial dir) initial)
             :default (if (consp default-filename) (car default-filename) default-filename))
          (let ((chosen (lispm-ui-choose-file
                         :title title
                         :file (and initial (expand-file-name initial directory))
                         :directory directory
                         :predicate predicate)))
            (or chosen (signal 'quit nil)))))
    (funcall orig-fn prompt dir default-filename mustmatch initial predicate)))

(defun lispm-ui--intercept-read-directory-name (orig-fn prompt &optional dir
                                                        default-dirname mustmatch
                                                        initial)
  "Route `read-directory-name' to `lispm-ui-choose-file' for GUI actions."
  (if (and (not lispm-ui--intercepting)
           (lispm-ui-gui-action-p)
           (lispm-ui-popup-graphic-p))
      (let* ((lispm-ui--intercepting t)
             (lispm-ui-in-gui-action t)
             (title (lispm-ui--clean-prompt prompt (lispm-ui-tr "Choose")))
             (chosen (lispm-ui-choose-file
                      :title title
                      :directory (or dir default-directory)
                      :predicate #'file-directory-p)))
        (or chosen (signal 'quit nil)))
    (funcall orig-fn prompt dir default-dirname mustmatch initial)))

(defconst lispm-ui--intercept-advices
  '((y-or-n-p . lispm-ui--intercept-yes-or-no)
    (yes-or-no-p . lispm-ui--intercept-yes-or-no)
    (read-multiple-choice . lispm-ui--intercept-multiple-choice)
    (read-string . lispm-ui--intercept-read-string)
    (read-passwd . lispm-ui--intercept-read-passwd)
    (read-number . lispm-ui--intercept-read-number)
    (completing-read . lispm-ui--intercept-completing-read)
    (read-file-name . lispm-ui--intercept-read-file-name)
    (read-directory-name . lispm-ui--intercept-read-directory-name))
  "Functions intercepted when a GUI action asks a person.")

(defun lispm-ui-intercept-enable ()
  "Answer GUI-initiated prompts with LispM UI's alerts, menus, forms and dialogs.
Keyboard commands typed directly in a buffer are left to Emacs."
  (pcase-dolist (`(,target . ,advice) lispm-ui--intercept-advices)
    (unless (advice-member-p advice target)
      (advice-add target :around advice))))

(defun lispm-ui-intercept-disable ()
  "Stop intercepting GUI-initiated prompts."
  (pcase-dolist (`(,target . ,advice) lispm-ui--intercept-advices)
    (advice-remove target advice)))

(provide 'lispm-ui-ask)
;;; lispm-ui-ask.el ends here
