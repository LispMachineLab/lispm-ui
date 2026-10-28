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
;; these do.  An alert, and choices, are drawn by LispM UI itself, by
;; default, once it has a palette to draw in, see lispm-ui-alert.el and
;; lispm-ui-menu.el.  The keyboard answers each, whatever draws it.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-ui-i18n)
(require 'lispm-ui-palette)

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

(defvar lispm-ui-choose-file-function nil
  "Function showing files to choose one of, as a dialog does, or nil for
the minibuffer.  It is called with the keyword arguments of
`lispm-ui-choose-file', and returns what it returns.")

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
        (and (file-regular-p chosen) (expand-file-name chosen)))
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

(provide 'lispm-ui-ask)
;;; lispm-ui-ask.el ends here
