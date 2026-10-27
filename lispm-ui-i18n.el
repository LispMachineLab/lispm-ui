;;; lispm-ui-i18n.el --- The language LispM UI speaks  -*- lexical-binding: t; -*-

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

;; LispM UI's words, as "Cancel" or "Choose a File", are written in
;; English in the code, and shown in the language
;; `lispm-ui-current-language' names: `lispm-ui-language' if set, else
;; the language of the program LispM UI is shown by, if it says one (see
;; `lispm-ui-language-functions'), else the computer's own.  Every word
;; a person reads passes through one of:
;;
;;   (lispm-ui-tr MESSAGE ARGS...)   MESSAGE translated, then formatted
;;                                   with ARGS, if any
;;   (lispm-ui-tr-n SINGULAR PLURAL COUNT ARGS...)
;;                                   the form for COUNT, formatted with
;;                                   ARGS, or with COUNT
;;   (lispm-ui-tr-in CONTEXT MESSAGE ARGS...)
;;                                   MESSAGE as meant in CONTEXT, where
;;                                   the same English says two things
;;
;; and `lispm-ui-tr-noop' marks words kept as a file loads, translated
;; where shown.  LispM Kit translates them, as it does every LispM
;; program's words (lispm-kit-i18n.el): LispM UI keeps only its
;; catalogs, in i18n/, one per language, the domain `lispm-ui'.
;;
;; The program LispM UI is shown by says its language with a function of
;; `lispm-ui-language-functions', and calls `lispm-ui-language-changed'
;; when it speaks another, so that what shows words is drawn again, by
;; `lispm-ui-language-changed-hook'.

;;; Code:

(require 'lispm-kit-i18n)
(require 'lispm-ui-base)

(defalias 'lispm-ui-tr-noop #'lispm-kit-tr-noop
  "Return MESSAGE as it is, marking it as words to translate where shown.
For words kept as a file loads, see lispm-ui-i18n.el.  MESSAGE as meant
in CONTEXT carries it, and `lispm-ui-tr' translates it so.

\(fn MESSAGE &optional CONTEXT)")

(defconst lispm-ui-languages lispm-kit-languages
  "The languages LispM UI speaks: (TAG ENGLISH-NAME).  TAG names the
catalog of i18n/.")

(defcustom lispm-ui-language nil
  "The language LispM UI speaks: a tag of `lispm-ui-languages', or nil.
Nil follows the program LispM UI is shown by, if it says its language
\(see `lispm-ui-language-functions'), else the computer's.  A new value
shows at once."
  :type `(choice (const :tag "The computer's language, or that of the program around" nil)
                 ,@(mapcar (pcase-lambda (`(,tag ,name)) `(const :tag ,name ,tag))
                           lispm-ui-languages))
  :group 'lispm-ui)

(defconst lispm-ui-i18n-directory (expand-file-name "i18n" lispm-ui-directory)
  "The directory of the catalogs, one per language: i18n/ of LispM UI.")

(defvar lispm-ui-language-functions nil
  "Functions saying the language of the program LispM UI is shown by, for
it to speak while `lispm-ui-language' is nil.  Each is called with no
arguments and returns a language tag, or nil; the first tag of
`lispm-ui-languages' returned is spoken, else the computer's language.
A program adding one calls `lispm-ui-language-changed' once its
language changes.")

(defvar lispm-ui-language-changed-hook nil
  "Hook run once LispM UI speaks another language, to draw again what
shows words.")

(lispm-kit-i18n-define 'lispm-ui
  :directory lispm-ui-i18n-directory
  :language 'lispm-ui-language
  :language-functions 'lispm-ui-language-functions
  :changed-hook 'lispm-ui-language-changed-hook)

(defun lispm-ui-current-language ()
  "Return the language LispM UI speaks now, a tag: `lispm-ui-language',
else the first a function of `lispm-ui-language-functions' returns that
it speaks, else the computer's."
  (lispm-kit-current-language 'lispm-ui))

;;;; Catalogs

(defun lispm-ui-i18n-catalog-file (language)
  "Return the file of LANGUAGE's catalog."
  (lispm-kit-i18n-catalog-file 'lispm-ui language))

(defun lispm-ui-i18n-read-catalog (language)
  "Read LANGUAGE's catalog from its file; see `lispm-kit-i18n-read-catalog'."
  (lispm-kit-i18n-read-catalog 'lispm-ui language))

(defun lispm-ui-language-name (language)
  "Return the name of LANGUAGE in itself, as its catalog gives it."
  (lispm-kit-language-name 'lispm-ui language))

;;;; Translating

(defun lispm-ui-tr-in (context message &rest args)
  "Return MESSAGE, as meant in CONTEXT, a string or nil, in the current
language, formatted with ARGS when there are any."
  (apply #'lispm-kit-tr-in 'lispm-ui context message args))

(defun lispm-ui-tr (message &rest args)
  "Return MESSAGE in the current language, formatted with ARGS when there
are any.  MESSAGE is the English written in the code; see
lispm-ui-i18n.el.  A MESSAGE marked with a context by `lispm-ui-tr-noop'
is translated as meant in it."
  (apply #'lispm-kit-tr 'lispm-ui message args))

(defun lispm-ui-tr-n (singular plural count &rest args)
  "Return the form of SINGULAR or PLURAL for COUNT in the current language,
formatted with ARGS, or with COUNT when there are none."
  (apply #'lispm-kit-tr-n 'lispm-ui singular plural count args))

(defun lispm-ui-format-time (format &optional time)
  "Return TIME, by default now, as FORMAT, translated, says it, the names
of months and days in the current language; see `format-time-string'."
  (lispm-kit-format-time 'lispm-ui format time))

;;;; Changing the language

(defun lispm-ui-language-changed ()
  "Draw again what shows words, now that the language a function of
`lispm-ui-language-functions' returns has changed, unless
`lispm-ui-language' chooses one: a program saying its language calls
this once it speaks another."
  (lispm-kit-language-changed 'lispm-ui))

(provide 'lispm-ui-i18n)
;;; lispm-ui-i18n.el ends here
