;;; lispm-finder-i18n.el --- The language LispM Finder speaks  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
;; URL: https://github.com/LispMachineLab/lispm-finder
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

;; LispM Finder's words are written in English in the code, and shown in
;; the language `lispm-finder-current-language' names:
;; `lispm-finder-language' if set, else the language LispM UI or the
;; host program speaks (see `lispm-finder-language-functions'), else the
;; computer's own.  LispM Kit translates them from `i18n/'.

;;; Code:

(require 'lispm-kit-i18n)
(require 'lispm-ui-i18n)

(defconst lispm-finder-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "The directory of LispM Finder, holding its icons and translations.")

(defalias 'lispm-finder-tr-noop #'lispm-kit-tr-noop
  "Return MESSAGE as it is, marking it as words to translate where shown.

\(fn MESSAGE &optional CONTEXT)")

(defconst lispm-finder-languages lispm-kit-languages
  "The languages LispM Finder speaks: (TAG ENGLISH-NAME).")

(defcustom lispm-finder-language nil
  "The language LispM Finder speaks: a tag of `lispm-finder-languages', or nil.
Nil follows the program around (see `lispm-finder-language-functions'),
else LispM UI's language, else the computer's."
  :type `(choice (const :tag "The computer's language, or that of the program around" nil)
                 ,@(mapcar (pcase-lambda (`(,tag ,name)) `(const :tag ,name ,tag))
                           lispm-finder-languages))
  :group 'lispm-finder)

(defconst lispm-finder-i18n-directory (expand-file-name "i18n" lispm-finder-directory)
  "The directory of the catalogs, one per language: i18n/ of LispM Finder.")

(defvar lispm-finder-language-functions (list #'lispm-ui-current-language)
  "Functions saying the language of the program LispM Finder is shown by,
while `lispm-finder-language' is nil.")

(defvar lispm-finder-language-changed-hook nil
  "Hook run once LispM Finder speaks another language.")

(lispm-kit-i18n-define 'lispm-finder
  :directory lispm-finder-i18n-directory
  :language 'lispm-finder-language
  :language-functions 'lispm-finder-language-functions
  :changed-hook 'lispm-finder-language-changed-hook)

(defun lispm-finder-current-language ()
  "Return the language LispM Finder speaks now."
  (lispm-kit-current-language 'lispm-finder))

(defun lispm-finder-i18n-catalog-file (language)
  "Return the file of LANGUAGE's catalog."
  (lispm-kit-i18n-catalog-file 'lispm-finder language))

(defun lispm-finder-i18n-read-catalog (language)
  "Read LANGUAGE's catalog from its file."
  (lispm-kit-i18n-read-catalog 'lispm-finder language))

(defun lispm-finder-language-name (language)
  "Return the name of LANGUAGE in itself, as its catalog gives it."
  (lispm-kit-language-name 'lispm-finder language))

(defun lispm-finder-tr-in (context message &rest args)
  "Return MESSAGE, as meant in CONTEXT, in the current language."
  (apply #'lispm-kit-tr-in 'lispm-finder context message args))

(defun lispm-finder-tr (message &rest args)
  "Return MESSAGE in the current language, formatted with ARGS when any."
  (apply #'lispm-kit-tr 'lispm-finder message args))

(defun lispm-finder-tr-n (singular plural count &rest args)
  "Return the form of SINGULAR or PLURAL for COUNT in the current language."
  (apply #'lispm-kit-tr-n 'lispm-finder singular plural count args))

(defun lispm-finder-language-changed ()
  "Draw again what shows words when the language changes."
  (lispm-kit-language-changed 'lispm-finder))

(add-hook 'lispm-ui-language-changed-hook #'lispm-finder-language-changed)

(provide 'lispm-finder-i18n)
;;; lispm-finder-i18n.el ends here
