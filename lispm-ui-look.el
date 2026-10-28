;;; lispm-ui-look.el --- The looks LispM UI draws in  -*- lexical-binding: t; -*-

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

;; What LispM UI draws is drawn in a look, the forms of one system's
;; interface; `lispm-ui-look' names the one in use, by default `classic'.
;; A look is defined with `lispm-ui-look-define', which keeps it in
;; `lispm-ui-looks'.
;;
;; LispM UI's drawing functions are generic functions whose default
;; methods draw the classic look; a look draws its own forms in methods
;; specialized on it:
;;
;;   (cl-defmethod lispm-ui-draw-something
;;     (palette width &context (lispm-ui-look (eql win3)))
;;     ...)
;;
;; Looks drawn alike make a family, which draws them in methods of its
;; own, specialized on `lispm-ui-look-family':
;;
;;   (cl-defmethod lispm-ui-draw-something
;;     (palette width &context (lispm-ui-look (lispm-ui-look-family pc)))
;;     ...)
;;
;; A family's methods draw each of its looks by the look's metrics, see
;; `lispm-ui-look-metric'.  A look's own method, `(eql win3)', comes
;; before its family's, whichever was defined first, and reaches it with
;; `cl-call-next-method'.  Every drawing still takes its colours from
;; the palette, so any colour theme draws any look.
;;
;; What shows LispM UI and has looks of its own, as a desktop letting
;; its user choose one, defines them here and makes `lispm-ui-look'
;; name the one chosen, an alias of its own variable, say.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-ui-base)

(defvar lispm-ui-look 'classic
  "The look LispM UI draws in, a name of `lispm-ui-looks' or `classic',
the look its default methods draw.")

(defvar lispm-ui-looks nil
  "The looks defined, as (NAME . PROPERTIES), in the order defined.
See `lispm-ui-look-define'.")

(defun lispm-ui-look-define (name &rest properties)
  "Define the look NAME, the forms of one system's interface.
PROPERTIES is a plist; LispM UI reads:

  :title            its name, a proper name
  :description      a line on what it is, in English, translated where
                    shown
  :system-families  font families for titles, buttons and menus, the
                    first installed one winning
  :list-families    font families for lists and labels
  :family           the family of looks drawn alike it belongs to, whose
                    methods draw it, see lispm-ui-look.el's Commentary
  :metrics          a plist of the measures and forms its family's
                    methods draw it by, see `lispm-ui-look-metric'

A look may hold properties of its own too, which what defines it reads,
as the colour theme in its system's colours.  Defining NAME again
replaces its properties.  Return NAME."
  (if-let* ((look (assq name lispm-ui-looks)))
      (setcdr look properties)
    (setq lispm-ui-looks (append lispm-ui-looks (list (cons name properties)))))
  name)

(defun lispm-ui-look-get (property &optional look)
  "Return PROPERTY of LOOK, by default of `lispm-ui-look'."
  (plist-get (alist-get (or look lispm-ui-look) lispm-ui-looks) property))

(defun lispm-ui-look-title (&optional look)
  "Return the name of LOOK, by default of `lispm-ui-look'."
  (or (lispm-ui-look-get :title look)
      (capitalize (symbol-name (or look lispm-ui-look)))))

(defun lispm-ui-look-family (&optional look)
  "Return the family LOOK belongs to, by default `lispm-ui-look''s, or nil
for a look drawn alike no other."
  (lispm-ui-look-get :family look))

(defun lispm-ui-look-metric (key &optional look)
  "Return the metric KEY of LOOK, by default of `lispm-ui-look': a measure
or form its family's methods draw it by, see `lispm-ui-look-define'."
  (plist-get (lispm-ui-look-get :metrics look) key))

(defun lispm-ui-look-families (property default)
  "Return the font families PROPERTY of the look, or DEFAULT where the
look names none."
  (or (lispm-ui-look-get property) default))

;; A method specialized on (lispm-ui-look-family FAMILY) draws every look
;; of FAMILY.  The family is told by the look itself, so a look's own
;; `eql' method, of the higher priority, comes first: specializing on the
;; family through an `&context' expression of its own would put
;; whichever expression a generic function saw first before the other.
(cl-generic-define-generalizer lispm-ui-look--family-generalizer
  50 (lambda (look &rest _) `(lispm-ui-look-family ,look))
  (lambda (family &rest _) (and family `((lispm-ui-look-family ,family)))))

(cl-defmethod cl-generic-generalizers ((_specializer (head lispm-ui-look-family)))
  "Support for (lispm-ui-look-family FAMILY) specializers, a look of FAMILY."
  (list lispm-ui-look--family-generalizer))

(provide 'lispm-ui-look)
;;; lispm-ui-look.el ends here
