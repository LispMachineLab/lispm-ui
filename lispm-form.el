;;; lispm-form.el --- LispM's forms, by the names they had before  -*- lexical-binding: t; -*-

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

;; LispM's forms were LispM Agent's lispm-form.el before they moved here,
;; to lispm-ui-form.el.  A program written for them, which requires
;; `lispm-form', finds this file, and their names as they were: each
;; function, variable and face of lispm-form.el is the one of
;; lispm-ui-form.el it became, and a form's mode is a `lispm-form-mode'
;; too, as `derived-mode-p' asks.  The widgets' properties a program
;; reads, :lispm-form-key and :lispm-form-control, kept their names.

;;; Code:

(require 'lispm-ui-form)

(dolist (names '((lispm-form . lispm-ui-form)
                 (lispm-form-value . lispm-ui-form-value)
                 (lispm-form-values . lispm-ui-form-values)
                 (lispm-form-redraw . lispm-ui-form-redraw)
                 (lispm-form-submit . lispm-ui-form-submit)
                 (lispm-form-cancel . lispm-ui-form-cancel)
                 (lispm-form-choose . lispm-ui-form-choose)
                 (lispm-form-mode . lispm-ui-form-mode)))
  (defalias (car names) (cdr names)))

(dolist (names '((lispm-form-types . lispm-ui-form-types)
                 (lispm-form-glyph-function . lispm-ui-form-glyph-function)
                 (lispm-form-group-function . lispm-ui-form-group-function)
                 (lispm-form-choose-function . lispm-ui-form-choose-function)
                 (lispm-form-choose-file-function . lispm-ui-form-choose-file-function)
                 (lispm-form-drawn-hook . lispm-ui-form-drawn-hook)
                 (lispm-form-show-header . lispm-ui-form-show-header)
                 (lispm-form-width . lispm-ui-form-width)
                 (lispm-form-mode-map . lispm-ui-form-mode-map)
                 (lispm-form-mode-hook . lispm-ui-form-mode-hook)
                 (lispm-form--title . lispm-ui-form--title)
                 (lispm-form--geometry . lispm-ui-form--geometry)
                 (lispm-form--widgets . lispm-ui-form--widgets)))
  (defvaralias (car names) (cdr names)))

(dolist (names '((lispm-form-title . lispm-ui-form-title)
                 (lispm-form-label . lispm-ui-form-label)
                 (lispm-form-help . lispm-ui-form-help)
                 (lispm-form-area . lispm-ui-form-area)
                 (lispm-form-area-edge . lispm-ui-form-area-edge)))
  (put (car names) 'face-alias (cdr names)))

;; A form's buffer is in `lispm-form-mode' as it was.
(derived-mode-add-parents 'lispm-ui-form-mode '(lispm-form-mode))

(provide 'lispm-form)
;;; lispm-form.el ends here
