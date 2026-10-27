;;; lispm-ui.el --- LispM's classic-style interface, in any Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
;; URL: https://github.com/LispMachineLab/lispm-ui
;; Version: 0.0.1
;; Package-Requires: ((emacs "30.1"))
;; Keywords: convenience, faces, mouse
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

;; LispM UI is what LispM and its applications are drawn with, in any
;; Emacs 30.1 or later, graphical or on a text terminal:
;;
;;   lispm-ui-base.el   where it is, whether a frame is a text terminal,
;;                      and lengths drawn in pixels counted in cells there
;;   lispm-ui-i18n.el   the language its words are shown in, and their
;;                      translations, i18n/
;;   lispm-ui-modal.el  what its modal panels share: reading events
;;                      without quitting, what each key does, focus
;;                      rings, holding a control pressed
;;
;; It knows of no desktop: an application requires it, or the part it
;; uses, and looks the same whether a desktop shows it or plain Emacs
;; does.  Requiring this file loads every part, so that a copy of LispM
;; UI found later on `load-path' cannot supply half of it.

;;; Code:

(require 'lispm-ui-base)
(require 'lispm-ui-i18n)
(require 'lispm-ui-modal)

(provide 'lispm-ui)
;;; lispm-ui.el ends here
