;;; lispm-ui.el --- LispM's classic-style interface, in any Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
;; URL: https://github.com/LispMachineLab/lispm-ui
;; Version: 0.4.0
;; Package-Requires: ((emacs "30.1") (lispm-kit "0.2.0"))
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
;;                      translations, i18n/, which LispM Kit's engine
;;                      reads
;;   lispm-ui-palette.el
;;                      the colours it draws in, derived from the colour
;;                      theme, and its faces
;;   lispm-ui-look.el   the looks it draws in, the forms of one system's
;;                      interface or another's
;;   lispm-ui-text.el   text laid out in pixels: widths, fitting, aligning
;;                      and breaking lines
;;   lispm-ui-draw.el   what it draws with: SVG primitives, text and
;;                      pictures drawn as Emacs shows them, and the cache
;;                      of drawn images
;;   lispm-ui-raster.el pictures drawn in text, in block characters, and
;;                      as PNGs, for text terminals
;;   lispm-ui-icons.el  icons, found by name: an interface's own, or
;;                      the alerts', which LispM UI has
;;   lispm-ui-controls.el
;;                      its controls: push buttons, pop-up menus, bevel
;;                      buttons, radio buttons, check boxes and group
;;                      boxes, drawn, as strings and in buffers
;;   lispm-ui-list.el   lists' column headings and disclosure triangles
;;   lispm-ui-scroll.el scroll bars, drawn
;;   lispm-ui-window.el windows' title bars, as dialogs draw them
;;   lispm-ui-modal.el  what its modal panels share: reading events
;;                      without quitting, what each key does, focus
;;                      rings, holding a control pressed
;;   lispm-ui-ask.el    asking a person: alerts, a question to agree
;;                      to, choices, a file
;;   lispm-ui-popup.el  panels popped up over a frame, in child frames
;;   lispm-ui-alert.el  alerts, drawn
;;   lispm-ui-menu.el   menus, drawn, and choices shown in them
;;   lispm-ui-form.el   forms, written as data
;;
;; lispm-form.el keeps the names LispM's forms had before they moved
;; here, for programs written for them.
;;
;; It knows of no desktop: an application requires it, or the part it
;; uses, and looks the same whether a desktop shows it or plain Emacs
;; does.  Requiring this file loads every part, so that a copy of LispM
;; UI found later on `load-path' cannot supply half of it.

;;; Code:

(require 'lispm-ui-base)
(require 'lispm-ui-i18n)
(require 'lispm-ui-palette)
(require 'lispm-ui-look)
(require 'lispm-ui-text)
(require 'lispm-ui-draw)
(require 'lispm-ui-raster)
(require 'lispm-ui-icons)
(require 'lispm-ui-modal)
(require 'lispm-ui-ask)
(require 'lispm-ui-controls)
(require 'lispm-ui-list)
(require 'lispm-ui-scroll)
(require 'lispm-ui-window)
(require 'lispm-ui-popup)
(require 'lispm-ui-alert)
(require 'lispm-ui-menu)
(require 'lispm-ui-form)

(provide 'lispm-ui)
;;; lispm-ui.el ends here
