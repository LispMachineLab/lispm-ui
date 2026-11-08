;;; lispm-ui-base.el --- Where LispM UI is, and what it draws on  -*- lexical-binding: t; -*-

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

;; What every part of LispM UI stands on: its directory, which holds its
;; catalogs and pictures; whether a frame is a text terminal, where
;; controls are drawn in text, cells for pixels; and lengths drawn in
;; pixels, counted in cells there.  It requires nothing, so that every
;; other part can require it.

;;; Code:

(defgroup lispm-ui nil
  "LispM's classic-style interface: its controls, forms and panels."
  :group 'convenience
  :prefix "lispm-ui-")

(defconst lispm-ui-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "LispM UI's directory, which holds its Lisp, its catalogs, i18n/, and its
pictures, icons/.")

(defun lispm-ui-tty-p (&optional frame)
  "Return non-nil when FRAME, by default the selected one, is on a text terminal.
Controls are then drawn in text, cells for pixels.  A batch Emacs has
no display to draw on and is not one: what it draws, as for tests, is
what a graphical display gets."
  (and (not noninteractive) (not (display-graphic-p frame))))

(defun lispm-ui-px (pixels)
  "Return PIXELS, a length drawn in pixels, for the selected frame.
A text terminal counts a cell as a pixel, so there it is the cells as
many pixels of an 8-pixel-wide font would take: 4 to 11 pixels a cell,
a gap of fewer none."
  (if (lispm-ui-tty-p)
      (/ (+ pixels 4) 8)
    pixels))

(defvar lispm-ui-in-gui-action nil
  "Non-nil while a control, menu item or form button is running its action.")

(defvar lispm-ui-intercept-in-tests nil
  "Non-nil to let GUI interception hooks run even while ERT tests run.")

(defvar lispm-ui--command-from-mouse nil
  "Non-nil when `this-command' was started by a mouse event.")

(defun lispm-ui--record-command-source ()
  "Record whether `this-command' was started by the mouse or the keyboard."
  (setq lispm-ui--command-from-mouse
        (or (mouse-event-p last-command-event)
            (mouse-event-p last-input-event))))

(add-hook 'pre-command-hook #'lispm-ui--record-command-source)

(defun lispm-ui-gui-action-p ()
  "Return non-nil when the current operation comes from a GUI control or mouse.
Keyboard commands typed directly in a buffer return nil."
  (and (or (not noninteractive) lispm-ui-intercept-in-tests)
       (not executing-kbd-macro)
       (or lispm-ui-intercept-in-tests
           (not (bound-and-true-p ert--running-tests)))
       (or lispm-ui-in-gui-action
           lispm-ui--command-from-mouse
           (mouse-event-p last-input-event)
           (mouse-event-p last-command-event))))

(provide 'lispm-ui-base)
;;; lispm-ui-base.el ends here
