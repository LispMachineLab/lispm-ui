;;; lispm-ui-popup.el --- Panels popped up over a frame  -*- lexical-binding: t; -*-

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

;; Mac OS 9 drew its menus and alerts itself, and so does LispM UI, each
;; in a child frame over the frame it belongs to: a popup.  A popup
;; takes no focus, holds the size it is given whatever its font, and casts
;; a shadow; on a text terminal, whose Emacs 31 shows child frames, it is
;; text outlined in lines Emacs draws round it.  The panels themselves
;; are lispm-ui-alert.el's and lispm-ui-menu.el's; they read the mouse and
;; the keys themselves, see lispm-ui-modal.el.
;;
;; Popups are drawn in the palette, so they pop up only once there is one,
;; see lispm-ui-palette.el; where none can (no palette, a text terminal of
;; an older Emacs, batch), what they ask is asked in the minibuffer.

;;; Code:

(require 'subr-x)
(require 'lispm-ui-base)
(require 'lispm-ui-palette)

(defun lispm-ui-popup-available-p ()
  "Return non-nil when menus and alerts can pop up: in an interactive Emacs
on a graphical display, or on a text terminal whose Emacs shows child
frames there, drawn in text, see `lispm-ui-popup-text-p'."
  (and (not noninteractive) lispm-ui-current-palette
       (or (display-graphic-p) (lispm-ui-popup-text-p))))

(defun lispm-ui-popup-graphic-p ()
  "Return non-nil when popups can be drawn as images: on a graphical display.
Panels such as the workspaces' thumbnails have no text to show instead."
  (and (display-graphic-p) (lispm-ui-popup-available-p)))

(defun lispm-ui-popup-text-p ()
  "Return non-nil when menus and alerts pop up as text, in child frames of a
text terminal, which Emacs 31 shows: outlined in lines Emacs draws round
them, see `standard-display-unicode-special-glyphs'."
  (and (lispm-ui-tty-p) (featurep 'tty-child-frames)))

(defun lispm-ui-popup-make-frame (parent)
  "Return a hidden child frame of PARENT with a buffer of its own.
It takes no focus, and hooks run for new frames do not see it."
  (let* ((buffer (generate-new-buffer " *lispm-ui-popup*"))
         (frame (let ((after-make-frame-functions nil)
                      (before-make-frame-hook nil))
                  (make-frame
                   `((parent-frame . ,parent)
                     (minibuffer . nil) (no-accept-focus . t) (no-focus-on-map . t)
                     ;; A text terminal outlines a decorated child frame.
                     (undecorated . ,(not (lispm-ui-tty-p parent)))
                     (unsplittable . t) (no-other-frame . t)
                     (child-frame-border-width . 1) (internal-border-width . 0)
                     (left-fringe . 0) (right-fringe . 0)
                     (vertical-scroll-bars . nil) (horizontal-scroll-bars . nil)
                     (menu-bar-lines . 0) (tool-bar-lines . 0) (tab-bar-lines . 0)
                     (line-spacing . 0) (cursor-type . nil) (no-special-glyphs . t)
                     (desktop-dont-save . t) (visibility . nil)
                     (background-color . ,(lispm-ui-color :chrome))
                     (foreground-color . ,(lispm-ui-color :text))
                     (width . 20) (height . 2))))))
    (with-current-buffer buffer
      (setq-local mode-line-format nil
                  header-line-format nil
                  tab-line-format nil
                  cursor-type nil
                  cursor-in-non-selected-windows nil
                  display-line-numbers nil
                  auto-hscroll-mode nil
                  truncate-lines t
                  line-spacing 0)
      (lispm-ui-popup-hold-size))
    (let ((root (frame-root-window frame)))
      (set-window-buffer root buffer)
      (set-window-dedicated-p root t)
      (set-window-fringes root 0 0))
    frame))

(defun lispm-ui-popup-dispose (frame)
  "Delete popup FRAME, its shadow and its buffer."
  (when (frame-live-p frame)
    (let ((buffer (window-buffer (frame-root-window frame)))
          (shadow (frame-parameter frame 'lispm-ui-popup-shadow)))
      (when (frame-live-p shadow)
        (delete-frame shadow t))
      (delete-frame frame t)
      (kill-buffer buffer))))

(defun lispm-ui-popup--shadow (frame kind)
  "Show the shadow of popup FRAME, a frame of its size a pixel down and right.
KIND is `menu', whose shadow is a dark grey line along its right and
bottom, or `alert', whose shadow is black.  The shadow is shown before
FRAME, so FRAME stands over it."
  (let* ((parent (frame-parent frame))
         (color (if (eq kind 'alert)
                    (lispm-ui-color :outline)
                  (lispm-ui-grey lispm-ui-current-palette #x22)))
         (shadow (let ((after-make-frame-functions nil)
                       (before-make-frame-hook nil))
                   (make-frame
                    `((parent-frame . ,parent)
                      (minibuffer . nil) (no-accept-focus . t) (no-focus-on-map . t)
                      (undecorated . t) (unsplittable . t) (no-other-frame . t)
                      (child-frame-border-width . 0) (internal-border-width . 0)
                      (left-fringe . 0) (right-fringe . 0)
                      (vertical-scroll-bars . nil) (horizontal-scroll-bars . nil)
                      (menu-bar-lines . 0) (tool-bar-lines . 0) (tab-bar-lines . 0)
                      (cursor-type . nil) (no-special-glyphs . t)
                      (desktop-dont-save . t) (visibility . nil)
                      (background-color . ,color)
                      (width . 2) (height . 1))))))
    (set-window-buffer (frame-root-window shadow) (get-buffer-create " *lispm-ui-shadow*"))
    (with-current-buffer " *lispm-ui-shadow*"
      (setq-local mode-line-format nil header-line-format nil tab-line-format nil cursor-type nil)
      (lispm-ui-popup-hold-size))
    (lispm-ui-popup-set-size shadow (frame-pixel-width frame) (frame-pixel-height frame))
    (pcase-let ((`(,x . ,y) (frame-position frame)))
      (set-frame-position shadow (1+ x) (1+ y)))
    (set-frame-parameter frame 'lispm-ui-popup-shadow shadow)
    (make-frame-visible shadow)))

(defun lispm-ui-popup-fill (frame text)
  "Replace popup FRAME's text with TEXT, scrolled to its start."
  (let ((window (frame-root-window frame)))
    (with-current-buffer (window-buffer window)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text)))
    (set-window-start window 1)
    (set-window-point window 1)
    (set-window-hscroll window 0)))

(defun lispm-ui-popup-set-size (frame width height)
  "Make popup FRAME's text area WIDTH by HEIGHT pixels.
FRAME keeps that size while its buffer holds it, see
`lispm-ui-popup-hold-size'."
  (set-frame-parameter frame 'lispm-ui-popup-size (cons width height))
  (let ((window-min-width 1)
        (window-safe-min-width 1)
        (window-min-height 1)
        (window-safe-min-height 1)
        (frame-resize-pixelwise t))
    (set-frame-size frame width height t)))

(defun lispm-ui-popup--keep-size (window)
  "Size WINDOW's frame again as `lispm-ui-popup-set-size' last did, once
Emacs has sized it anew.  This is a buffer-local
`window-size-change-functions' function."
  (let* ((frame (window-frame window))
         (size (frame-parameter frame 'lispm-ui-popup-size)))
    (when (and size (not (and (= (frame-text-width frame) (car size))
                              (= (frame-text-height frame) (cdr size)))))
      (lispm-ui-popup-set-size frame (car size) (cdr size)))))

(defun lispm-ui-popup-hold-size ()
  "Have the frame showing the current buffer hold the size
`lispm-ui-popup-set-size' gives it.
As its font changes, with `set-face-attribute' or a theme, Emacs sizes a
frame anew to whole lines and columns of the new font, and not below the
four lines and ten columns `window-min-height' and `window-min-width'
keep a window to.  The buffer asks its window for a line and a column
instead, as `lispm-ui-popup-set-size' does, and its frame is sized back
once Emacs has sized it."
  (setq-local window-min-height 1
              window-min-width 1
              window-safe-min-width 1)
  (add-hook 'window-size-change-functions #'lispm-ui-popup--keep-size nil t))

(defun lispm-ui-popup-show (frame x y &optional shadow)
  "Show popup FRAME with its top left at X, Y of its parent, kept inside it.
SHADOW, `menu' or `alert', casts its shadow, see `lispm-ui-popup--shadow'.
On a text terminal none is cast, and X, Y place the outline Emacs draws
round FRAME, a cell outside it, as they place the outline a graphical
display draws at FRAME's edge."
  (let ((parent (frame-parent frame)))
    (if (lispm-ui-tty-p parent)
        (set-frame-position frame
                            (1+ (max 0 (min x (- (frame-pixel-width parent) (frame-pixel-width frame) 2))))
                            (1+ (max 0 (min y (- (frame-pixel-height parent)
                                                 (frame-pixel-height frame) 2)))))
      (set-frame-position frame
                          (max 0 (min x (- (frame-pixel-width parent) (frame-pixel-width frame) 2)))
                          (max 0 (min y (- (frame-pixel-height parent)
                                           (frame-pixel-height frame) 2)))))
    (when (and shadow (not (lispm-ui-tty-p parent)))
      (lispm-ui-popup--shadow frame shadow))
    (make-frame-visible frame)
    (redisplay t)))

(provide 'lispm-ui-popup)
;;; lispm-ui-popup.el ends here
