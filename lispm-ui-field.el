;;; lispm-ui-field.el --- The minibuffer of a window drawn inside another  -*- lexical-binding: t; -*-

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

;; A window drawn inside another -- an application's window, a frame
;; another process shows -- has no minibuffer of its own on the screen: one
;; read from, or echoed to, in a frame of its own would stand apart from
;; the window it belongs to.  A field is that minibuffer instead: a
;; minibuffer-only child frame at the bottom of the window's frame, as wide
;; as it and as tall as what it shows, which the frame takes as its
;; minibuffer window, so that what is read and what is echoed happen inside
;; the window.
;;
;;   a frame --`lispm-ui-field-make'--> its field, the frame's minibuffer
;;   a message --`lispm-ui-field-echo'--> in the field of the frame it was
;;                                       meant for, for some seconds
;;   what is read --`lispm-ui-field-setup'--> the field shown, and as tall
;;                                           as what it shows
;;
;; A program makes a field with `lispm-ui-field-make' for a frame it draws
;; inside another, and `lispm-ui-field-dispose' takes it away again;
;; `lispm-ui-field-enable' arranges messages and reads for every field, and
;; is enough for a program whose frames go no other way.  Fields are child
;; frames: they are drawn on a graphical display, or on the text terminal of
;; an Emacs that shows child frames there, as popups are (see
;; lispm-ui-popup.el).

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'lispm-ui-base)
(require 'lispm-ui-palette)

;;;; Fields

(defvar lispm-ui-field-max-lines 6
  "The most lines a field is tall, however much it shows.")

(defvar lispm-ui-field-echo-seconds 4
  "Seconds a message shows in a field.")

(defvar lispm-ui-field--fields nil
  "The fields made, as (FRAME . FIELD): FRAME reads and echoes in FIELD.")

(defvar lispm-ui-field--before nil
  "The minibuffer each frame read from before it had a field: (FRAME . VALUE),
VALUE being the frame's `minibuffer' parameter then.")

(defun lispm-ui-field (frame)
  "Return the field FRAME reads and echoes in, or nil when it has none."
  (and (frame-live-p frame)
       (let ((field (alist-get frame lispm-ui-field--fields)))
         (and (frame-live-p field) field))))

(defun lispm-ui-field-p (frame)
  "Return non-nil when FRAME is a field: the minibuffer of a window drawn
inside another."
  (and (frame-live-p frame)
       (cl-some (lambda (entry) (eq (cdr entry) frame))
                lispm-ui-field--fields)
       t))

(defun lispm-ui-field--has-minibuffer-of-its-own-p (frame)
  "Return non-nil when FRAME shows a minibuffer of its own.
A frame with one cannot take a field: a field is the minibuffer a window
drawn inside another takes in place of the one it has not."
  (and (frame-live-p frame)
       (null (frame-parameter frame 'minibuffer))
       (let ((mini (minibuffer-window frame)))
         (and mini (eq (window-frame mini) frame)))))

(defun lispm-ui-field-make (frame)
  "Return FRAME's field, making it when missing, and have FRAME read from it.
FRAME is drawn inside another frame: its field is a minibuffer-only child
frame at its bottom, as wide as FRAME, which FRAME takes as its minibuffer
window.  A frame showing a minibuffer of its own, as a frame on the desktop
does, cannot take a field."
  (let ((field (lispm-ui-field frame)))
    (unless field
      (when (lispm-ui-field--has-minibuffer-of-its-own-p frame)
        (user-error "A frame with a minibuffer of its own cannot read in a field"))
      (setq field (lispm-ui-field--make-field frame))
      (setf (alist-get frame lispm-ui-field--fields) field)
      (unless (assq frame lispm-ui-field--before)
        (push (cons frame (frame-parameter frame 'minibuffer))
              lispm-ui-field--before)))
    (set-frame-parameter frame 'minibuffer (minibuffer-window field))
    (lispm-ui-field-fit field)
    field))

(defun lispm-ui-field-dispose (frame)
  "Delete FRAME's field; FRAME reads again from where it read before."
  (let ((field (lispm-ui-field frame)))
    (when field
      (setq lispm-ui-field--fields
            (assq-delete-all frame lispm-ui-field--fields))
      (let ((before (assq frame lispm-ui-field--before)))
        (when before
          (setq lispm-ui-field--before
                (assq-delete-all frame lispm-ui-field--before))
          (when (and (frame-live-p frame)
                     (eq (frame-parameter frame 'minibuffer)
                         (minibuffer-window field)))
            (if (cdr before)
                (set-frame-parameter frame 'minibuffer (cdr before))
              ;; What FRAME had was Emacs's default, as no minibuffer of its
              ;; own: reset the parameter.
              (modify-frame-parameters frame '((minibuffer)))))))
      (when (frame-visible-p field)
        (make-frame-invisible field t))
      (delete-frame field t))))

(defun lispm-ui-field--make-field (frame)
  "Return a hidden minibuffer-only child frame at the bottom of FRAME."
  (let ((field (let ((after-make-frame-functions nil)
                     (before-make-frame-hook nil))
                 (make-frame
                  `((parent-frame . ,frame) (minibuffer . only)
                    (undecorated . t) (visibility . nil)
                    ;; Keys typed in FRAME read here; shown, the field takes
                    ;; no focus from FRAME.
                    (no-accept-focus . t) (no-focus-on-map . t)
                    (no-other-frame . t) (skip-taskbar . t)
                    (desktop-dont-save . t) (unsplittable . t)
                    (menu-bar-lines . 0) (tool-bar-lines . 0) (tab-bar-lines . 0)
                    (vertical-scroll-bars . nil) (horizontal-scroll-bars . nil)
                    (left-fringe . 0) (right-fringe . 0)
                    (no-special-glyphs . t) (line-spacing . 0)
                    (internal-border-width . 1)
                    (width . (text-pixels . ,(frame-text-width frame)))
                    (height . (text-pixels . ,(frame-char-height frame))))))))
    (when-let* ((chrome (lispm-ui-color :chrome)))
      (set-face-attribute 'internal-border field :background chrome))
    field))

;;;; What a field shows

(defun lispm-ui-field--text-width (frame)
  "Return how wide FRAME's text is: its columns on a text terminal, its
pixels on a graphical display."
  (if (lispm-ui-tty-p frame)
      (frame-width frame)
    (frame-text-width frame)))

(defun lispm-ui-field--lines (field)
  "Return how many lines what FIELD shows takes, at least one."
  (when-let* ((window (minibuffer-window field))
              ((window-live-p window)))
    (with-current-buffer (window-buffer window)
      (max 1 (count-lines (point-min) (point-max))))))

(defun lispm-ui-field--pixels (field)
  "Return how many pixels tall what FIELD shows is, or nil when unknown."
  (when-let* ((window (minibuffer-window field))
              ((window-live-p window)))
    (cdr (window-text-pixel-size window nil nil 4000 4000))))

(defun lispm-ui-field-fit (field &optional lines)
  "Put FIELD at the bottom of the frame it is under, as wide as it, LINES
lines tall, or as tall as what it shows, and never taller than the frame
it is under nor than `lispm-ui-field-max-lines'."
  (when (and (frame-live-p field) (frame-live-p (frame-parent field)))
    (let ((parent (frame-parent field)))
      (if (lispm-ui-tty-p field)
          (let ((height (max 1 (min (or lines (lispm-ui-field--lines field)
                                        1)
                                    lispm-ui-field-max-lines
                                    (frame-height parent)))))
            (set-frame-width field (frame-width parent))
            (set-frame-height field height)
            (set-frame-position field 0 (- (frame-height parent) height)))
        (let* ((line (max 1 (frame-char-height field)))
               (wanted (or (and lines (* lines line))
                           (lispm-ui-field--pixels field)
                           line))
               (height (min (max line
                                 (min (* lispm-ui-field-max-lines line) wanted))
                            (max line (frame-pixel-height parent)))))
          (set-frame-width field (lispm-ui-field--text-width parent) nil t)
          (set-frame-height field height nil t)
          (set-frame-position field 0 (- (frame-pixel-height parent)
                                         (frame-pixel-height field))))))))

(defun lispm-ui-field-show (frame &optional lines)
  "Show FRAME's field, LINES lines tall, or as tall as what it shows."
  (when-let* ((field (lispm-ui-field frame)))
    (lispm-ui-field-fit field lines)
    (make-frame-visible field)
    field))

(defun lispm-ui-field-hide (frame)
  "Hide FRAME's field."
  (when-let* ((field (lispm-ui-field frame))
              ((frame-visible-p field)))
    (make-frame-invisible field t)))

(defun lispm-ui-field-hide-all ()
  "Hide every field shown."
  (dolist (entry lispm-ui-field--fields)
    (when (and (frame-live-p (cdr entry)) (frame-visible-p (cdr entry)))
      (make-frame-invisible (cdr entry) t))))

(defun lispm-ui-field-fit-current ()
  "Keep the field the minibuffer is in, when it is one, as tall as what it
shows.  This is a `post-command-hook' function, run while read from."
  (when-let* ((mini (active-minibuffer-window))
              (field (window-frame mini))
              ((lispm-ui-field-p field))
              ((frame-visible-p field)))
    (lispm-ui-field-fit field)))

;;;; Messages

(defvar lispm-ui-field--echo-timer nil
  "Timer hiding a field after a message.")

(defun lispm-ui-field--stop-echo ()
  "Let the field shown stay, as it is read from now."
  (when (timerp lispm-ui-field--echo-timer)
    (cancel-timer lispm-ui-field--echo-timer))
  (setq lispm-ui-field--echo-timer nil))

(defun lispm-ui-field--message-lines (message)
  "Return how many lines of a field MESSAGE takes."
  (1+ (min (1- lispm-ui-field-max-lines) (cl-count ?\n message))))

(defun lispm-ui-field-echo (message)
  "Show MESSAGE, meant for a frame with a field, in that field for
`lispm-ui-field-echo-seconds': Emacs shows the message itself, this makes
the field visible and as tall as it.
This is one of `set-message-functions', and returns nil so that Emacs
still shows the message."
  (let* ((asking (selected-frame))
         (mini (and (not (active-minibuffer-window))
                    (minibuffer-window asking)))
         (field (and mini (window-frame mini))))
    (when (lispm-ui-field-p field)
      (lispm-ui-field--stop-echo)
      (lispm-ui-field-fit field (lispm-ui-field--message-lines message))
      (make-frame-visible field)
      (setq lispm-ui-field--echo-timer
            (run-with-timer lispm-ui-field-echo-seconds nil
                            (lambda ()
                              (setq lispm-ui-field--echo-timer nil)
                              (when (and (frame-live-p field)
                                         (not (active-minibuffer-window)))
                                (make-frame-invisible field t))))))
    nil))

(defun lispm-ui-field-setup ()
  "Show the field of the frame asking, as tall as what it shows, and keep it
so as what it shows changes.  This is a `minibuffer-setup-hook' function,
run after Vertico's."
  (let ((field (window-frame (minibuffer-window))))
    (when (lispm-ui-field-p field)
      (lispm-ui-field--stop-echo)
      (add-hook 'post-command-hook #'lispm-ui-field-fit-current 90 t)
      (lispm-ui-field-fit field)
      (make-frame-visible field)
      ;; Once Vertico has drawn what it shows.
      (run-at-time 0 nil #'lispm-ui-field-fit-current))))

(defun lispm-ui-field-exit ()
  "Hide every field once nothing is read from a minibuffer.  This is a
`minibuffer-exit-hook' function."
  (run-at-time 0 nil
               (lambda ()
                 (when (zerop (minibuffer-depth))
                   (lispm-ui-field-hide-all)))))

;;;; Turning them on

(defun lispm-ui-field-enable ()
  "Show messages and what is read in fields: a message meant for a frame
that has one shows in it, and what is read from the frame shows there, as
tall as it is.  Call it once, in a program whose frames have fields."
  (add-hook 'set-message-functions #'lispm-ui-field-echo)
  (add-hook 'minibuffer-setup-hook #'lispm-ui-field-setup 90)
  (add-hook 'minibuffer-exit-hook #'lispm-ui-field-exit))

(defun lispm-ui-field-disable ()
  "Leave messages and reads to Emacs, as frames without fields have them."
  (remove-hook 'set-message-functions #'lispm-ui-field-echo)
  (remove-hook 'minibuffer-setup-hook #'lispm-ui-field-setup)
  (remove-hook 'minibuffer-exit-hook #'lispm-ui-field-exit))

(provide 'lispm-ui-field)
;;; lispm-ui-field.el ends here
