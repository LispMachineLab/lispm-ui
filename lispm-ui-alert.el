;;; lispm-ui-alert.el --- Alerts, drawn as Mac OS 9 drew them  -*- lexical-binding: t; -*-

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

;; An alert is a panel popped up over the frame's upper middle, as Mac OS
;; 9's were: the caution, stop or note icon, the message, and push
;; buttons at the bottom right, the default one ringed.  It is modal:
;; Return presses the default button, Escape, C-g and Command-period
;; cancel, Tab and Shift-Tab take the keyboard from button to button and
;; Space presses the one it is on; a button held with the mouse is
;; pressed where it is let go.
;;
;; `lispm-ui-alert-default' shows one, as `lispm-ui-alert' asks it to;
;; on a text terminal it is text, see `lispm-ui-alert--text', and where
;; no popup can be shown it is asked in the minibuffer.  A look draws
;; alerts its own way with methods of `lispm-ui-draw-alert' and
;; `lispm-ui-alert-layout'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'lispm-ui-base)
(require 'lispm-ui-palette)
(require 'lispm-ui-text)
(require 'lispm-ui-draw)
(require 'lispm-ui-raster)
(require 'lispm-ui-icons)
(require 'lispm-ui-controls)
(require 'lispm-ui-modal)
(require 'lispm-ui-ask)
(require 'lispm-ui-popup)

(defconst lispm-ui-alert-width 364
  "Width in pixels of an alert inside its outline, Mac OS 9's.")

(defconst lispm-ui-alert-text-x 79
  "Pixels from an alert's inside left edge to its message: 80 from its outline.")

(defconst lispm-ui-alert-button-gap 10
  "Pixels between neighbouring buttons of an alert, ring to outline.")

(defun lispm-ui-alert--wrap (text width face)
  "Return TEXT broken into lines no wider than WIDTH pixels in FACE.
Lines break between words, or between the characters of Chinese and
Japanese; a word wider than WIDTH stands alone."
  (lispm-ui-text-wrap
   text (lambda (line) (<= (lispm-ui-text-width line face) width))))

(cl-defgeneric lispm-ui-alert-layout (message detail buttons)
  "Return where an alert of MESSAGE, DETAIL and BUTTONS draws its parts.
The value is a plist: :height of the alert inside its outline, and its
:width where it is not `lispm-ui-alert-width',
:message and :detail as lists of lines, and :buttons as (LABEL VALUE
ROLE X Y WIDTH).  Laid out as Mac OS 9 laid out its alerts: the caution,
stop or note icon at the top left, the message to its right in the
system font, the detail under it; the default button at the bottom
right, Cancel to its left, and a button such as Don't Save, which comes
before Cancel, standing apart at the left, under the message.  Where
the buttons' words are too long for Mac OS 9's width, as in a longer
language, the alert is just wide enough for those standing apart to
stay clear of the others."
  (let* ((layout (lispm-ui-alert--layout-at lispm-ui-alert-width message detail buttons))
         (overlap (plist-get layout :overlap)))
    (if (<= overlap 0)
        layout
      (append (list :width (+ lispm-ui-alert-width overlap))
              (lispm-ui-alert--layout-at (+ lispm-ui-alert-width overlap)
                                         message detail buttons)))))

(defun lispm-ui-alert--layout-at (w message detail buttons)
  "Return the layout of an alert W pixels wide, as
`lispm-ui-alert-layout' does, with :overlap, the pixels its
buttons standing apart come too close to the others, if any."
  (let* ((text-width (- w lispm-ui-alert-text-x 20))
         (lines (lispm-ui-alert--wrap message text-width 'lispm-ui-title))
         (details (and detail (lispm-ui-alert--wrap detail text-width 'lispm-ui-list)))
         (text-bottom (+ 15 (* 16 (length lines)) (if details (+ 6 (* 16 (length details))) 0)))
         (top (+ (max text-bottom 47) 29))
         (cancel (seq-position buttons 'cancel (lambda (button role) (eq (nth 2 button) role))))
         (apart (lambda (index) (and cancel (< index cancel) (null (nth 2 (nth index buttons))))))
         (placed nil)
         (right-start w)
         (apart-end 0))
    ;; The buttons at the right, from the right: the default's ring three
    ;; pixels about it and twelve from the edge.
    (let ((right (- w 13)) (previous nil))
      (dolist (index (number-sequence (1- (length buttons)) 0 -1))
        (unless (funcall apart index)
          (pcase-let* ((`(,label ,value ,role) (nth index buttons))
                       (width (lispm-ui-push-button-width label))
                       (ring (if (eq role 'default) 3 0)))
            (when previous
              (setq right (- right lispm-ui-alert-button-gap 1)))
            (push (list label value role (- right ring width -1) top width) placed)
            (setq right-start (min right-start (- right ring width -1 ring)))
            (setq right (- right ring width -1 ring) previous t)))))
    ;; Those standing apart, from the message's left.
    (let ((left (- lispm-ui-alert-text-x 4)))
      (seq-do-indexed
       (lambda (button index)
         (when (funcall apart index)
           (pcase-let* ((`(,label ,value ,role) button)
                        (width (lispm-ui-push-button-width label)))
             (push (list label value role left top width) placed)
             (setq apart-end (+ left width)
                   left (+ left width lispm-ui-alert-button-gap)))))
       buttons))
    (list :height (+ top lispm-ui-button-height 15)
          ;; Those apart keep twice the gap between buttons from Cancel.
          :overlap (if (> apart-end 0) (- (+ apart-end (* 2 lispm-ui-alert-button-gap)) right-start) 0)
          :message lines :detail details
          :buttons (seq-sort-by (lambda (button) (seq-position buttons (nth 1 button)
                                                               (lambda (b value) (equal (nth 1 b) value))))
                                #'< placed))))

(defun lispm-ui-svg-alert-contents (svg layout x y palette pressed focused &optional detail-color)
  "Draw on SVG what LAYOUT places, see `lispm-ui-alert-layout':
the message from X, Y in the system font, the detail six pixels under it
in the list's, in DETAIL-COLOR, by default PALETTE's text colour, and the
buttons.  PRESSED and FOCUSED are values of buttons, held down or
holding the keyboard."
  (dolist (line (plist-get layout :message))
    (lispm-ui-svg-content svg line x y 16 (plist-get palette :text) 'lispm-ui-title)
    (setq y (+ y 16)))
  (when (plist-get layout :detail)
    (setq y (+ y 6))
    (dolist (line (plist-get layout :detail))
      (lispm-ui-svg-content svg line x y 16 (or detail-color (plist-get palette :text))
                            'lispm-ui-list)
      (setq y (+ y 16))))
  (pcase-dolist (`(,label ,value ,role ,x ,top ,width) (plist-get layout :buttons))
    (lispm-ui-svg-push-button svg x top palette label width
                              :state (and (equal pressed value) 'pressed)
                              :default (eq role 'default)
                              :focused (and focused (equal focused value)))))

(cl-defgeneric lispm-ui-draw-alert (palette icon message detail buttons pressed focused)
  "Return an SVG of an alert of ICON, MESSAGE, DETAIL and BUTTONS, in PALETTE.
It is drawn as Mac OS 9 drew its alerts, inside the outline the alert's
frame draws: a light red ring lit along the top and left and darker along
the bottom and right, then a white line and a grey one, round a grey
body; the parts placed by `lispm-ui-alert-layout'.  PRESSED is the VALUE
of the button held down, FOCUSED that of the one the keyboard is on."
  (let* ((layout (lispm-ui-alert-layout message detail buttons))
         (w (or (plist-get layout :width) lispm-ui-alert-width))
         (h (plist-get layout :height))
         (svg (svg-create w h))
         (grey (lambda (level) (lispm-ui-grey palette level)))
         (y 15))
    (lispm-ui-svg-fill svg 0 0 w h (plist-get palette :alert-dark))
    (lispm-ui-svg-fill svg 0 0 (1- w) (1- h) (plist-get palette :alert))
    (lispm-ui-svg-fill svg 1 1 (- w 2) (- h 2) (funcall grey #x99))
    (lispm-ui-svg-fill svg 1 1 (- w 3) (- h 3) (funcall grey #xFF))
    (lispm-ui-svg-fill svg 2 2 (- w 4) (- h 4) (funcall grey #xDD))
    (lispm-ui-svg-icon svg (lispm-ui-icon-file icon) 25 15 32)
    (lispm-ui-svg-alert-contents svg layout lispm-ui-alert-text-x y palette
                                 pressed focused)
    svg))

(defconst lispm-ui-text-alert-width 64
  "Width in cells of an alert on a text terminal, inside its outline.")

(defconst lispm-ui-alert--text-icon-size 16
  "Size in pixels of an alert's icon on a text terminal, 2 by 4 pixels a
cell.")

(defconst lispm-ui-alert--text-x 13
  "Cells from an alert's inside left edge to its message, on a text terminal.")

(defun lispm-ui-alert--text-button-width (label)
  "Return the width in cells of a push button reading LABEL on a text terminal."
  (string-width (lispm-ui-text-button (concat " " label " "))))

(defun lispm-ui-alert--text-layout (message detail buttons)
  "Return where an alert of MESSAGE, DETAIL and BUTTONS draws its parts on a
text terminal, in cells, as `lispm-ui-alert-layout' does:
the icon at the top left, the message beside it, the detail a line under
it, and the buttons on the last line but one, the default at the right,
Cancel to its left, and a button such as Don't Save, which comes before
Cancel, standing apart under the message.  The alert widens where the
buttons would not fit."
  (let* ((cancel (seq-position buttons 'cancel (lambda (button role) (eq (nth 2 button) role))))
         (apart (lambda (index) (and cancel (< index cancel) (null (nth 2 (nth index buttons))))))
         (x lispm-ui-alert--text-x)
         (widths (mapcar (lambda (button) (lispm-ui-alert--text-button-width (car button))) buttons))
         (needed (+ x (apply #'+ (mapcar (lambda (w) (+ w 2)) widths)) 2))
         (w (max lispm-ui-text-alert-width needed))
         (text-width (- w x 2))
         (lines (lispm-ui-alert--wrap message text-width 'lispm-ui-title))
         (details (and detail (lispm-ui-alert--wrap detail text-width 'lispm-ui-list)))
         ;; Under the message and the detail, and under the icon.
         (top (+ 1 (max (+ (length lines) (if details (1+ (length details)) 0))
                        (cdr (lispm-ui-text-size lispm-ui-alert--text-icon-size)))
                 1))
         (placed nil))
    (let ((right (- w 2)))
      (dolist (index (number-sequence (1- (length buttons)) 0 -1))
        (unless (funcall apart index)
          (pcase-let ((`(,label ,value ,role) (nth index buttons))
                      (width (nth index widths)))
            (setq right (- right width))
            (push (list label value role right top width) placed)
            (setq right (- right 2))))))
    (let ((left x))
      (seq-do-indexed (lambda (button index)
                        (when (funcall apart index)
                          (pcase-let ((`(,label ,value ,role) button))
                            (push (list label value role left top (nth index widths)) placed)
                            (setq left (+ left (nth index widths) 2)))))
                      buttons))
    (list :width w :height (+ top 2) :message lines :detail details
          :buttons (seq-sort-by (lambda (button) (seq-position buttons (nth 1 button)
                                                               (lambda (b value) (equal (nth 1 b) value))))
                                #'< placed))))

(defun lispm-ui-alert--text (icon message detail buttons pressed focused)
  "Return an alert of ICON, MESSAGE, DETAIL and BUTTONS on a text terminal,
as text laid out by `lispm-ui-alert--text-layout'.  The icon
is a mark in the alert colour; the button the keyboard is on is
underlined; PRESSED and FOCUSED are as `lispm-ui-draw-alert'
takes them."
  (let* ((layout (lispm-ui-alert--text-layout message detail buttons))
         (w (plist-get layout :width))
         (x lispm-ui-alert--text-x)
         (rows (make-vector (plist-get layout :height) nil))
         (row 1))
    (cl-flet ((put (row column string)
                (aset rows row (cons (cons column string) (aref rows row)))))
      (if-let* ((picture (or (when lispm-ui-tty-image-function
                               (let ((size (lispm-ui-text-size lispm-ui-alert--text-icon-size)))
                                 (funcall lispm-ui-tty-image-function 'icon (symbol-name icon) nil
                                          (car size) (cdr size))))
                             (lispm-ui-icon-picture
                              (symbol-name icon) lispm-ui-alert--text-icon-size nil
                              (lispm-ui-color :chrome)))))
          (seq-do-indexed (lambda (line index) (put (1+ index) 3 line)) picture)
        (put 1 3 (propertize (concat " " (pcase icon ('stop "×") ('note "i") (_ "!")) " ")
                             'face `(:foreground ,(lispm-ui-color :highlight-text)
                                                 :background ,(lispm-ui-color :alert-dark)
                                                 :weight bold))))
      (dolist (line (plist-get layout :message))
        (put row x (propertize line 'face 'lispm-ui-title))
        (setq row (1+ row)))
      (when (plist-get layout :detail)
        (setq row (1+ row))
        (dolist (line (plist-get layout :detail))
          (put row x line)
          (setq row (1+ row))))
      (pcase-dolist (`(,label ,value ,role ,left ,top ,_) (plist-get layout :buttons))
        (let ((button (lispm-ui-text-button
                       (concat " " label " ") (and (equal pressed value) 'pressed) (eq role 'default))))
          (when (and focused (equal focused value))
            (add-face-text-property 0 (length button) '(:underline t) t button))
          (put top left button))))
    (mapconcat (lambda (parts)
                 (let ((line "") (column 0))
                   (pcase-dolist (`(,at . ,string) (sort parts (lambda (a b) (< (car a) (car b)))))
                     (setq line (concat line (make-string (max 0 (- at column)) ?\s) string)
                           column (+ (max at column) (string-width string))))
                   (concat line (make-string (max 0 (- w column)) ?\s))))
               rows "\n")))

(defun lispm-ui-alert--layout (message detail buttons)
  "Return the layout of an alert of MESSAGE, DETAIL and BUTTONS: in pixels,
see `lispm-ui-alert-layout', or in cells on a text terminal,
see `lispm-ui-alert--text-layout'."
  (if (lispm-ui-tty-p)
      (lispm-ui-alert--text-layout message detail buttons)
    (lispm-ui-alert-layout message detail buttons)))

(defun lispm-ui-alert--draw (frame icon message detail buttons pressed focused)
  "Show the alert of ICON, MESSAGE, DETAIL and BUTTONS in popup FRAME.
PRESSED and FOCUSED are as `lispm-ui-draw-alert' takes them.
The buttons' places are kept in FRAME's `lispm-ui-popup-buttons'.
On a text terminal the alert is text, see `lispm-ui-alert--text'."
  (lispm-ui-popup-fill
   frame (if (lispm-ui-tty-p frame)
             (lispm-ui-alert--text icon message detail buttons pressed focused)
           (propertize " " 'display
                       (svg-image (lispm-ui-draw-alert lispm-ui-current-palette icon message
                                                       detail buttons pressed focused)
                                  :ascent 100 :scale 1))))
  ;; The icon as an image where the terminal shows it.
  (when (and (lispm-ui-tty-p frame) lispm-ui-tty-image-show-function)
    (funcall lispm-ui-tty-image-show-function (frame-root-window frame)))
  (set-frame-parameter frame 'lispm-ui-popup-buttons
                       (plist-get (lispm-ui-alert--layout message detail buttons) :buttons)))

(defun lispm-ui-alert--button-at (event frame)
  "Return the value of the alert button under EVENT in FRAME, or nil."
  (when-let* ((position (lispm-ui-modal-over event frame))
              (xy (posn-x-y position)))
    (lispm-ui-modal-hit (mapcar (pcase-lambda (`(,_ ,value ,_ ,left ,top ,width))
                                  (list value left top width
                                        (if (lispm-ui-tty-p frame) 1
                                          lispm-ui-button-height)))
                                (frame-parameter frame 'lispm-ui-popup-buttons))
                        (car xy) (cdr xy))))

(defun lispm-ui-alert-open (icon message buttons &optional detail)
  "Show an alert of ICON, MESSAGE, BUTTONS and DETAIL.
See `lispm-ui-alert-default'.  It sits over the upper middle of the
selected frame, as Mac OS 9's alerts did, outlined and casting a black
shadow.  Return its frame; nothing is read from the user."
  (let* ((parent (selected-frame))
         (popup (lispm-ui-popup-make-frame parent))
         (layout (lispm-ui-alert--layout message detail buttons))
         (width (or (plist-get layout :width) lispm-ui-alert-width)))
    (lispm-ui-alert--draw popup icon message detail buttons nil nil)
    (lispm-ui-popup-set-size popup width (plist-get layout :height))
    (lispm-ui-popup-show popup (/ (- (frame-pixel-width parent) width) 2)
                         (/ (frame-pixel-height parent) 5)
                         'alert)
    popup))

(defun lispm-ui-alert-default (icon message buttons &optional detail)
  "Show a Mac OS 9 alert and return the value of the button chosen.
ICON is `caution', `stop' or `note'; MESSAGE is the question, DETAIL a
line or two under it.  BUTTONS is a list of (LABEL VALUE ROLE KEYS...):
ROLE `default' marks the button Return presses, `cancel' the one Escape
does, and without one Escape dismisses the alert by its default; KEYS
are events pressing a button too, as Command-D pressed Don't Save.
Where no popup can be drawn, see `lispm-ui-popup-available-p', it is
asked in the minibuffer, a letter a button.  This is how
`lispm-ui-alert' shows an alert, unless `lispm-ui-alert-function' says
otherwise."
  (let ((default (nth 1 (seq-find (lambda (button) (eq (nth 2 button) 'default)) buttons)))
        (cancel (nth 1 (seq-find (lambda (button) (eq (nth 2 button) 'cancel)) buttons))))
    (if (not (lispm-ui-popup-available-p))
        (lispm-ui--alert-in-minibuffer message buttons detail cancel)
      (let* ((popup (lispm-ui-alert-open icon message buttons detail))
             (values (mapcar #'cadr buttons))
             (pressed nil) (held nil) (focused nil) (choice cancel))
        (unwind-protect
            (cl-flet* ((redraw ()
                         (lispm-ui-alert--draw popup icon message detail buttons pressed focused)
                         (redisplay t))
                       (press (value)
                         ;; A button pressed from the keyboard blinks, as
                         ;; Mac OS 9's did, and is chosen.
                         (lispm-ui-modal-flash 'button (lambda () (setq pressed value) (redraw)))
                         (setq choice value) (throw 'done nil)))
              (track-mouse
                (catch 'done
                  (while t
                    (let* ((event (lispm-ui-modal-read))
                           (role (lispm-ui-modal-role event))
                           (step (lispm-ui-modal-step event)))
                      (cond
                       ((seq-find (lambda (button) (memql event (nthcdr 3 button))) buttons)
                        (press (nth 1 (seq-find (lambda (button) (memql event (nthcdr 3 button)))
                                                buttons))))
                       ((and (eq role 'cancel) (or cancel default))
                        ;; An alert with no Cancel, only OK, is dismissed.
                        (setq choice (or cancel default)) (throw 'done nil))
                       ((and (eq role 'confirm) default)
                        (press default))
                       (step
                        ;; Tab goes from no button to the first, as from the
                        ;; message to the controls.
                        (setq focused (nth (lispm-ui-modal-cycle (seq-position values focused) step
                                                                 (length values))
                                           values))
                        (redraw))
                       ((and focused (eq role 'press))
                        ;; Space presses the button the keyboard is on.
                        (press focused))
                       ((lispm-ui-modal-mouse-down-p event)
                        (when (setq held (lispm-ui-alert--button-at event popup))
                          (setq pressed held) (redraw)))
                       ((and held (mouse-movement-p event))
                        (let ((over (equal (lispm-ui-alert--button-at event popup) held)))
                          (unless (eq over (and pressed t))
                            (setq pressed (and over held)) (redraw))))
                       ((lispm-ui-modal-mouse-up-p event)
                        (when (and held (equal (lispm-ui-alert--button-at event popup) held))
                          (setq choice held) (throw 'done nil))
                        (setq held nil pressed nil) (redraw))))))))
          (lispm-ui-popup-dispose popup))
        choice))))

(provide 'lispm-ui-alert)
;;; lispm-ui-alert.el ends here
