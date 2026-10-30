;;; lispm-ui-controls.el --- LispM UI's buttons, menus and boxes  -*- lexical-binding: t; -*-

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

;; The controls LispM UI draws, as Mac OS 9 drew them, in the colours of
;; the palette (lispm-ui-palette.el): push buttons and the ring of the
;; default one, pop-up menus, bevel buttons, radio buttons, check boxes,
;; group boxes and the keyboard's focus ring.  Each is drawn as SVG by a
;; generic function, `lispm-ui-draw-push-button' and the like, whose
;; default method draws the classic look; a look draws its own in a
;; method of its own (lispm-ui-look.el).
;;
;; Each is also a string displaying it, memoized for the palette, to put
;; on a line, `lispm-ui-push-button' and the like, which on a text
;; terminal is the control in text; and a button inserted in a buffer,
;; `lispm-ui-insert-push-button' and the like, which the keyboard works:
;; Tab and Shift-Tab go from one to the next, Return and Space press the
;; one point is on, which is drawn ringed (`lispm-ui-show-focus').

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'dom)
(require 'button)
(require 'lispm-ui-base)
(require 'lispm-ui-palette)
(require 'lispm-ui-text)
(require 'lispm-ui-draw)
(require 'lispm-ui-modal)
(require 'lispm-ui-ask)

;;;; Group boxes

(defconst lispm-ui-group-title-height 18
  "Height in pixels of a group box's top edge, which carries its title.")

(cl-defgeneric lispm-ui-draw-group-edge (palette width &optional title bottom)
  "Return an SVG of a group box's top edge WIDTH pixels wide, in PALETTE.
Mac OS 9 framed a group of controls in an engraved line, a grey line
with a lit one along it, TITLE breaking the top line near its left end
in the system font.  BOTTOM draws the bottom edge instead, closing the
frame's sides at the top of a line as tall as a line of text."
  (let* ((h (if bottom (default-line-height) lispm-ui-group-title-height))
         (y (if bottom 1 (/ h 2)))
         (svg (svg-create width h))
         (dark (lispm-ui-grey palette #x88))
         (lit (lispm-ui-grey palette #xFF)))
    (if bottom
        (progn
          (lispm-ui-svg-column svg 0 0 y dark)
          (lispm-ui-svg-column svg 1 0 (1- y) lit)
          (lispm-ui-svg-column svg (- width 2) 0 y dark)
          (lispm-ui-svg-column svg (1- width) 0 (1+ y) lit))
      (lispm-ui-svg-column svg 0 y (1- h) dark)
      (lispm-ui-svg-column svg 1 (1+ y) (1- h) lit)
      (lispm-ui-svg-column svg (- width 2) y (1- h) dark)
      (lispm-ui-svg-column svg (1- width) y (1- h) lit))
    (lispm-ui-svg-row svg 0 (- width 2) y dark)
    (lispm-ui-svg-row svg 1 (1- width) (1+ y) lit)
    (when title
      (let ((title-width (lispm-ui-text-width title 'lispm-ui-title)))
        (lispm-ui-svg-fill svg 8 0 (+ title-width 8) h (lispm-ui-grey palette #xEE))
        (lispm-ui-svg-content svg title 12 0 h (plist-get palette :text)
                              'lispm-ui-title)))
    svg))

(defun lispm-ui-group-edge (width &optional title bottom)
  "Return a group box's edge WIDTH pixels wide as a string displaying it.
See `lispm-ui-draw-group-edge'.  On a text terminal it is a
line drawn in text, WIDTH cells long, TITLE breaking it near its left end."
  (if (lispm-ui-tty-p)
      (let ((rule `(:foreground ,(lispm-ui-color :muted))))
        (if bottom
            (propertize (concat "└" (make-string (max 0 (- width 2)) ?─) "┘") 'face rule)
          (let ((title (if title (concat " " title " ") "")))
            (concat (propertize "┌─" 'face rule)
                    (propertize title 'face 'lispm-ui-title)
                    (propertize (concat (make-string (max 0 (- width 3 (string-width title))) ?─) "┐")
                                'face rule)))))
    (propertize " " 'display
                (lispm-ui-image
                 (list 'group-edge width title bottom)
                 (lambda () (lispm-ui-draw-group-edge lispm-ui-current-palette
                                                      width title bottom))))))

(cl-defgeneric lispm-ui-draw-group-side (palette height)
  "Return an SVG of a group box's side for a line HEIGHT pixels tall, in
PALETTE: the grey line and the lit one beside it, two pixels wide."
  (let ((svg (svg-create 2 height)))
    (lispm-ui-svg-fill svg 0 0 1 height (lispm-ui-grey palette #x88))
    (lispm-ui-svg-fill svg 1 0 1 height (lispm-ui-grey palette #xFF))
    svg))

(defun lispm-ui-group-side (height)
  "Return a group box's side for a line HEIGHT pixels tall, as a string
displaying it; see `lispm-ui-draw-group-side'.  On a text
terminal it is a line drawn in text."
  (if (lispm-ui-tty-p)
      (propertize "│" 'face `(:foreground ,(lispm-ui-color :muted)))
    (propertize " " 'display
                (lispm-ui-image
                 (list 'group-side height)
                 (lambda () (lispm-ui-draw-group-side lispm-ui-current-palette
                                                      height))))))

;;;; Controls (pure)
;;
;; Mac OS 9's controls as its Appearance Manager drew them, measured
;; pixel by pixel on Mac OS 9.0.4: push buttons and the ring of the
;; default one, pop-up menus, bevel buttons, radio buttons, check boxes
;; and focus rings.  Each draws with `lispm-ui-grey', so the classic
;; greys come out as themselves and another theme gets their
;; counterparts.  Labels are SVG text in the system font, as heavy as the
;; titles and menus Emacs draws (`lispm-ui-svg-font').

(defconst lispm-ui-button-height 20
  "Height in pixels of a push button, its outline included.")

(defconst lispm-ui-bevel-padding 8
  "Pixels between a bevel button's sides and its label.")

(defun lispm-ui-svg-pixel-map (svg x y rows colors)
  "Draw ROWS, strings of pixels, on SVG from X, Y.
COLORS maps each character to its colour; a character it leaves out is
left clear."
  (seq-do-indexed
   (lambda (row dy)
     (seq-do-indexed
      (lambda (pixel dx)
        (when-let* ((color (alist-get pixel colors)))
          (lispm-ui-svg-dot svg (+ x dx) (+ y dy) color)))
      row))
   rows))

;;;;; Push buttons

(defun lispm-ui-push-button-colors (palette state)
  "Return the colours of a push button's parts in STATE, as a plist.
STATE is nil, `pressed' or `disabled'.  The parts are its :outline, the
:edge pixels rounding its corners, the outer and inner lines lit along
its top and left (:lit, :lit-inner), its :face, the inner and outer
lines shaded along its bottom and right (:shade-inner, :shade), the
pixels where lit and shaded meet (:corner, :corner-low, :corner-inner)
and its :label."
  (cl-flet ((g (level) (lispm-ui-grey palette level)))
    (pcase state
      ('pressed
       (list :outline (plist-get palette :outline) :edge (g #x22)
             :lit (g #x44) :lit-inner (g #x55) :face (g #x66)
             :shade-inner (g #x77) :shade (g #x88)
             :corner (g #x44) :corner-low (g #x77) :corner-inner (g #x44)
             :label (lispm-ui-legible (g #x66) (plist-get palette :text) (g #xFF))))
      ('disabled
       (let ((face (g #xDD)))
         (list :outline (g #x88) :edge (g #x88)
               :lit face :lit-inner face :face face :shade-inner face :shade face
               :corner face :corner-low face :corner-inner face
               :label (g #x77))))
      (_
       (list :outline (plist-get palette :outline) :edge (g #x22)
             :lit (g #xDD) :lit-inner (g #xFF) :face (g #xDD)
             :shade-inner (g #xAA) :shade (g #x77)
             :corner (g #xBB) :corner-low (g #xBB) :corner-inner (g #xFF)
             :label (plist-get palette :text))))))

(defun lispm-ui--draw-push-frame (svg x y width colors)
  "Draw a push button WIDTH pixels wide from X, Y of SVG, without its label.
COLORS is a value of `lispm-ui-push-button-colors'.  The
outline is rounded by stepping in a pixel on each of its first two rows;
inside it, two lines are lit along the top and left and two shaded along
the bottom and right.  The pixels its corners cut off are left clear."
  (let ((w width)
        (h lispm-ui-button-height))
    (cl-flet ((dot (dx dy part)
                (lispm-ui-svg-dot svg (+ x dx) (+ y dy) (plist-get colors part)))
              (row (from to dy part)
                (lispm-ui-svg-row svg (+ x from) (+ x to) (+ y dy) (plist-get colors part)))
              (column (dx from to part)
                (lispm-ui-svg-column svg (+ x dx) (+ y from) (+ y to) (plist-get colors part))))
      (lispm-ui-svg-fill svg (+ x 1) (+ y 1) (- w 2) (- h 2) (plist-get colors :face))
      ;; The outline.
      (row 3 (- w 4) 0 :outline)
      (row 3 (- w 4) (- h 1) :outline)
      (column 0 3 (- h 4) :outline)
      (column (- w 1) 3 (- h 4) :outline)
      (pcase-dolist (`(,dx . ,dy) `((1 . 1) (,(- w 2) . 1) (1 . ,(- h 2)) (,(- w 2) . ,(- h 2))))
        (dot dx dy :outline))
      (pcase-dolist (`(,dx . ,dy) `((2 . 0) (,(- w 3) . 0) (0 . 2) (,(- w 1) . 2)
                                    (0 . ,(- h 3)) (,(- w 1) . ,(- h 3))
                                    (2 . ,(- h 1)) (,(- w 3) . ,(- h 1))))
        (dot dx dy :edge))
      ;; Lit along the top and left.
      (row 3 (- w 4) 1 :lit)
      (row 3 (- w 4) 2 :lit-inner)
      (column 1 3 (- h 4) :lit)
      (column 2 3 (- h 4) :lit-inner)
      (dot 3 3 :lit-inner)
      ;; Shaded along the bottom and right.
      (column (- w 3) 3 (- h 4) :shade-inner)
      (column (- w 2) 3 (- h 4) :shade)
      (dot (- w 4) (- h 4) :shade-inner)
      (row 3 (- w 4) (- h 3) :shade-inner)
      (row 3 (- w 3) (- h 2) :shade)
      (dot (- w 3) (- h 3) :shade)
      (dot (- w 2) (- h 3) :shade)
      ;; Where lit meets shaded.
      (dot 2 1 :corner)
      (dot (- w 3) 1 :corner)
      (dot 1 2 :corner)
      (dot 1 (- h 3) :corner)
      (dot 2 2 :corner-inner)
      (dot (- w 2) 2 :corner-low)
      (dot 2 (- h 2) :corner-low)
      (dot (- w 3) 2 :face)
      (dot 2 (- h 3) :face))))

(defun lispm-ui--draw-default-ring (svg x y width palette &optional disabled)
  "Draw the ring of a default push button WIDTH pixels wide, from X, Y of SVG.
The ring stands three pixels about the button: an outline rounded at its
corners, then a band lit along the top and left and shaded along the
bottom and right, which shows too where the button's corners are cut.
A DISABLED button's ring is a flat grey outline."
  (let* ((w (+ width 6))
         (h (+ lispm-ui-button-height 6))
         (grey (lambda (level) (lispm-ui-grey palette level)))
         (outline (if disabled (funcall grey #x88) (plist-get palette :outline)))
         (edge (if disabled outline (funcall grey #x22)))
         (lit (funcall grey #xDD))
         (lit-inner (if disabled lit (funcall grey #xAA)))
         (shade-inner (if disabled lit (funcall grey #xAA)))
         (shade (if disabled lit (funcall grey #x77)))
         (meet (if disabled lit (funcall grey #x88))))
    (cl-flet ((dot (dx dy color) (lispm-ui-svg-dot svg (+ x dx) (+ y dy) color))
              (row (from to dy color)
                (lispm-ui-svg-row svg (+ x from) (+ x to) (+ y dy) color))
              (column (dx from to color)
                (lispm-ui-svg-column svg (+ x dx) (+ y from) (+ y to) color)))
      ;; The band.
      (row 3 (- w 4) 1 lit)
      (row 2 3 2 lit)
      (row 4 (- w 5) 2 lit-inner)
      (column 1 3 (- h 4) lit)
      (column 2 3 3 lit)
      (column 2 4 (- h 5) lit-inner)
      (column (- w 3) 4 (- h 5) shade-inner)
      (column (- w 2) 3 (- h 4) shade)
      (row 4 (- w 5) (- h 3) shade-inner)
      (row 3 (- w 4) (- h 2) shade)
      (dot (- w 3) (- h 4) meet)
      (dot (- w 4) (- h 3) meet)
      (dot (- w 3) (- h 3) shade)
      ;; The corners the button's outline cuts off.
      (pcase-dolist (`(,cx ,cy ,sx ,sy) `((3 3 1 1) (,(- w 4) 3 -1 1)
                                          (3 ,(- h 4) 1 -1) (,(- w 4) ,(- h 4) -1 -1)))
        (dot cx cy shade-inner)
        (dot (+ cx sx) cy shade)
        (dot cx (+ cy sy) shade))
      ;; The outline.
      (row 4 (- w 5) 0 outline)
      (row 4 (- w 5) (- h 1) outline)
      (column 0 4 (- h 5) outline)
      (column (- w 1) 4 (- h 5) outline)
      (pcase-dolist (`(,dx . ,dy) `((2 . 1) (1 . 2) (,(- w 3) . 1) (,(- w 2) . 2)
                                    (1 . ,(- h 3)) (2 . ,(- h 2))
                                    (,(- w 2) . ,(- h 3)) (,(- w 3) . ,(- h 2))))
        (dot dx dy outline))
      (pcase-dolist (`(,dx . ,dy) `((3 . 0) (,(- w 4) . 0) (0 . 3) (,(- w 1) . 3)
                                    (0 . ,(- h 4)) (,(- w 1) . ,(- h 4))
                                    (3 . ,(- h 1)) (,(- w 4) . ,(- h 1))))
        (dot dx dy edge)))))

(cl-defgeneric lispm-ui-svg-focus-ring (svg x y width height radius palette)
  "Draw on SVG the keyboard focus ring about the WIDTH by HEIGHT box at X, Y.
The ring is two pixels in PALETTE's focus colour just outside the box,
its corners rounded by RADIUS, as Mac OS 9 ringed the control the
keyboard was on."
  (svg-rectangle svg (- x 1) (- y 1) (+ width 2) (+ height 2)
                 :rx (+ radius 1) :fill "none"
                 :stroke (plist-get palette :focus) :stroke-width 2))

(cl-defgeneric lispm-ui-push-button-width (label)
  "Return the width of a push button reading LABEL: room for it, at least 60.
LABEL is measured in the titles' face, the system font."
  (max 60 (+ 24 (string-pixel-width (propertize label 'face 'lispm-ui-title)))))

(cl-defun lispm-ui-draw-push-button (palette label width
                                             &key state default focused (pad 0) font)
  "Return an SVG of a push button WIDTH pixels wide reading LABEL, in PALETTE.
The button is `lispm-ui-button-height' tall, drawn as Mac
OS 9 drew it: rounded, outlined, lit along the top and left, its label
centred in the system font.  STATE is nil; `pressed', held down, dark
and lit from below, its label light; or `disabled', outlined in grey,
flat, its label dimmed.

DEFAULT rings the button, as the one Return presses; the ring takes
three pixels about it.  FOCUSED rings it, or its default ring, in the
focus colour, two pixels more, as the button Space presses: LispM's own
addition, for the keyboard.  PAD is the room left clear about the
button, raised to what its rings need, so buttons drawn with the same
PAD keep their places as the focus moves.  FONT is a value of
`lispm-ui-svg-font', by default the system font's."
  (let* ((ring (if default 3 0))
         (pad (max pad (+ ring (if focused 2 0))))
         (h lispm-ui-button-height)
         (svg (svg-create (+ width (* 2 pad)) (+ h (* 2 pad)))))
    (lispm-ui-svg-push-button svg pad pad palette label width
                              :state state :default default :focused focused :font font)
    svg))

(cl-defgeneric lispm-ui-svg-push-button (svg x y palette label width
                                             &key state default focused font)
  "Draw on SVG at X, Y a push button WIDTH pixels wide reading LABEL.
Its rings, when DEFAULT or FOCUSED, are drawn about it; see
`lispm-ui-draw-push-button' for the rest."
  (let ((ring (if default 3 0))
        (h lispm-ui-button-height)
        (colors (lispm-ui-push-button-colors palette state)))
    (when focused
      (lispm-ui-svg-focus-ring svg (- x ring) (- y ring)
                               (+ width (* 2 ring)) (+ h (* 2 ring))
                               (+ 3 ring) palette))
    (when default
      (lispm-ui--draw-default-ring svg (- x 3) (- y 3) width palette
                                   (eq state 'disabled)))
    (lispm-ui--draw-push-frame svg x y width colors)
    (lispm-ui-svg-label svg label (+ x (/ width 2.0)) (+ y 14)
                        (plist-get colors :label)
                        (or font (lispm-ui-svg-font)))))

;;;;; Pop-up menus

(defun lispm-ui-popup-button-width (label)
  "Return the width of a pop-up menu showing LABEL: room for it and its
arrows, at least 80.  LABEL is measured in the titles' face."
  (max 80 (+ 40 (string-pixel-width (propertize label 'face 'lispm-ui-title)))))

(cl-defgeneric lispm-ui-svg-popup-arrows (svg x y palette color)
  "Draw on SVG the arrows of a pop-up menu in the 12 by 20 pixel box at X, Y,
in COLOR, PALETTE's: a triangle up over a triangle down, as Mac OS 9 drew
them, for a menu that opens about its current item."
  (ignore palette)
  (svg-polygon svg (list (cons (+ x 2) (+ y 8)) (cons (+ x 10) (+ y 8)) (cons (+ x 6) (+ y 4)))
               :fill color)
  (svg-polygon svg (list (cons (+ x 2) (+ y 11)) (cons (+ x 10) (+ y 11)) (cons (+ x 6) (+ y 15)))
               :fill color))

(cl-defun lispm-ui-draw-popup-button (palette label width &key state focused (pad 0))
  "Return an SVG of a pop-up menu WIDTH pixels wide showing LABEL, in PALETTE.
It is a push button, as the look draws one, with LABEL on its left and
the menu's arrows on its right, past a line.  STATE is nil, `pressed' or
`disabled'; FOCUSED rings it in the focus colour; PAD is the room left
about it, raised to what the ring needs."
  (let* ((pad (max pad (if focused 2 0)))
         (h lispm-ui-button-height)
         (svg (svg-create (+ width (* 2 pad)) (+ h (* 2 pad))))
         (colors (lispm-ui-push-button-colors palette state)))
    (lispm-ui-svg-push-button svg pad pad palette "" width :state state :focused focused)
    (lispm-ui-svg-label svg label (+ pad 10) (+ pad 14) (plist-get colors :label)
                        (lispm-ui-svg-font) "start")
    (svg-line svg (+ pad width -21.5) (+ pad 4) (+ pad width -21.5) (+ pad h -4)
              :stroke (plist-get colors :shade))
    (lispm-ui-svg-popup-arrows svg (+ pad width -17) pad palette (plist-get colors :label))
    svg))

(cl-defun lispm-ui-popup-button (label &key state focused pad)
  "Return a pop-up menu showing LABEL, as a string displaying it.
STATE, FOCUSED and PAD are as `lispm-ui-draw-popup-button'
takes them; PAD defaults to two pixels, room for the focus ring.  On a
text terminal it is LABEL and an arrow, between brackets."
  (if (lispm-ui-tty-p)
      (propertize " " 'display (lispm-ui-text-control
                                (lispm-ui-text-button (concat " " label " \u25be ") state)
                                focused state))
    (let ((width (lispm-ui-popup-button-width label))
          (pad (or pad 2)))
      (propertize " " 'display
                  (lispm-ui-image
                   (list 'popup label width state focused pad)
                   (lambda ()
                     (lispm-ui-draw-popup-button lispm-ui-current-palette
                                                 label width :state state
                                                 :focused focused :pad pad)))))))

;;;;; Bevel buttons

(defun lispm-ui-bevel-button-colors (palette state)
  "Return the colours of a bevel button's parts in STATE, as a plist.
STATE is nil, `pressed' or `disabled'.  The parts are its outline lit
along the top and left (:outline-lit) and shaded along the bottom and
right (:outline-shade), its two lit lines (:lit, :lit-inner), its :face,
its two shaded lines (:shade-inner, :shade) and its :label."
  (cl-flet ((g (level) (lispm-ui-grey palette level)))
    (pcase state
      ('pressed
       (list :outline-lit (g #x11) :outline-shade (g #x44)
             :lit (g #x44) :lit-inner (g #x66) :face (g #x88)
             :shade-inner (g #x99) :shade (g #xBB)
             :label (lispm-ui-legible (g #x88) (plist-get palette :text) (g #xFF))))
      ('disabled
       (let ((face (g #xCC)))
         (list :outline-lit (g #x99) :outline-shade (g #x99)
               :lit face :lit-inner face :face face :shade-inner face :shade face
               :label (g #x77))))
      (_
       (list :outline-lit (g #x66) :outline-shade (g #x33)
             :lit (g #xCC) :lit-inner (g #xFF) :face (g #xCC)
             :shade-inner (g #x99) :shade (g #x77)
             :label (plist-get palette :text))))))

(cl-defun lispm-ui-draw-bevel-button (palette width height &key state label
                                              (face 'lispm-ui-list))
  "Return an SVG of a bevel button WIDTH by HEIGHT pixels, in PALETTE.
Bevel buttons are square, as Mac OS 9's toolbars held them: an outline
darker along the bottom and right, two lines lit along the top and left
and two shaded along the bottom and right.  STATE is nil, `pressed'
\(darkened, its bevel turned over) or `disabled' (flat, its outline and
label greyed).  LABEL, when given, is centred: a string as Emacs would
show it in FACE, its icons and images included, see
`lispm-ui-svg-content'."
  (let ((svg (svg-create width height)))
    (lispm-ui-svg-bevel-button svg 0 0 palette width height
                               :state state :label label :face face)
    svg))

(cl-defgeneric lispm-ui-svg-bevel-button (svg x y palette width height
                                              &key state label face)
  "Draw on SVG at X, Y a bevel button WIDTH by HEIGHT pixels, in PALETTE.
See `lispm-ui-draw-bevel-button'; FACE is by default the list's."
  (let ((face (or face 'lispm-ui-list))
        (colors (lispm-ui-bevel-button-colors palette state))
        (w width)
        (h height))
    (cl-flet ((row (from to line part)
                (lispm-ui-svg-row svg (+ x from) (+ x to) (+ y line) (plist-get colors part)))
              (column (col from to part)
                (lispm-ui-svg-column svg (+ x col) (+ y from) (+ y to) (plist-get colors part))))
      (lispm-ui-svg-fill svg x y w h (plist-get colors :face))
      (row 1 (- w 2) 1 :lit)
      (column 1 1 (- h 2) :lit)
      (row 2 (- w 3) 2 :lit-inner)
      (column 2 2 (- h 3) :lit-inner)
      (column (- w 3) 2 (- h 3) :shade-inner)
      (row 2 (- w 3) (- h 3) :shade-inner)
      (column (- w 2) 1 (- h 2) :shade)
      (row 1 (- w 2) (- h 2) :shade)
      (row 0 (- w 1) 0 :outline-lit)
      (column 0 0 (- h 1) :outline-lit)
      (column (- w 1) 1 (- h 1) :outline-shade)
      (row 1 (- w 1) (- h 1) :outline-shade))
    (when label
      (lispm-ui-svg-content
       svg label (+ x (/ (- w (lispm-ui-text-width label face)) 2)) y h
       (plist-get colors :label) face))))

(defun lispm-ui--gradient (svg id from to)
  "Define on SVG the gradient ID, from colour FROM at the top left to TO at
the bottom right."
  (let ((defs (or (car (dom-by-tag svg 'defs))
                  (let ((defs (dom-node 'defs)))
                    (dom-add-child-before svg defs)
                    defs))))
    (dom-append-child defs (dom-node 'linearGradient
                                     `((id . ,id) (x1 . 0) (y1 . 0) (x2 . 1) (y2 . 1))
                                     (dom-node 'stop `((offset . "0%") (stop-color . ,from)))
                                     (dom-node 'stop `((offset . "100%") (stop-color . ,to)))))))

(cl-defgeneric lispm-ui-draw-radio (palette on &key state focused pad)
  "Return an SVG of a radio button in PALETTE, ON or off.
It is a twelve-pixel sphere, outlined, shaded from white at its top left
to grey at its bottom right, with a dot when ON.  STATE is nil,
`pressed' (darkened) or `disabled' (flat and grey).  FOCUSED rings it in
the focus colour, two pixels out; PAD is the room left about it, raised
to two pixels for the ring."
  (let* ((pad (max (or pad 0) (if focused 2 0)))
         (size (+ 12 (* 2 pad)))
         (middle (/ size 2.0))
         (svg (svg-create size size))
         (grey (lambda (level) (lispm-ui-grey palette level)))
         (disabled (eq state 'disabled))
         (from (funcall grey (if (eq state 'pressed) #x88 #xFF)))
         (to (funcall grey (if (eq state 'pressed) #x55 #x88))))
    (lispm-ui--gradient svg "sphere" from to)
    (when focused
      (svg-circle svg middle middle 7 :fill "none" :stroke (plist-get palette :focus) :stroke-width 2))
    (svg-circle svg middle middle 5.5 :fill (if disabled (funcall grey #xDD) "url(#sphere)")
                :stroke (if disabled (funcall grey #x88) (plist-get palette :outline))
                :stroke-width 1)
    (when on
      (svg-circle svg middle middle 2
                  :fill (if disabled
                            (funcall grey #x88)
                          (lispm-ui-legible
                           (lispm-ui-blend from to 0.5)
                           (plist-get palette :outline) (plist-get palette :text)))))
    svg))

(cl-defgeneric lispm-ui-draw-check-box (palette on &key state focused pad)
  "Return an SVG of a check box in PALETTE, checked when ON.
The box is twelve pixels square, outlined, lit along its top and left
and shaded along its bottom and right; the check mark breaks out past
its top right corner, as Mac OS 9's did, so the SVG is fourteen pixels
square, the box at its bottom left.  STATE is nil, `pressed' (darkened)
or `disabled' (flat and grey).  FOCUSED rings the box in the focus
colour, two pixels out; PAD is the room left about it, raised to two
pixels for the ring."
  (let* ((pad (max (or pad 0) (if focused 2 0)))
         (svg (svg-create (+ 14 (* 2 pad)) (+ 14 (* 2 pad))))
         (grey (lambda (level) (lispm-ui-grey palette level)))
         (disabled (eq state 'disabled))
         (pressed (eq state 'pressed))
         (ink (if disabled (funcall grey #x88) (plist-get palette :outline))))
    (when focused
      (lispm-ui-svg-focus-ring svg pad (+ pad 2) 12 12 0 palette))
    (lispm-ui-svg-fill svg pad (+ pad 2) 12 12 ink)
    (lispm-ui-svg-fill svg (+ pad 1) (+ pad 3) 10 10 (funcall grey (if pressed #xAA #xDD)))
    (unless disabled
      (lispm-ui-svg-row svg (+ pad 1) (+ pad 9) (+ pad 3) (funcall grey (if pressed #x88 #xFF)))
      (lispm-ui-svg-column svg (+ pad 1) (+ pad 3) (+ pad 11) (funcall grey (if pressed #x88 #xFF)))
      (lispm-ui-svg-row svg (+ pad 2) (+ pad 10) (+ pad 12) (funcall grey (if pressed #xDD #x88)))
      (lispm-ui-svg-column svg (+ pad 10) (+ pad 4) (+ pad 12) (funcall grey (if pressed #xDD #x88))))
    (when on
      (svg-polyline svg (mapcar (pcase-lambda (`(,x . ,y)) (cons (+ x pad) (+ y pad)))
                                '((2.5 . 7.5) (5.5 . 10.5) (12.5 . 1.5)))
                    :fill "none" :stroke ink :stroke-width 2 :stroke-linecap "square"))
    svg))

(defun lispm-ui-bevel-button (label &optional state height face)
  "Return a bevel button reading LABEL, as a string displaying it.
LABEL is shown as Emacs would show it in FACE, by default the list's,
its icons and images included; STATE is nil, `pressed' or `disabled';
HEIGHT defaults to 20 pixels.  See `lispm-ui-draw-bevel-button'."
  (let* ((face (or face 'lispm-ui-list))
         (height (or height 20))
         (width (+ (lispm-ui-text-width label face)
                   (* 2 lispm-ui-bevel-padding))))
    (if (lispm-ui-tty-p)
        (lispm-ui-text-button label state)
      (propertize " " 'display
                  (lispm-ui-image
                   (list 'bevel (lispm-ui-content-key label) state height face width)
                   (lambda ()
                     (lispm-ui-draw-bevel-button lispm-ui-current-palette width height
                                                 :state state :label label :face face)))))))

(defun lispm-ui-text-button (label &optional state default)
  "Return a button reading LABEL on a text terminal, as text: LABEL between
brackets.  STATE is nil, `pressed' or `disabled'; DEFAULT draws the
default button, which Return presses, in the highlight colour, as the
ring about it marks it on Mac OS 9."
  (let* ((palette lispm-ui-current-palette)
         (ink (cond ((eq state 'disabled) (plist-get palette :muted))
                    ((or default (eq state 'pressed)) (plist-get palette :highlight-text))
                    (t (plist-get palette :text))))
         (ground (cond ((or default (eq state 'pressed)) (plist-get palette :highlight))
                       (t (plist-get palette :chrome))))
         (button (concat "[" label "]")))
    (add-face-text-property 0 (length button) `(:foreground ,ink :background ,ground) t button)
    button))

(cl-defun lispm-ui-push-button (label &key state default focused pad)
  "Return a push button reading LABEL, as a string displaying it.
STATE, DEFAULT, FOCUSED and PAD are as `lispm-ui-draw-push-button'
takes them; PAD defaults to two pixels, room for the focus ring, so a
button keeps its place as the keyboard comes and goes."
  (let ((width (lispm-ui-push-button-width label))
        (pad (or pad (if default 5 2))))
    (if (lispm-ui-tty-p)
        (propertize " " 'display (lispm-ui-text-control
                                  (lispm-ui-text-button (concat " " label " ") state default)
                                  focused))
      (propertize " " 'display
                  (lispm-ui-image
                   (list 'push label width state default focused pad)
                   (lambda ()
                     (lispm-ui-draw-push-button lispm-ui-current-palette label width
                                                :state state :default default
                                                :focused focused :pad pad)))))))

(defun lispm-ui-radio (on &optional state focused)
  "Return a radio button, ON or off, in STATE, as a string displaying it.
FOCUSED rings it; either way it leaves room for the ring, so it keeps its
place as the keyboard comes and goes.  See `lispm-ui-draw-radio'."
  (propertize " " 'display
              (if (lispm-ui-tty-p)
                  (lispm-ui-text-control (if on "(•)" "( )") focused state)
                (lispm-ui-image
                 (list 'radio on state focused)
                 (lambda () (lispm-ui-draw-radio lispm-ui-current-palette on
                                                 :state state :focused focused :pad 2))))))

(defconst lispm-ui--list-check-boxes
  '((on "############" "#wwwwwwwwwd#" "#wd#dddd#dg#" "#wd##dd##sg#" "#wdd####sag#"
        "#wddd##sadg#" "#wdd####ddg#" "#wd##sa##dg#" "#wd#sadd#sg#" "#wddaddddag#"
        "#dggggggggg#" "############")
    (off "############" "#wwwwwwwwwd#" "#wddddddddg#" "#wddddddddg#" "#wddddddddg#"
         "#wddddddddg#" "#wddddddddg#" "#wddddddddg#" "#wddddddddg#" "#wddddddddg#"
         "#dggggggggg#" "############")
    (mixed "############" "#wwwwwwwwwd#" "#wddddddddg#" "#wddddddddg#" "#wddddddddg#"
           "#wd######ag#" "#wd######ag#" "#wddaaaaaag#" "#wddddddddg#" "#wddddddddg#"
           "#dggggggggg#" "############"))
  "The check boxes of Extensions Manager's list, as Mac OS 9 drew them, a row
of pixels a string: checked with a cross, empty, or crossed by a bar for
a group only some of whose items are on.  # is the outline, w, d, a, g
and s the greys #FF, #DD, #AA, #88 and #77.")

(cl-defgeneric lispm-ui-draw-list-check-box (palette state)
  "Return an SVG of a list's check box in PALETTE, twelve pixels square.
STATE is `on', `off' or `mixed', see `lispm-ui--list-check-boxes'."
  (let ((svg (svg-create 12 12)))
    (lispm-ui-svg-pixel-map
     svg 0 0 (alist-get state lispm-ui--list-check-boxes)
     `((?# . ,(plist-get palette :outline))
       ,@(mapcar (pcase-lambda (`(,char . ,level)) (cons char (lispm-ui-grey palette level)))
                 '((?w . #xFF) (?d . #xDD) (?a . #xAA) (?g . #x88) (?s . #x77)))))
    svg))

(defun lispm-ui-list-check-box (state)
  "Return a list's check box in STATE as a string displaying it.
See `lispm-ui-draw-list-check-box'."
  (propertize " " 'display
              (if (lispm-ui-tty-p)
                  (lispm-ui-text-control (pcase state ('on "[✓]") ('mixed "[-]") (_ "[ ]")))
                (lispm-ui-image
                 (list 'list-check-box state)
                 (lambda () (lispm-ui-draw-list-check-box lispm-ui-current-palette
                                                          state))))))

(defun lispm-ui-check-box (on &optional state focused pad)
  "Return a check box, checked when ON, in STATE, as a string displaying it.
FOCUSED rings it, and PAD leaves room about it; see
`lispm-ui-draw-check-box'."
  (propertize " " 'display
              (if (lispm-ui-tty-p)
                  (lispm-ui-text-control (if on "[✓]" "[ ]") focused state)
                (lispm-ui-image
                 (list 'check-box on state focused pad)
                 (lambda () (lispm-ui-draw-check-box lispm-ui-current-palette on
                                                     :state state :focused focused
                                                     :pad (or pad 0)))))))

(defun lispm-ui-text-control (text &optional focused state)
  "Return TEXT, a control drawn as text on a text terminal, ringed in the
focus colour when FOCUSED, dimmed when STATE is `disabled'.  Controls
inserted in a buffer show it as their `display', as they show their
images elsewhere, see `lispm-ui-show-focus'."
  (let ((text (copy-sequence text)))
    (cond (focused
           (add-face-text-property 0 (length text)
                                   `(:foreground ,(lispm-ui-color :highlight-text)
                                                 :background ,(lispm-ui-color :focus))
                                   nil text))
          ((eq state 'disabled)
           (add-face-text-property 0 (length text) `(:foreground ,(lispm-ui-color :muted))
                                   nil text)))
    text))

;;;; Controls in buffers
;;
;; Push buttons and radio buttons inserted in a buffer, as a settings
;; window holds them, are buttons of button.el: Tab and Shift-Tab go from one to
;; the next, Return presses the one point is on.  Point itself is not
;; shown in such a buffer, so the control it is on is drawn ringed in the
;; focus colour, as Mac OS 9 ringed the control the keyboard was on; a
;; push button pressed, from the keyboard or with the mouse, is drawn
;; pushed in until it is let go.

(defvar-local lispm-ui-focus nil
  "Position of the control drawn focused in this buffer, or nil.")

;;;; Tabs

(defconst lispm-ui-tab-height 21
  "Height in pixels of a classic tab; its pane edge takes three more rows.")

(defconst lispm-ui-tab-padding 13
  "Pixels between a classic tab's bottom sides and its label.")

(defconst lispm-ui--tabs-lead 8
  "Pixels of pane edge before a graphical tab row.")

(defconst lispm-ui--tab-slant
  '((10 . t) (8 . t) (7 . t) (6 . t) (6) (5 . t) (5) (5) (4 . t) (4) (4)
    (3 . t) (3) (3) (2 . t) (2) (2) (1 . t) (1) (1) (0 . t))
  "Where a tab's left side runs, row by row from its top, as (COLUMN . STEP).
COLUMN holds the outline; STEP is non-nil on a row where the outline
steps out, which is softened there.  The right side is the mirror.")

(cl-defgeneric lispm-ui-draw-tab (palette label width &key front face)
  "Return an SVG of a tab WIDTH pixels wide reading LABEL, in PALETTE.
Tabs are trapezoids, their sides slanting out a pixel every three rows
from top corners rounded off, and stand on the edge of the pane they
choose among: the SVG is `lispm-ui-tab-height' tall, and
three rows more for that edge.  The tab in FRONT is the pane's colour
and opens into it; a tab behind is darker, lit along its top and left,
and the pane's outline runs under it.  LABEL is a string as Emacs would
show it in FACE, by default the system font's, its icons included; every
label is black, none heavier than the others."
  (let* ((face (or face 'lispm-ui-title))
         (h lispm-ui-tab-height)
         (w width)
         (svg (svg-create w (+ h 3)))
         (grey (lambda (level) (lispm-ui-grey palette level)))
         (outline (plist-get palette :outline))
         (edge (funcall grey #x22))
         (body (funcall grey (if front #xEE #xCC)))
         (between (funcall grey #xCC))
         (lit (funcall grey (if front #xFF #xDD)))
         (soft (funcall grey (if front #xBB #xAA)))
         (shade (funcall grey #x88))
         (shade-soft (funcall grey #x44))
         (shade-lit (funcall grey (if front #xDD #xCC))))
    (cl-flet ((dot (x y color) (lispm-ui-svg-dot svg x y color))
              (row (from to y color) (lispm-ui-svg-row svg from to y color)))
      (lispm-ui-svg-fill svg 0 0 w h (plist-get palette :chrome))
      (seq-do-indexed
       (pcase-lambda (`(,left . ,step) y)
         (let ((right (- w 1 left)))
           (pcase y
             (0 (row (1+ left) (1- right) 0 outline)
                (dot left 0 edge) (dot right 0 edge))
             (1 (row (+ left 2) (- right 2) 1 between)
                (dot left 1 edge) (dot (1+ left) 1 outline) (dot (+ left 2) 1 soft)
                (dot right 1 edge) (dot (1- right) 1 outline))
             (2 (row (+ left 3) (- right 3) 2 lit)
                (dot left 2 edge) (dot (1+ left) 2 soft) (dot (+ left 2) 2 between)
                (dot right 2 edge) (dot (1- right) 2 shade) (dot (- right 2) 2 shade-lit))
             (_ (row (1+ left) (1- right) y body)
                (dot left y (if step edge outline))
                (dot right y (if step edge outline))
                (if (= y 3)
                    (progn (dot (1+ left) y soft) (dot (+ left 2) y between) (dot (+ left 3) y lit))
                  (dot (1+ left) y (if step soft between))
                  (dot (+ left 2) y lit))
                (dot (1- right) y (if step shade-soft shade))
                (when front (dot (- right 2) y shade-lit))))))
       lispm-ui--tab-slant)
      ;; The pane's edge: its outline, then a lit band, under the tabs
      ;; behind; the tab in front opens into the pane.
      (if front
          (progn
            (row 0 (1- w) h body)
            (row 0 (1- w) (+ h 1) body)
            (row 0 (1- w) (+ h 2) body)
            (dot 0 h outline) (dot 1 h between) (dot 2 h lit)
            (dot (- w 1) h outline) (dot (- w 2) h soft) (dot (- w 3) h shade-lit)
            (row 0 1 (+ h 1) between) (row (- w 2) (- w 1) (+ h 1) between)
            (dot 0 (+ h 2) (funcall grey #xFF)) (dot (- w 1) (+ h 2) (funcall grey #xFF)))
        (row 0 (1- w) h outline)
        (row 0 (1- w) (+ h 1) between)
        (row 0 (1- w) (+ h 2) (funcall grey #xFF))))
    (lispm-ui-svg-content
     svg label (/ (- w (lispm-ui-text-width label face)) 2) 0 h
     (plist-get palette :text) face)
    svg))

(cl-defgeneric lispm-ui-draw-tab-edge (palette width)
  "Return an SVG of the pane's edge WIDTH pixels wide, beside or between tabs.
It is as tall as a tab and the pane's edge under it, see
`lispm-ui-draw-tab': the window's body above, the pane's
outline and lit band below, in PALETTE."
  (let* ((h lispm-ui-tab-height)
         (svg (svg-create width (+ h 3))))
    (lispm-ui-svg-fill svg 0 0 width h (plist-get palette :chrome))
    (lispm-ui-svg-row svg 0 (1- width) h (plist-get palette :outline))
    (lispm-ui-svg-row svg 0 (1- width) (+ h 1) (lispm-ui-grey palette #xCC))
    (lispm-ui-svg-row svg 0 (1- width) (+ h 2) (lispm-ui-grey palette #xFF))
    svg))


(defun lispm-ui-tabs-width (labels &optional face)
  "Return the natural width of tabs reading LABELS in FACE.
The result is in pixels on a graphical frame and cells on a terminal,
including the pane edge before and between tabs.  FACE defaults to
`lispm-ui-title'; see `lispm-ui-tabs' for the control's data format."
  (if (lispm-ui-tty-p)
      (+ 1 (apply #'+ (mapcar (lambda (label) (+ 3 (string-width label))) labels)))
    (+ lispm-ui--tabs-lead
       (apply #'+ (mapcar (lambda (label)
                           (+ 2 (* 2 lispm-ui-tab-padding)
                              (lispm-ui-text-width label (or face 'lispm-ui-title))))
                         labels)))))

(defun lispm-ui--tab-edge (width)
  "Return the graphical pane edge WIDTH pixels wide as a display string."
  (propertize " " 'display
              (lispm-ui-image
               (list 'tabs-edge width)
               (lambda () (lispm-ui-draw-tab-edge lispm-ui-current-palette width)))))

(defun lispm-ui--tab-labels (labels room measure fit)
  "Fit LABELS in ROOM units, shortening the widest labels first.
MEASURE returns a label's width; FIT takes a label and its width limit.
Only label widths count toward ROOM; callers reserve borders first.
Return new strings without changing the input or its properties."
  (let* ((widths (mapcar measure labels))
         (cap (apply #'max 0 widths)))
    (while (and (> cap 0)
                (> (apply #'+ (mapcar (lambda (width) (min width cap)) widths)) room))
      (setq cap (1- cap)))
    (seq-mapn (lambda (label width)
                (if (<= width cap) label (funcall fit label cap)))
              labels widths)))

(defun lispm-ui--text-tabs (tabs width)
  "Return one row of TABS in WIDTH cells, retaining each tab's properties."
  (let* ((labels (lispm-ui--tab-labels
                  (mapcar #'car tabs) (max 0 (- width 1 (* 3 (length tabs))))
                  #'string-width
                  (lambda (label limit)
                    (truncate-string-to-width label limit nil nil (if (> limit 0) "…" "")))))
         (rule (lambda (count)
                 (propertize (make-string (max 0 count) ?─)
                             'face `(:foreground ,(lispm-ui-color :muted)))))
         (row (concat
               (funcall rule 1)
               (mapconcat
                #'identity
                (seq-mapn
                 (pcase-lambda (`(,_ ,front . ,properties) label)
                   (let ((tab (concat " " label " ")))
                     (add-face-text-property 0 (length tab)
                                             (if front 'tab-line-tab-current 'tab-line-tab) t tab)
                     (concat (apply #'propertize tab properties) (funcall rule 1))))
                 tabs labels) ""))))
    (concat row (funcall rule (- width (string-width row))))))

(defun lispm-ui--image-tabs (tabs width face)
  "Return one row of TABS in WIDTH pixels, using FACE below label faces."
  (let* ((face (or face 'lispm-ui-title))
         (padding (* 2 lispm-ui-tab-padding))
         (labels (lispm-ui--tab-labels
                  (mapcar #'car tabs)
                  (max 0 (- width lispm-ui--tabs-lead (* (+ padding 2) (length tabs))))
                  (lambda (label) (lispm-ui-text-width label face))
                  (lambda (label limit)
                    (if (< limit (lispm-ui-text-width "…" face)) ""
                      (lispm-ui-text-fit label limit face)))))
         (used lispm-ui--tabs-lead)
         (parts
          (seq-mapn
           (pcase-lambda (`(,_ ,front . ,properties) label)
             (let* ((tab-width (+ padding (lispm-ui-text-width label face)))
                    (image (lispm-ui-image
                            (list 'tabs-tab (lispm-ui-content-key label) front face tab-width)
                            (lambda ()
                              (lispm-ui-draw-tab lispm-ui-current-palette label tab-width
                                                 :front front :face face)))))
               (setq used (+ used tab-width 2))
               (concat (apply #'propertize " " 'display image properties)
                       (lispm-ui--tab-edge 2))))
           tabs labels)))
    (concat (lispm-ui--tab-edge lispm-ui--tabs-lead)
            (apply #'concat parts)
            (when (> width used) (lispm-ui--tab-edge (- width used))))))

(defun lispm-ui-tabs (tabs width &optional face)
  "Return TABS on their pane edge, fitting WIDTH pixels or terminal cells.
Each tab is (LABEL FRONT . PROPERTIES): LABEL is its text, FRONT selects
the active pane, and PROPERTIES are copied to its clickable string.
Callers supply their own button action or keymap, so the control works
in a buffer, header line or tab line without knowing its application.
FACE defaults to `lispm-ui-title' for graphical labels.  Text terminals
use ordinary tab faces and retain the same action properties.  Long
labels are shortened; narrow panes wrap complete tabs onto more rows.
Drawing uses the current palette and look-overridable tab generics.
WIDTH must allow at least one tab border (4 cells or 36 pixels)."
  (let* ((tty (lispm-ui-tty-p))
         (lead (if tty 1 lispm-ui--tabs-lead))
         (border (if tty 3 (+ 2 (* 2 lispm-ui-tab-padding))))
         (width (max (+ lead border) width))
         ;; Keep a short readable label before putting another tab on a row.
         (count (max 1 (/ (- width lead) (if tty 6 50)))))
    (mapconcat (lambda (row)
                 (if tty (lispm-ui--text-tabs row width)
                   (lispm-ui--image-tabs row width face)))
               (seq-partition tabs count) "\n")))

(defun lispm-ui--swap-image (position image)
  "Show IMAGE for the control at POSITION of the current buffer."
  (with-silent-modifications
    (let ((inhibit-read-only t))
      (put-text-property position (1+ position) 'display image))))

(defun lispm-ui-show-focus ()
  "Draw the control at point focused, and the one focused before plain.
Controls carry their images as the text properties `lispm-ui-plain' and
`lispm-ui-focused'.  Buffers holding controls run this from
`post-command-hook', and after drawing their controls again."
  (let ((here (and (get-text-property (point) 'lispm-ui-focused) (point)))
        (before lispm-ui-focus))
    (unless (eql here before)
      (when (and before (< before (point-max))
                 (get-text-property before 'lispm-ui-plain))
        (lispm-ui--swap-image before (get-text-property before 'lispm-ui-plain)))
      (when here
        (lispm-ui--swap-image here (get-text-property here 'lispm-ui-focused)))
      (setq lispm-ui-focus here))))

(defun lispm-ui--press-at (position)
  "Draw the push button at POSITION pushed in, until it is let go.
From the mouse, it is let go when the button rises, and pressed only if
it rises over the push button, as on Mac OS 9; from the keyboard, it
blinks.  Return non-nil when the button was pressed."
  (let ((pressed (get-text-property position 'lispm-ui-pressed))
        (shown (get-text-property position 'display)))
    (lispm-ui--swap-image position pressed)
    (redisplay t)
    (prog1 (if (not (mouse-event-p last-input-event))
               (progn (sit-for (alist-get 'button lispm-ui-modal-flashes)) t)
             (let ((release (let ((echo-keystrokes 0)) (lispm-ui-modal-read-event))))
               (and (mouse-event-p release)
                    (eq (posn-window (event-end release)) (posn-window (event-start last-input-event)))
                    (eql (posn-point (event-end release)) position))))
      (lispm-ui--swap-image position shown))))

(defun lispm-ui-push (&optional position)
  "Press the push button at POSITION, by default point's, as Return and
Space do, or as a press of the mouse does; other buttons are pushed as
`push-button' pushes them."
  (interactive)
  (let ((position (or position (point))))
    (if (and (get-text-property position 'lispm-ui-pressed)
             (not (lispm-ui--press-at position)))
        nil
      (push-button position))))

(defun lispm-ui--mouse-push (event)
  "Press the push button EVENT went down on, see `lispm-ui-push'."
  (interactive "e")
  (let ((position (event-start event)))
    (with-selected-window (posn-window position)
      (lispm-ui-push (posn-point position)))))

(defvar-keymap lispm-ui-push-button-map
  :doc "Keymap of a push button in a buffer."
  :parent button-map
  "RET" #'lispm-ui-push
  "SPC" #'lispm-ui-push
  "<down-mouse-1>" #'lispm-ui--mouse-push
  "<mouse-1>" #'ignore)

(defun lispm-ui-image-of (control)
  "Return the image CONTROL, a string displaying one, shows."
  (get-text-property 0 'display control))

(cl-defun lispm-ui-insert-push-button (label action &key help default disabled)
  "Insert a push button reading LABEL, which calls ACTION with no arguments.
HELP describes it; DEFAULT rings it, as the button Return presses.  The
button is drawn focused while point is on it, see `lispm-ui-show-focus';
its text, drawn in its image, is kept in its `lispm-ui-label' property.
A DISABLED button is drawn dimmed and does nothing; Tab passes it by."
  (if disabled
      (insert (propertize " " 'display (lispm-ui-image-of
                                        (lispm-ui-push-button label :state 'disabled))
                          'lispm-ui-label label 'face nil 'help-echo (or help label)))
    (lispm-ui--insert-push-button label action help default)))

(defun lispm-ui--insert-control (images label action help &optional keymap)
  "Insert a control showing IMAGES, as a button calling ACTION with no arguments.
IMAGES is a function of FOCUSED and STATE returning the control's image,
or its text on a text terminal.  LABEL follows it, if non-nil; HELP
describes it.  KEYMAP, by default button.el's, is the control's."
  (insert-text-button
   (concat (propertize " " 'display (funcall images nil nil)
                       'lispm-ui-plain (funcall images nil nil)
                       'lispm-ui-focused (funcall images t nil)
                       'lispm-ui-pressed (funcall images nil 'pressed))
           (if label (concat " " label) ""))
   'action (lambda (_) (funcall action))
   'face 'default 'follow-link t 'help-echo help
   'keymap (or keymap button-map)))

(cl-defun lispm-ui-insert-radio-button (label on action &key help)
  "Insert a radio button named LABEL, ON or off, which calls ACTION with no
arguments when pressed; HELP describes it.  Drawn focused while point is
on it, see `lispm-ui-show-focus'."
  (lispm-ui--insert-control
   (lambda (focused state)
     (lispm-ui-image-of (lispm-ui-radio on state focused)))
   label action (or help label)))

(cl-defun lispm-ui-insert-check-box (label on action &key help)
  "Insert a check box named LABEL, checked when ON, which calls ACTION with no
arguments when pressed; HELP describes it.  Drawn focused while point is
on it, see `lispm-ui-show-focus'."
  (lispm-ui--insert-control
   (lambda (focused state)
     (lispm-ui-image-of (lispm-ui-check-box on state focused 2)))
   label action (or help label)))

(cl-defun lispm-ui-insert-popup-button (label choices action &key help current)
  "Insert a pop-up menu showing LABEL, offering CHOICES, a list of (VALUE .
TEXT); pressed, its menu opens at it, CURRENT's item checked, and the
VALUE of the item chosen is given to ACTION.  HELP describes it.  Return,
Space and the mouse open it, as they press a push button."
  (lispm-ui--insert-control
   (lambda (focused state) (lispm-ui-image-of
                            (lispm-ui-popup-button label :state state :focused focused)))
   nil
   (lambda ()
     (when-let* ((choice (lispm-ui-choose choices current nil (or help label))))
       (funcall action (car choice))))
   (or help label)
   lispm-ui-push-button-map))

(defun lispm-ui--insert-push-button (label action help default)
  "Insert an enabled push button, see `lispm-ui-insert-push-button'."
  (insert-text-button " "
                      'display (lispm-ui-image-of
                                (lispm-ui-push-button label :default default))
                      'lispm-ui-plain (lispm-ui-image-of
                                       (lispm-ui-push-button label :default default))
                      'lispm-ui-focused (lispm-ui-image-of
                                         (lispm-ui-push-button
                                          label :default default :focused t))
                      'lispm-ui-pressed (lispm-ui-image-of
                                         (lispm-ui-push-button
                                          label :default default :state 'pressed))
                      'action (lambda (_) (funcall action))
                      'lispm-ui-label label
                      'face nil 'keymap lispm-ui-push-button-map
                      'help-echo (or help label)))

(provide 'lispm-ui-controls)
;;; lispm-ui-controls.el ends here
