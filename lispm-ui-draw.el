;;; lispm-ui-draw.el --- What LispM UI draws with: SVG, and the images made of it  -*- lexical-binding: t; -*-

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

;; The primitives every control is drawn of, all pure: palette and
;; geometry in, an SVG out.  Rectangles, rows, columns and dots of
;; pixels, bevelled edges, arrowheads and dotted rings; text in faces,
;; icons and Emacs's own images, laid out as Emacs lays them out on a
;; line.  And the cache of drawn images, `lispm-ui-image', each made once
;; for each palette.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'dom)
(require 'lispm-ui-base)
(require 'lispm-ui-palette)
(require 'lispm-ui-look)
(require 'lispm-ui-text)

;;;; Images

(defvar lispm-ui--images (make-hash-table :test #'equal)
  "Drawn images by key: what was drawn, its size and the palette generation.")

(defun lispm-ui-svg-image (svg)
  "Return the image of SVG, svg.el's or SVG text, centred on its line and
never scaled.  An Emacs built without SVG gets the image all the same,
which it cannot show: the text under it shows instead, where
`svg-image' would signal."
  (cond ((not (image-type-available-p 'svg))
         (list 'image :type 'svg :data (if (stringp svg) svg (with-temp-buffer (svg-print svg) (buffer-string)))
               :ascent 'center :scale 1))
        ((stringp svg) (create-image svg 'svg t :ascent 'center :scale 1))
        (t (svg-image svg :ascent 'center :scale 1))))

(defun lispm-ui-image (key draw)
  "Return the image of KEY, calling DRAW to make its SVG the first time,
an SVG of svg.el's or SVG text.  It is made again in a new palette."
  (let ((key (cons lispm-ui-palette-generation key)))
    (or (gethash key lispm-ui--images)
        (progn
          (when (> (hash-table-count lispm-ui--images) 512)
            (clrhash lispm-ui--images))
          (puthash key (lispm-ui-svg-image (funcall draw))
                   lispm-ui--images)))))

(defun lispm-ui-look-image (key draw)
  "Return the image of KEY drawn by DRAW, kept apart for the look in use,
see `lispm-ui-image'."
  (lispm-ui-image (cons lispm-ui-look key) draw))

;;;; Pixels

(defun lispm-ui-svg-fill (svg x y width height color)
  "Fill the rectangle of WIDTH by HEIGHT at X, Y of SVG with COLOR.
Lines are drawn as one-pixel rectangles, so they fall on whole pixels."
  (svg-rectangle svg x y width height :fill color :stroke "none"))

(defun lispm-ui-svg-dot (svg x y color)
  "Colour the pixel at X, Y of SVG with COLOR."
  (lispm-ui-svg-fill svg x y 1 1 color))

(defun lispm-ui-svg-row (svg from to y color)
  "Draw a line along row Y of SVG in COLOR, from column FROM to TO, both in."
  (when (<= from to)
    (lispm-ui-svg-fill svg from y (1+ (- to from)) 1 color)))

(defun lispm-ui-svg-column (svg x from to color)
  "Draw a line down column X of SVG in COLOR, from row FROM to TO, both in."
  (when (<= from to)
    (lispm-ui-svg-fill svg x from 1 (1+ (- to from)) color)))

;;;; Forms of the PC looks
;;
;; The PC looks build their controls of a few forms: a rectangle framed
;; in lines lit along its top and left and shaded along its bottom and
;; right, small arrows of pixels, a dotted ring.

(defun lispm-ui-svg-paint (svg x y width height color)
  "Fill the WIDTH by HEIGHT rectangle at X, Y of SVG with COLOR, as
`lispm-ui-svg-fill' does; nothing is drawn for a rectangle without area
or a nil COLOR."
  (when (and color (> width 0) (> height 0))
    (svg-rectangle svg x y width height :fill color :stroke "none")))

(defun lispm-ui-svg-edges (svg x y width height top-left bottom-right)
  "Frame the WIDTH by HEIGHT rectangle at X, Y of SVG in lines inside it.
TOP-LEFT lists the colours of the lines along its top and left,
outermost first, BOTTOM-RIGHT those along its bottom and right; a nil
colour leaves its line out.  A line along the bottom and right takes the
corners it shares with one along the top and left, as Windows and OS/2
drew their buttons."
  (dotimes (i (max (length top-left) (length bottom-right)))
    (let ((lit (nth i top-left))
          (shade (nth i bottom-right))
          (left (+ x i))
          (top (+ y i))
          (right (- (+ x width) 1 i))
          (bottom (- (+ y height) 1 i)))
      (when (and (<= left right) (<= top bottom))
        (when shade
          (lispm-ui-svg-paint svg left bottom (1+ (- right left)) 1 shade)
          (lispm-ui-svg-paint svg right top 1 (1+ (- bottom top)) shade))
        (when lit
          (lispm-ui-svg-paint svg left top (- right left) 1 lit)
          (lispm-ui-svg-paint svg left top 1 (- bottom top) lit))))))

(defun lispm-ui-svg-outline (svg x y width height color)
  "Outline the WIDTH by HEIGHT rectangle at X, Y of SVG in COLOR, inside it."
  (lispm-ui-svg-edges svg x y width height (list color) (list color)))

(defun lispm-ui-svg-arrow (svg x y size direction color)
  "Draw on SVG a solid arrowhead SIZE rows deep pointing in DIRECTION.
DIRECTION is `up', `down', `left' or `right'; the arrowhead's widest row
is 2 SIZE - 1 pixels long, and its bounding box's top left is at X, Y."
  (dotimes (i size)
    (let ((from i) (length (- (* 2 size) 1 (* 2 i))))
      (pcase direction
        ('down (lispm-ui-svg-paint svg (+ x from) (+ y i) length 1 color))
        ('up (lispm-ui-svg-paint svg (+ x from) (+ y (- size 1 i)) length 1 color))
        ('right (lispm-ui-svg-paint svg (+ x i) (+ y from) 1 length color))
        ('left (lispm-ui-svg-paint svg (+ x (- size 1 i)) (+ y from) 1 length color))))))

(defun lispm-ui-svg-dotted (svg x y width height color)
  "Draw on SVG a dotted rectangle about the WIDTH by HEIGHT box at X, Y,
every other pixel in COLOR, as Windows and OS/2 marked the control the
keyboard was on."
  (cl-flet ((dot (dx dy) (lispm-ui-svg-paint svg (+ x dx) (+ y dy) 1 1 color)))
    (cl-loop for dx from 0 below width by 2
             do (dot dx 0) (dot dx (1- height)))
    (cl-loop for dy from 0 below height by 2
             do (dot 0 dy) (dot (1- width) dy))))

(defun lispm-ui-svg-pixels (svg x y rows colors)
  "Draw ROWS, strings of pixels, on SVG from X, Y.
COLORS maps each character to its colour; a character it leaves out is
left clear.  A run of one colour along a row is drawn as one rectangle."
  (seq-do-indexed
   (lambda (row dy)
     (let ((start 0))
       (while (< start (length row))
         (let* ((char (aref row start))
                (end start))
           (while (and (< end (length row)) (eq (aref row end) char))
             (setq end (1+ end)))
           (when-let* ((color (alist-get char colors)))
             (lispm-ui-svg-paint svg (+ x start) (+ y dy) (- end start) 1 color))
           (setq start end)))))
   rows))

;;;; Fonts

(defvar lispm-ui--svg-fonts (make-hash-table :test #'equal)
  "Fonts of SVG labels by palette generation and face, see
`lispm-ui-svg-font'; faces are set again with every palette.")

(defun lispm-ui--synthetic-bold-p (face)
  "Return non-nil when Emacs draws bold FACE by thickening its regular glyphs.
A family without a bold face of its own, as Geneva, is drawn so; its
bold text then runs exactly as wide as its regular, which a real bold
face hardly ever does."
  (with-memoization (gethash (list lispm-ui-palette-generation 'synthetic face)
                             lispm-ui--svg-fonts)
    (let ((sample "Don’t Save Cancel OK")
          (spec (if (symbolp face) (list :inherit face) face)))
      (= (string-pixel-width (propertize sample 'face `(:weight bold ,@spec)))
         (string-pixel-width (propertize sample 'face `(:weight normal ,@spec)))))))

(defun lispm-ui--svg-families (family)
  "Return the font families SVG text set in FAMILY, a face's, is drawn
in: FAMILY, by default Geneva, then the one for Chinese and Japanese in
the language spoken, then any sans-serif."
  (string-join (delq nil (list (if (stringp family) family "Geneva")
                               (lispm-ui-text-cjk-family)
                               "sans-serif"))
               ", "))

(defun lispm-ui-svg-font (&optional face)
  "Return the font of SVG labels matching FACE, by default `lispm-ui-title'.
The value is a plist: :family and :size, in pixels, are FACE's; :weight
is bold when FACE is.  A family without a bold face Emacs makes bold
itself, drawing every glyph a second time half a pixel to the right;
:synthetic is then non-nil and labels are drawn the same way, so a label
drawn as SVG is as heavy as the titles and menus Emacs draws beside it."
  (let ((face (or face 'lispm-ui-title)))
    (with-memoization (gethash (cons lispm-ui-palette-generation face)
                               lispm-ui--svg-fonts)
      (let* ((family (face-attribute face :family nil t))
             (height (face-attribute face :height nil t))
             (bold (memq (face-attribute face :weight nil t)
                         '(semi-bold bold extra-bold ultra-bold heavy black)))
             (synthetic (and bold (display-graphic-p)
                             (lispm-ui--synthetic-bold-p face))))
        (list :family (lispm-ui--svg-families family)
              :size (if (numberp height) (/ height 10.0) 12)
              :weight (if (and bold (not synthetic)) "bold" "normal")
              :synthetic synthetic)))))

(defun lispm-ui-svg-label (svg text x y color font &optional anchor)
  "Write TEXT on SVG at X, its baseline at Y, in COLOR and FONT.
FONT is a value of `lispm-ui-svg-font', with its :style, as \"italic\",
and its :decoration, as \"underline\", where given; ANCHOR is SVG's text
anchor, by default the middle."
  (dolist (dx (if (plist-get font :synthetic) '(0 0.5) '(0)))
    ;; Spaces are kept, as Emacs shows them, at a run's ends too.
    (apply #'svg-text svg (substring-no-properties text) :x (+ x dx) :y y :text-anchor (or anchor "middle")
           :xml:space "preserve"
           :font-family (plist-get font :family) :font-size (plist-get font :size)
           :font-weight (plist-get font :weight) :fill color
           (append (and (plist-get font :style) (list :font-style (plist-get font :style)))
                   (and (plist-get font :decoration) (list :text-decoration (plist-get font :decoration)))))))

;;;; Pictures

(defvar lispm-ui--svg-doms (make-hash-table :test #'equal)
  "SVG documents read from files or data, by file or data.")

(defun lispm-ui--escape-xml (string)
  "Return STRING with the characters XML reserves written as references."
  (replace-regexp-in-string "[&<>\"]"
                            (lambda (char)
                              (pcase char ("&" "&amp;") ("<" "&lt;") (">" "&gt;") (_ "&quot;")))
                            string t t))

(defun lispm-ui--clean-dom (node)
  "Return NODE, parsed XML, ready to be printed again by `svg-print':
comments dropped, text and attribute values escaped, which `svg-print'
writes as they are."
  (cond ((stringp node) (lispm-ui--escape-xml node))
        ((memq (car-safe node) '(comment top)) nil)
        ((consp node)
         `(,(car node)
           ,(mapcar (lambda (attribute)
                      (cons (car attribute)
                            (if (stringp (cdr attribute))
                                (lispm-ui--escape-xml (cdr attribute))
                              (cdr attribute))))
                    (nth 1 node))
           ,@(delq nil (mapcar #'lispm-ui--clean-dom (nthcdr 2 node)))))))

(defun lispm-ui-svg-dom (file &optional data)
  "Return the SVG document of FILE, or of the string DATA, or nil.
Nil too where Emacs cannot parse XML.  The document is cleaned to be
printed again, see `lispm-ui--clean-dom'."
  (when (fboundp 'libxml-parse-xml-region)
    (with-memoization (gethash (or data file) lispm-ui--svg-doms)
      (when (or data (file-readable-p file))
        (with-temp-buffer
          (if data (insert data) (insert-file-contents file))
          (let ((dom (libxml-parse-xml-region (point-min) (point-max))))
            ;; A document with a declaration or comments around its root
            ;; comes back as a `top' node holding them.
            (when (eq (car-safe dom) 'top)
              (setq dom (seq-find (lambda (node) (eq (car-safe node) 'svg)) (nthcdr 2 dom))))
            (and dom (lispm-ui--clean-dom dom))))))))

(defun lispm-ui-svg-embed (svg dom x y width height)
  "Draw the SVG document DOM on SVG at X, Y, WIDTH by HEIGHT pixels."
  (let ((copy (copy-tree dom)))
    (dom-set-attribute copy 'x x)
    (dom-set-attribute copy 'y y)
    (dom-set-attribute copy 'width width)
    (dom-set-attribute copy 'height height)
    (dom-append-child svg copy)))

(defun lispm-ui-svg-icon (svg file x y size)
  "Draw the SVG icon FILE on SVG at X, Y, SIZE pixels square."
  (when-let* ((icon (lispm-ui-svg-dom file)))
    (lispm-ui-svg-embed svg icon x y size size)))

(defun lispm-ui--image-size (spec)
  "Return the size in pixels of image SPEC, as (WIDTH . HEIGHT).
Where no image can be shown its :width and :height are taken, or 16."
  (or (ignore-errors (image-size spec t))
      (cons (or (image-property spec :width) 16) (or (image-property spec :height) 16))))

(defun lispm-ui-svg-emacs-image (svg spec x y width height)
  "Draw the Emacs image SPEC on SVG at X, Y, WIDTH by HEIGHT pixels.
SVG images are drawn as SVG; others are embedded as they are."
  (let* ((file (image-property spec :file))
         (data (image-property spec :data))
         (type (or (image-property spec :type)
                   (and file (image-supported-file-p file))
                   (and data (image-type-from-data data)))))
    (if (eq type 'svg)
        (when-let* ((dom (lispm-ui-svg-dom file data)))
          (lispm-ui-svg-embed svg dom x y width height))
      (when (or data (and file (file-readable-p file)))
        (svg-embed svg (or data file) (format "image/%s" (if (eq type 'jpeg) "jpeg" (or type 'png)))
                   (and data t) :x x :y y :width width :height height)))))

;;;; Text as Emacs shows it
;;
;; A control's label may be any text Emacs shows: words in a face, glyphs
;; of an icon font, images.  Drawn into the control's SVG run by run, it
;; keeps the widths Emacs measures for it, so a control is as wide as its
;; label would be on the line.

(defun lispm-ui--face-attribute (faces attribute)
  "Return ATTRIBUTE of FACES, a face property's value, or nil.
The first face naming it wins, as in Emacs's own merging."
  (catch 'found
    (dolist (face (if (or (symbolp faces) (keywordp (car-safe faces))) (list faces) faces))
      (let ((value (cond ((and (symbolp face) (facep face)) (face-attribute face attribute nil t))
                         ((consp face) (or (plist-get face attribute)
                                           (when-let* ((parent (plist-get face :inherit)))
                                             (lispm-ui--face-attribute parent attribute)))))))
        (unless (or (null value) (eq value 'unspecified))
          (throw 'found value))))
    nil))

(defun lispm-ui-svg-content (svg content x top height color face)
  "Draw CONTENT, a string as Emacs would show it, on SVG from X.
Its runs are centred in the HEIGHT rows from TOP: words and icon glyphs
in their faces layered over FACE, images as they are, stretches of space
as wide as they say.  Text without a colour of its own is drawn in COLOR.
Return the x where CONTENT ends."
  (let ((content (copy-sequence content))
        (start 0))
    (add-face-text-property 0 (length content) face t content)
    (while (< start (length content))
      (let* ((end (or (next-property-change start content) (length content)))
             (run (substring content start end))
             (width (string-pixel-width run))
             (display (get-text-property 0 'display run))
             (image (and (eq (car-safe display) 'image) display)))
        (cond
         (image
          ;; As wide as Emacs lays it out, which `image-size' can exceed
          ;; on a display that scales images up.
          (pcase-let* ((`(,iw . ,ih) (lispm-ui--image-size image))
                       (w (if (> width 0) width iw))
                       (h (round (* ih (/ (float w) (max 1 iw))))))
            (lispm-ui-svg-emacs-image svg image x (+ top (/ (- height h) 2)) w h)))
         ;; A stretch of space, or text shown as something else.
         ((or (eq (car-safe display) 'space) (stringp display)))
         ((not (string-blank-p run))
          (let* ((faces (get-text-property 0 'face run))
                 (family (lispm-ui--face-attribute faces :family))
                 (base (face-attribute face :height nil t))
                 (scale (seq-reduce (lambda (size face)
                                      (let ((h (cond ((consp face) (plist-get face :height))
                                                     ((and (symbolp face) (facep face))
                                                      (face-attribute face :height nil t)))))
                                        (if (floatp h) (* size h) size)))
                                    (ensure-list faces) 1.0))
                 (size (* scale (/ (if (numberp base) base 120) 10.0)))
                 (bold (memq (lispm-ui--face-attribute faces :weight)
                             '(semi-bold bold extra-bold ultra-bold heavy black)))
                 (synthetic (and bold (display-graphic-p)
                                 (lispm-ui--synthetic-bold-p `(:family ,family))))
                 (ink (or (lispm-ui--face-attribute faces :foreground) color))
                 (lines (delq nil (list (and (lispm-ui--face-attribute faces :underline) "underline")
                                        (and (lispm-ui--face-attribute faces :strike-through)
                                             "line-through")))))
            (lispm-ui-svg-label
             svg run x (+ top (round (/ (+ height (* 0.7 size)) 2.0))) ink
             (list :family (lispm-ui--svg-families family)
                   :size size :weight (if (and bold (not synthetic)) "bold" "normal")
                   :synthetic synthetic
                   :style (and (memq (lispm-ui--face-attribute faces :slant) '(italic oblique))
                               "italic")
                   :decoration (and lines (string-join lines " ")))
             "start"))))
        (setq x (+ x width)
              start end)))
    x))

(defun lispm-ui-content-key (content)
  "Return what tells CONTENT apart from other contents, for memoizing:
its text and its properties, which `equal' alone ignores."
  (cons (substring-no-properties content) (object-intervals content)))

(provide 'lispm-ui-draw)
;;; lispm-ui-draw.el ends here
