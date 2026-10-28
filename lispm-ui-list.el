;;; lispm-ui-list.el --- Lists' column headings and disclosure triangles  -*- lexical-binding: t; -*-

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

;; The parts of a list view, as Mac OS 9's Finder drew them, in the
;; colours of the palette (lispm-ui-palette.el): the row of column
;; headings at a list's top, the column it is sorted by pushed in, a
;; button turning the order over at its right end; and the disclosure
;; triangle before a group of rows, pointing right at a closed group and
;; down at an open one.
;;
;; Each is drawn as SVG by a generic function whose default method draws
;; the classic look (lispm-ui-look.el).  The triangle is also a string
;; displaying it, `lispm-ui-triangle', which on a text terminal is a
;; character.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'lispm-ui-base)
(require 'lispm-ui-palette)
(require 'lispm-ui-draw)
(require 'lispm-ui-controls)

;;;; Column headings

(defconst lispm-ui-list-header-height 21
  "Height in pixels of a list's row of column headings.")

(defconst lispm-ui-list--sort-glyph
  '("      #" "     ##" "    ###" "   ####" "  #####" " ######" "#######")
  "The glyph of a list's sort-direction button, a row of pixels a string.")

(cl-defgeneric lispm-ui-draw-list-header (palette columns &optional label-x)
  "Return an SVG of a list's row of column headings, in PALETTE.
COLUMNS lists (LABEL WIDTH SORTED [X]) from left to right; a
sixteen-pixel sort-direction button ends the row.  Each heading is a
bevelled cell, its label in the list's font X pixels in, by default
LABEL-X or eight; the heading of the SORTED column is drawn pushed in
and darker, as Mac OS 9 drew the column a list was sorted by."
  (let* ((h lispm-ui-list-header-height)
         (width (+ 16 (apply #'+ (mapcar #'cadr columns))))
         (svg (svg-create width h))
         (grey (lambda (level) (lispm-ui-grey palette level)))
         (x 0))
    (pcase-dolist (`(,label ,w ,sorted ,x-in) (append columns '((nil 16 nil))))
      (cl-flet ((row (y level) (lispm-ui-svg-row svg x (+ x w -1) y (funcall grey level)))
                (column (cx level) (lispm-ui-svg-column svg (+ x cx) 0 (1- h) (funcall grey level))))
        (if sorted
            (progn
              (lispm-ui-svg-fill svg x 0 w h (funcall grey #x88))
              (row (- h 2) #xAA) (column (- w 2) #xAA)
              (row (1- h) #x44) (column (1- w) #x44)
              (row 1 #x55) (column 1 #x55)
              (row 0 #x11) (column 0 #x11))
          (lispm-ui-svg-fill svg x 0 w h (funcall grey #xCC))
          (row (- h 2) #x88) (row (1- h) #x33)
          (row 1 #xFF) (column 1 #xFF)
          (row 0 #x66) (column 0 #x66)))
      (when label
        (lispm-ui-svg-content svg label (+ x (or x-in label-x 8)) 1 (- h 3)
                              (plist-get palette :text) 'lispm-ui-list))
      (setq x (+ x w)))
    (lispm-ui-svg-pixel-map svg (- width 12) 7 lispm-ui-list--sort-glyph
                            `((?# . ,(plist-get palette :text))))
    svg))

;;;; Disclosure triangles

(defconst lispm-ui-list--triangle
  '("O" "OO" "OLO" "OLFO" "OLFFO" "OLFFDO" "OLFDOSs" "OLDOSs" "OFOSs" "OOSs" "OSs" " s")
  "Mac OS 9's disclosure triangle pointing right, a row of pixels a string.
O is the outline, L, F and D the light, face and dark of the variation
colour, S and s the shadow falling to the lower right.")

(cl-defgeneric lispm-ui-draw-triangle (palette open)
  "Return an SVG of a list's disclosure triangle in PALETTE.
OPEN points it down, at a disclosed group; otherwise it points right.
It is drawn as Mac OS 9 drew it: outlined, in the variation colour lit
from the left, casting a shadow; pointing down, it is the same triangle
turned over its diagonal."
  (let ((svg (svg-create 12 12))
        (colors `((?O . ,(plist-get palette :outline)) (?L . ,(plist-get palette :thumb-light))
                  (?F . ,(plist-get palette :thumb)) (?D . ,(plist-get palette :thumb-dark))
                  (?S . ,(lispm-ui-grey palette #x88))
                  (?s . ,(lispm-ui-grey palette #xBB)))))
    (seq-do-indexed
     (lambda (row y)
       (seq-do-indexed
        (lambda (pixel x)
          (when-let* ((color (alist-get pixel colors)))
            (if open
                (lispm-ui-svg-dot svg y (+ x 3) color)
              (lispm-ui-svg-dot svg (+ x 3) y color))))
        row))
     lispm-ui-list--triangle)
    svg))

(defun lispm-ui-triangle (open)
  "Return a disclosure triangle as a string, pointing down when OPEN.
On a text terminal it is a character, a cell wide."
  (if (lispm-ui-tty-p)
      (if open "▾" "▸")
    (propertize " " 'display (lispm-ui-image
                              (list 'triangle open)
                              (lambda () (lispm-ui-draw-triangle lispm-ui-current-palette open))))))

(provide 'lispm-ui-list)
;;; lispm-ui-list.el ends here
