;;; lispm-ui-scroll.el --- Scroll bars, drawn  -*- lexical-binding: t; -*-

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

;; Mac OS 9's scroll bar, in the colours of the palette
;; (lispm-ui-palette.el): a recessed track, a thumb in the variation
;; colour with grip ridges, and the two arrow boxes together at its
;; bottom, as Mac OS 9 placed them with its proportional thumbs.  An
;; inactive bar, and one with nothing to scroll, lies flat.
;;
;;   what shows, of what there is --`lispm-ui-scroll-thumb'--> thumb
;;   height --`lispm-ui-scroll-layout'--> where the track and arrows lie
;;   height, thumb --`lispm-ui-draw-scroll-bar'--> SVG
;;
;; Each is a generic function whose default method is the classic look's
;; (lispm-ui-look.el).  What scrolls, and where the bar is shown, is the
;; caller's: a list in a dialog, a window's edge.

;;; Code:

(require 'cl-lib)
(require 'svg)
(require 'lispm-ui-palette)
(require 'lispm-ui-draw)

(defconst lispm-ui-scroll-width 16
  "Width in pixels of a scroll bar, its outlines included.")

(defconst lispm-ui-scroll-arrow 15
  "Height in pixels of a scroll bar's arrow box, the line above it included.")

(defun lispm-ui-scroll--track (height)
  "Return the length in pixels of the track of a scroll bar HEIGHT tall.
The arrow boxes and the line closing the bar take the rest."
  (- height (* 2 lispm-ui-scroll-arrow) 1))

(cl-defgeneric lispm-ui-scroll-layout (height)
  "Return where the parts of a scroll bar HEIGHT pixels tall lie, a plist:
the track from :track, :track-length pixels long, and the arrow boxes
from :up and :down, each :arrow pixels tall.  Mac OS 9 put the arrows
together at the bar's bottom, under the track."
  (let ((track (lispm-ui-scroll--track height)))
    (list :track 0 :track-length track
          :up track :down (+ track lispm-ui-scroll-arrow)
          :arrow lispm-ui-scroll-arrow)))

(cl-defgeneric lispm-ui-scroll-thumb (height start end min max)
  "Return the thumb of a scroll bar HEIGHT pixels tall, as (TOP . LENGTH).
TOP is counted from the bar's top, see `lispm-ui-scroll-layout'.  What
scrolls shows from START to END of what spans MIN to MAX, as a window
shows a part of its buffer.  The value is nil when everything shows, or
when the track is too short."
  (let* ((track (lispm-ui-scroll--track height))
         (size (- max min))
         (shown (- end start)))
    (when (and (> size 0) (> track 24) (or (> start min) (< end max)))
      (let* ((length (min track (max 16 (round (* track (/ (float shown) size))))))
             (fraction (/ (float (- start min)) (max 1 (- size shown)))))
        (cons (round (* (- track length) (min 1.0 fraction))) length)))))

(defun lispm-ui-scroll--draw-arrow (svg y up palette enabled)
  "Draw a scroll bar's arrow box from row Y of SVG, the line above included.
UP points its arrow up.  An ENABLED box is lit along its top and left
and shaded along its bottom and right, its arrow black; otherwise it is
flat, its arrow grey."
  (let* ((grey (lambda (level) (lispm-ui-grey palette level)))
         (top (1+ y))
         (bottom (+ y lispm-ui-scroll-arrow -1)))
    (lispm-ui-svg-row svg 1 14 y (if enabled (plist-get palette :outline) (funcall grey #x88)))
    (lispm-ui-svg-fill svg 1 top 14 (- bottom y) (funcall grey (if enabled #xDD #xEE)))
    (when enabled
      (lispm-ui-svg-row svg 1 13 top (funcall grey #xFF))
      (lispm-ui-svg-column svg 1 top (1- bottom) (funcall grey #xFF))
      (lispm-ui-svg-row svg 2 14 bottom (funcall grey #xBB))
      (lispm-ui-svg-column svg 14 (1+ top) bottom (funcall grey #xBB)))
    (dotimes (i 4)
      (lispm-ui-svg-row svg (- 7 i) (+ 8 i) (+ top (if up (+ 4 i) (- 7 i)))
                        (if enabled (plist-get palette :outline) (funcall grey #x88))))))

(cl-defgeneric lispm-ui-draw-scroll-bar (palette height thumb active)
  "Return an SVG of a scroll bar HEIGHT pixels tall in PALETTE.
It is `lispm-ui-scroll-width' wide, outlined down both sides: a track
recessed under a lip along its top and left, the thumb in the variation
colour, lit along its top and left, with four grip ridges, then the two
arrow boxes.  THUMB, as returned by `lispm-ui-scroll-thumb', places the
thumb; nil means there is nothing to scroll, and the bar lies flat, its
arrows grey.  An inactive bar, ACTIVE nil, lies flat without its arrows."
  (let* ((w lispm-ui-scroll-width)
         (svg (svg-create w height))
         (grey (lambda (level) (lispm-ui-grey palette level)))
         (outline (plist-get palette :outline))
         (track (lispm-ui-scroll--track height))
         (live (and active thumb)))
    (lispm-ui-svg-fill svg 0 0 w height outline)
    (lispm-ui-svg-fill svg 1 0 (- w 2) height (funcall grey #xEE))
    (when live
      ;; The track, recessed: dark along its top and left, lit along its right.
      (lispm-ui-svg-fill svg 1 0 (- w 2) track (funcall grey #xAA))
      (lispm-ui-svg-row svg 1 13 0 (funcall grey #x77))
      (lispm-ui-svg-column svg 1 0 (1- track) (funcall grey #x77))
      (lispm-ui-svg-row svg 2 12 1 (funcall grey #x88))
      (lispm-ui-svg-column svg 2 1 (1- track) (funcall grey #x88))
      (lispm-ui-svg-column svg 13 1 (1- track) (funcall grey #xBB))
      (lispm-ui-svg-column svg 14 0 (1- track) (funcall grey #xCC))
      ;; The thumb, lit along its top and left, parted from the track
      ;; by a line at either end.
      (let* ((top (car thumb))
             (bottom (+ top (cdr thumb) -1)))
        (lispm-ui-svg-fill svg 1 top 14 (cdr thumb) (plist-get palette :thumb))
        (lispm-ui-svg-row svg 1 13 top (plist-get palette :thumb-light))
        (lispm-ui-svg-column svg 1 top (1- bottom) (plist-get palette :thumb-light))
        (lispm-ui-svg-row svg 2 14 bottom (plist-get palette :thumb-dark))
        (lispm-ui-svg-column svg 14 top bottom (plist-get palette :thumb-dark))
        (when (> top 0)
          (lispm-ui-svg-row svg 1 14 (1- top) outline))
        (when (< (1+ bottom) track)
          (lispm-ui-svg-row svg 1 14 (1+ bottom) outline))
        ;; Four grip ridges across its middle, each lit over shaded.
        (when (>= (cdr thumb) 14)
          (let ((first (- (+ top (/ (cdr thumb) 2)) 4)))
            (dotimes (i 4)
              (let ((y (+ first (* 2 i))))
                (lispm-ui-svg-dot svg 4 y (funcall grey #xEE))
                (lispm-ui-svg-row svg 5 10 y (plist-get palette :thumb-light))
                (lispm-ui-svg-row svg 5 11 (1+ y) (plist-get palette :highlight))))))))
    (when active
      (lispm-ui-scroll--draw-arrow svg track t palette live)
      (lispm-ui-scroll--draw-arrow svg (+ track lispm-ui-scroll-arrow) nil palette live))
    (lispm-ui-svg-row svg 0 (1- w) (1- height) outline)
    svg))

(provide 'lispm-ui-scroll)
;;; lispm-ui-scroll.el ends here
