;;; lispm-ui-window.el --- Windows' title bars, drawn  -*- lexical-binding: t; -*-

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

;; A window's title bar as Mac OS 9 drew it, in the colours of the
;; palette (lispm-ui-palette.el): grey, lit along its top and shaded
;; along its bottom, its title centred between engraved ridges, boxes at
;; its ends; an inactive bar lies flat, its title greyed.  A dialog's
;; title bar is one stretch of it, ridged either side of its title and
;; holding no boxes, as a movable modal window's was.
;;
;;   width, title's width, boxes --`lispm-ui-title-layout'--> where the
;;                                  boxes, ridges and title stand
;;   a stretch of it --`lispm-ui-draw-title-part'--> SVG
;;
;; The layout is a generic function whose default method is the classic
;; look's (lispm-ui-look.el).  The boxes themselves, and what pressing
;; them or the title does, are the caller's.

;;; Code:

(require 'cl-lib)
(require 'svg)
(require 'lispm-ui-palette)
(require 'lispm-ui-draw)

(defconst lispm-ui-title-bar-height 21
  "Height in pixels of a title bar: its lit top line to the content's
black line, both included.  The outline above it is drawn by what is
above it: a window's frame, a dialog's outline.")

(defconst lispm-ui-title-box-size 13
  "Side in pixels of a title bar box.")

(defconst lispm-ui-title-box-margin 3
  "Pixels from a title bar's end to the box nearest it, Mac OS 9's.")

(defun lispm-ui-svg-title-ground (svg x width palette active)
  "Draw a title bar's ground on SVG from X, WIDTH pixels wide, in PALETTE.
An ACTIVE bar is grey, lit along its top, shaded along its bottom
over the content's black line; an inactive one lies flat, over a grey
line."
  (let ((grey (lambda (level) (lispm-ui-grey palette level)))
        (h lispm-ui-title-bar-height))
    (lispm-ui-svg-fill svg x 0 width h (funcall grey (if active #xCC #xDD)))
    (if (not active)
        (lispm-ui-svg-fill svg x (1- h) width 1 (funcall grey #x55))
      (lispm-ui-svg-fill svg x 0 width 1 (funcall grey #xFF))
      (lispm-ui-svg-fill svg x (- h 2) width 1 (funcall grey #x99))
      (lispm-ui-svg-fill svg x (1- h) width 1 (plist-get palette :outline)))))

(defun lispm-ui-svg-title-ridges (svg from to palette)
  "Draw a title bar's six ridges on SVG from column FROM to TO, both in.
Each ridge is a lit line over a shaded one, the lit line starting a
pixel further left and the shaded one ending a pixel further right, so
their ends look engraved."
  (when (< from to)
    (dotimes (i 6)
      (let ((y (+ 3 (* 2 i))))
        (lispm-ui-svg-row svg from (1- to) y (lispm-ui-grey palette #xFF))
        (lispm-ui-svg-row svg (1+ from) to (1+ y) (lispm-ui-grey palette #x77))))))

(cl-defun lispm-ui-draw-title-part (palette width &key active ridges label label-x lead trail)
  "Return an SVG of a stretch of title bar WIDTH pixels wide, in PALETTE.
It shows the ground of an ACTIVE or inactive bar, the RIDGES, a list of
\(FROM . TO) columns, and LABEL, a string as Emacs would show it in the
titles' face, its icon included, from column LABEL-X.  LEAD draws the
lit line down the bar's left end, TRAIL the shaded one down its right."
  (let ((svg (svg-create width lispm-ui-title-bar-height))
        (h lispm-ui-title-bar-height))
    (lispm-ui-svg-title-ground svg 0 width palette active)
    (when active
      (when lead
        (lispm-ui-svg-column svg 0 0 (- h 3) (lispm-ui-grey palette #xFF)))
      (when trail
        (lispm-ui-svg-column svg (1- width) 1 (- h 3) (lispm-ui-grey palette #x99)))
      (pcase-dolist (`(,from . ,to) ridges)
        (lispm-ui-svg-title-ridges svg from to palette)))
    (when label
      (lispm-ui-svg-content
       svg label label-x 0 (- h 2)
       (if active (plist-get palette :text) (lispm-ui-grey palette #x77))
       (if active 'lispm-ui-title 'lispm-ui-title-inactive)))
    svg))

(cl-defgeneric lispm-ui-title-layout (width title-width &optional controls margins)
  "Return how a title bar WIDTH pixels wide lays out a title TITLE-WIDTH wide.
CONTROLS, (CLOSE ZOOM COLLAPSE), says which boxes the bar holds, each
non-nil for a box it holds, by default all three; a dialog's bar holds
none.  MARGINS, (LEFT . RIGHT) pixels, stands the boxes nearest the
bar's ends that far in, by default Mac OS 9's
`lispm-ui-title-box-margin'; an end without boxes keeps Mac OS 9's.  A
window's bar stands its boxes clear of where the window system takes a
press as resizing the window.  The value is a plist: the title stretch,
between the boxes, runs from :start to :end; the title, as wide as
:title, starts at :label-x; :ridges lists the (FROM . TO) columns of
the ridges either side; :margins, (LEFT . RIGHT), the bar's ground
before the close box and after the last box.  Spacing is Mac OS 9's:
the close box three pixels in from the edge, four pixels to the ridges,
four from the ridges to the zoom box, three to the collapse box and
three to the edge.  The title is centred in the bar unless that would
push it into the boxes; a title too wide is narrowed so that a short run
of ridges stays on either side."
  (pcase-let* ((`(,close ,zoom ,collapse) (or controls '(on on on)))
               (s lispm-ui-title-box-size)
               (margin lispm-ui-title-box-margin)
               (`(,lead . ,trail) (or margins (cons margin margin)))
               (lead (if close lead 0))
               (trail (if (or zoom collapse) trail margin))
               (start (if close (+ lead s) 0))
               (end (- width trail (if zoom s 0) (if (and zoom collapse) 3 0) (if collapse s 0)))
               (from (+ start (if close 4 7)))
               (to (- end (if (or zoom collapse) 5 8)))
               (pad 6)
               (least 12)
               (title (max 0 (min title-width (- to from (* 2 (+ pad least))))))
               (label-x (max (+ from least pad)
                             (min (/ (- width title) 2) (- to least pad title)))))
    (list :start start :end end :title title :label-x label-x :margins (cons lead trail)
          :ridges (list (cons from (- label-x pad)) (cons (+ label-x title pad) to)))))

(provide 'lispm-ui-window)
;;; lispm-ui-window.el ends here
