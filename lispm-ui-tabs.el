;;; lispm-ui-tabs.el --- Tab rows, drawn in any look  -*- lexical-binding: t; -*-

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

;; Tab rows (`lispm-ui-tabs', `lispm-ui-tabs-width') and look-overridable
;; tab drawing generics (`lispm-ui-draw-tab', `lispm-ui-draw-tab-edge').
;; Extracted from `lispm-ui-controls.el' so each control domain remains
;; concise and focused.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'svg)
(require 'lispm-ui-base)
(require 'lispm-ui-palette)
(require 'lispm-ui-text)
(require 'lispm-ui-draw)

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

(defun lispm-ui--draw-tab-pane-edge (svg width height front palette)
  "Draw the 3-pixel pane edge at the bottom of tab SVG of WIDTH and HEIGHT."
  (let* ((grey (lambda (level) (lispm-ui-grey palette level)))
         (outline (plist-get palette :outline))
         (body (funcall grey (if front #xEE #xCC)))
         (between (funcall grey #xCC))
         (lit (funcall grey (if front #xFF #xDD)))
         (soft (funcall grey (if front #xBB #xAA)))
         (shade-lit (funcall grey (if front #xDD #xCC)))
         (w width)
         (h height))
    (cl-flet ((dot (x y color) (lispm-ui-svg-dot svg x y color))
              (row (from to y color) (lispm-ui-svg-row svg from to y color)))
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
        (row 0 (1- w) (+ h 2) (funcall grey #xFF))))))

(cl-defgeneric lispm-ui-draw-tab (palette label width &key front face close focused)
  "Return an SVG of a tab WIDTH pixels wide reading LABEL, in PALETTE.
Tabs are trapezoids, their sides slanting out a pixel every three rows
from top corners rounded off, and stand on the edge of the pane they
choose among: the SVG is `lispm-ui-tab-height' tall, and
three rows more for that edge.  The tab in FRONT is the pane's colour
and opens into it; a tab behind is darker, lit along its top and left,
and the pane's outline runs under it.  LABEL is a string as Emacs would
show it in FACE, by default the system font's, its icons included; every
label is black, none heavier than the others.
Optional CLOSE draws a close indicator; FOCUSED draws a focus ring."
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
       lispm-ui--tab-slant))
    (lispm-ui--draw-tab-pane-edge svg w h front palette)
    (let* ((room (if close (- w 16) w))
           (tx (max 6 (/ (- room (lispm-ui-text-width label face)) 2))))
      (lispm-ui-svg-content svg label tx 0 h (plist-get palette :text) face))
    (when close
      (lispm-ui-svg-content svg "×" (- w 15) 0 h (plist-get palette :muted) 'lispm-ui-small))
    (when focused
      (svg-rectangle svg 1 1 (- w 2) (- h 2) :fill "none"
                     :stroke (or (plist-get palette :focus) "#2563eb")
                     :stroke-width 1.5))
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
              (lispm-ui-look-image
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
                    (image (lispm-ui-look-image
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

(defun lispm-ui-tabs--spec-parts (spec)
  "Return `(KEY LABEL CLOSABLE)' normalized from tab SPEC."
  (if (and (listp spec) (keywordp (car spec)))
      (list (plist-get spec :key) (plist-get spec :label) (plist-get spec :closable))
    (list (car spec) (cdr spec) nil)))

(defun lispm-ui-tabs--insert-one (spec active keys idx on-select on-close face)
  "Insert one interactive tab from SPEC at IDX with arrow/close bindings."
  (pcase-let* ((`(,key ,label ,closable) (lispm-ui-tabs--spec-parts spec))
               (front (equal key active))
               (close-p (and on-close closable))
               (prev (nth (mod (1- idx) (length keys)) keys))
               (next (nth (mod (1+ idx) (length keys)) keys))
               (map (let ((m (make-sparse-keymap)))
                      (keymap-set m "<left>" (lambda () (interactive)
                                               (when on-select (funcall on-select prev))))
                      (keymap-set m "<right>" (lambda () (interactive)
                                                (when on-select (funcall on-select next))))
                      (keymap-set m "RET" (lambda () (interactive)
                                            (when on-select (funcall on-select key))))
                      (keymap-set m "SPC" (lambda () (interactive)
                                            (when on-select (funcall on-select key))))
                      (keymap-set m "<down-mouse-1>"
                                  (lambda (_e) (interactive "e")
                                    (when on-select (funcall on-select key))))
                      (keymap-set m "<mouse-1>" #'ignore)
                      (when close-p
                        (keymap-set m "C-w" (lambda () (interactive) (funcall on-close key)))
                        (keymap-set m "d" (lambda () (interactive) (funcall on-close key))))
                      m))
               (f (or face 'lispm-ui-title))
               (body
                (if (lispm-ui-tty-p)
                    (propertize (concat " " label " ")
                                'face (if front 'tab-line-tab-current 'tab-line-tab))
                  (let* ((w (+ (* 2 lispm-ui-tab-padding)
                               (lispm-ui-text-width label f)
                               (if close-p 14 0)))
                         (plain (lispm-ui-look-image
                                 (list 'tabs-int (lispm-ui-content-key label) front close-p w f)
                                 (lambda ()
                                   (lispm-ui-draw-tab lispm-ui-current-palette label w
                                                      :front front :face f :close close-p))))
                         (foc (lispm-ui-look-image
                               (list 'tabs-int-f (lispm-ui-content-key label) front close-p w f)
                               (lambda ()
                                 (lispm-ui-draw-tab lispm-ui-current-palette label w
                                                    :front front :face f
                                                    :close close-p :focused t)))))
                    (propertize " " 'display plain
                                'lispm-ui-plain plain 'lispm-ui-focused foc)))))
    (insert (propertize body 'keymap map 'pointer 'hand 'lispm-ui-focusable t
                        'lispm-ui-tab-key key 'lispm-ui-label label))
    (when (and close-p (lispm-ui-tty-p))
      (let ((cmap (let ((m (make-sparse-keymap)))
                    (keymap-set m "RET" (lambda () (interactive) (funcall on-close key)))
                    (keymap-set m "<down-mouse-1>"
                                (lambda (_e) (interactive "e") (funcall on-close key)))
                    (keymap-set m "<mouse-1>" #'ignore)
                    m)))
        (insert (propertize "×" 'keymap cmap 'face 'shadow 'pointer 'hand
                            'lispm-ui-tab-close key))))))

(cl-defun lispm-ui-insert-tabs (specs active on-select &key width on-close face)
  "Insert interactive tabs from SPECS, selecting ACTIVE via ON-SELECT.
SPECS entries are `(KEY . LABEL)' or `(:key K :label L :closable BOOL)'.
Bind Left/Right arrows to switch tabs and optional ON-CLOSE for closeable tabs."
  (ignore width)
  (let ((keys (mapcar (lambda (s) (car (lispm-ui-tabs--spec-parts s))) specs)))
    (cl-loop for spec in specs for idx from 0 do
             (when (> idx 0)
               (insert (propertize " " 'face 'default)))
             (lispm-ui-tabs--insert-one spec active keys idx on-select on-close face))
    (insert "\n")))

(provide 'lispm-ui-tabs)
;;; lispm-ui-tabs.el ends here
