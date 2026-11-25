;;; lispm-ui-layout.el --- Compositional layout primitives and split panes -*- lexical-binding: t; -*-

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

;; Compositional layout building blocks working uniformly across GUI and
;; TTY displays:
;;
;; - Stack & spacing: `lispm-ui-spacer', `lispm-ui-hstack' (with flex),
;;   `lispm-ui-vstack', `lispm-ui-divider'.
;; - Containers: `lispm-ui-card', `lispm-ui-insert-card'.
;; - Responsive grid: `lispm-ui-grid-columns', `lispm-ui-grid-move',
;;   `lispm-ui-grid-render'.
;; - Chrome bars: `lispm-ui-toolbar', `lispm-ui-footer-bar' (with
;;   Look-overridable `lispm-ui-draw-toolbar' and `lispm-ui-draw-footer-bar').
;; - Multi-pane layout controller: `lispm-ui-split-create' and pane ops.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'lispm-ui-base)
(require 'lispm-ui-palette)
(require 'lispm-ui-tokens)
(require 'lispm-ui-look)
(require 'lispm-ui-text)
(require 'lispm-ui-draw)
(require 'lispm-ui-icons)
(require 'lispm-ui-controls)

;;;; Measurement & stacks

(defun lispm-ui-layout--as-string (value)
  "Return VALUE as a string while preserving text properties on strings."
  (if (stringp value) value (format "%s" (or value ""))))

(defun lispm-ui-layout--measure (text)
  "Return rendered width of TEXT: columns on TTY, pixels on GUI."
  (if (lispm-ui-tty-p)
      (string-width (or text ""))
    (lispm-ui-text-width (or text ""))))

(defun lispm-ui-layout--gap-units (gap)
  "Return spacing GAP in display units (cells on TTY, pixels on GUI)."
  (let ((px (lispm-ui-space gap)))
    (if (lispm-ui-tty-p)
        (if (zerop px) 0 (max 1 (lispm-ui-px px)))
      px)))

(cl-defun lispm-ui-spacer (&key width height)
  "Return an empty spacer string of WIDTH or HEIGHT (pixels GUI / cells TTY)."
  (cond
   ((and width (> width 0))
    (if (lispm-ui-tty-p)
        (make-string (max 1 width) ?\s)
      (propertize " " 'display `(space :width (,width)))))
   ((and height (> height 0))
    (if (lispm-ui-tty-p)
        (make-string (max 1 (lispm-ui-px height)) ?\n)
      (propertize "\n" 'line-height height)))
   (t "")))

(defun lispm-ui-layout--flex-p (item)
  "Return non-nil when ITEM is a flexible spacer token in `lispm-ui-hstack'."
  (memq item '(flex :flex :spacer spacer)))

(cl-defun lispm-ui-hstack (children &key (gap 'sm) width)
  "Arrange non-nil CHILDREN horizontally with GAP spacing and optional WIDTH.
Elements eq to `flex' / `:spacer' expand evenly when WIDTH is provided."
  (let ((items (delq nil (copy-sequence children))))
    (if (null items)
        ""
      (let* ((unit-gap (lispm-ui-layout--gap-units gap))
             (sep (if (> unit-gap 0) (lispm-ui-spacer :width unit-gap) ""))
             (flex-count (cl-count-if #'lispm-ui-layout--flex-p items))
             (fixed-parts (cl-remove-if #'lispm-ui-layout--flex-p items))
             (fixed-width (+ (apply #'+ 0 (mapcar #'lispm-ui-layout--measure fixed-parts))
                             (* (max 0 (1- (length items))) unit-gap)))
             (extra (if (and width (> flex-count 0))
                        (max 0 (- width fixed-width))
                      0))
             (per-flex (if (> flex-count 0) (/ extra flex-count) 0))
             (rem (if (> flex-count 0) (mod extra flex-count) 0))
             (flex-idx 0)
             (out nil))
        (dolist (item items)
          (push (if (lispm-ui-layout--flex-p item)
                    (let ((w (+ per-flex (if (< flex-idx rem) 1 0))))
                      (setq flex-idx (1+ flex-idx))
                      (if (> w 0) (lispm-ui-spacer :width w) ""))
                  (lispm-ui-layout--as-string item))
                out))
        (string-join (nreverse out) sep)))))

(cl-defun lispm-ui-vstack (children &key (gap 'none) indent)
  "Arrange non-nil CHILDREN vertically with optional GAP and INDENT."
  (let ((items (delq nil (copy-sequence children))))
    (if (null items)
        ""
      (let* ((gap-px (lispm-ui-space gap))
             (sep (cond
                   ((<= gap-px 0) "\n")
                   ((lispm-ui-tty-p) (concat "\n" (make-string (max 1 (lispm-ui-px gap-px)) ?\n)))
                   (t (concat "\n" (propertize "\n" 'line-height gap-px)))))
             (prefix (if (and indent (> indent 0))
                         (lispm-ui-spacer :width (if (lispm-ui-tty-p)
                                                     (lispm-ui-px indent)
                                                   indent))
                       ""))
             (formatted
              (mapcar (lambda (block)
                        (let ((s (lispm-ui-layout--as-string block)))
                          (if (string-empty-p prefix)
                              s
                            (mapconcat (lambda (ln) (concat prefix ln))
                                       (split-string s "\n")
                                       "\n"))))
                      items)))
        (string-join formatted sep)))))

(defun lispm-ui-layout--draw-divider (width color)
  "Return SVG horizontal 1px divider WIDTH pixels wide in COLOR."
  (let* ((w (max 4 width))
         (svg (svg-create w 9)))
    (lispm-ui-svg-row svg 0 (1- w) 4 (or color "#888888"))
    svg))

(cl-defun lispm-ui-divider (&key width label (orientation 'horizontal) color)
  "Return a crisp horizontal or vertical divider string with optional LABEL."
  (let* ((rule-color (or color (lispm-ui-token :border-subtle) (lispm-ui-color :muted) "#888888"))
         (w (or width (if (lispm-ui-tty-p) (max 8 (- (window-body-width) 2)) 320))))
    (cond
     ((eq orientation 'vertical)
      (if (lispm-ui-tty-p)
          (propertize "│" 'face `(:foreground ,rule-color))
        (propertize " " 'display
                    (lispm-ui-look-image
                     (list 'divider-v rule-color)
                     (lambda ()
                       (let ((svg (svg-create 3 16)))
                         (lispm-ui-svg-column svg 1 0 15 rule-color)
                         svg))))))
     ((lispm-ui-tty-p)
      (let* ((face `(:foreground ,rule-color))
             (cells (max 4 (if width width (max 8 (- (window-body-width) 2))))))
        (if (or (null label) (string-empty-p label))
            (propertize (make-string cells ?─) 'face face)
          (let* ((tag (concat " " label " "))
                 (rem (max 2 (- cells (string-width tag))))
                 (left (/ rem 2))
                 (right (- rem left)))
            (concat (propertize (make-string left ?─) 'face face)
                    (propertize tag 'face 'lispm-ui-caption)
                    (propertize (make-string right ?─) 'face face))))))
     ((and label (not (string-empty-p label)))
      (let* ((tag (propertize (concat " " label " ") 'face 'lispm-ui-caption))
             (lw (lispm-ui-text-width tag))
             (side (max 8 (/ (- w lw) 2)))
             (img (lispm-ui-look-image
                   (list 'divider-h side rule-color)
                   (lambda () (lispm-ui-layout--draw-divider side rule-color)))))
        (concat (propertize " " 'display img) tag (propertize " " 'display img))))
     (t
      (propertize " " 'display
                  (lispm-ui-look-image
                   (list 'divider-h w rule-color)
                   (lambda () (lispm-ui-layout--draw-divider w rule-color))))))))

;;;; Card container

(defun lispm-ui-layout--card-tty (body title subtitle header-right footer width)
  "Render BODY card with TTY Unicode box borders."
  (let* ((cols (max 20 (or width (max 24 (- (window-body-width) 4)))))
         (inner (max 10 (- cols 4)))
         (border-face `(:foreground ,(or (lispm-ui-token :border-strong) "grey")))
         (top-title (if title (concat " " (truncate-string-to-width title (- inner 2) nil nil "…") " ") ""))
         (top (concat (propertize "╭─" 'face border-face)
                      (propertize top-title 'face 'lispm-ui-h4)
                      (propertize (concat (make-string (max 0 (- cols 3 (string-width top-title))) ?─) "╮")
                                  'face border-face)))
         (lines nil))
    (cl-flet ((box-line (txt)
                (let* ((raw (or txt ""))
                       (fit (truncate-string-to-width raw inner nil ?\s "…")))
                  (concat (propertize "│ " 'face border-face)
                          fit
                          (propertize " │" 'face border-face)))))
      (push top lines)
      (when (or subtitle header-right)
        (push (box-line (lispm-ui-hstack (list subtitle 'flex header-right) :width inner)) lines)
        (push (concat (propertize "├" 'face border-face)
                      (propertize (make-string (- cols 2) ?─) 'face border-face)
                      (propertize "┤" 'face border-face))
              lines))
      (dolist (ln (split-string (or body "") "\n"))
        (push (box-line ln) lines))
      (when footer
        (push (concat (propertize "├" 'face border-face)
                      (propertize (make-string (- cols 2) ?─) 'face border-face)
                      (propertize "┤" 'face border-face))
              lines)
        (dolist (ln (split-string (lispm-ui-layout--as-string footer) "\n"))
          (push (box-line ln) lines)))
      (push (concat (propertize "╰" 'face border-face)
                    (propertize (make-string (- cols 2) ?─) 'face border-face)
                    (propertize "╯" 'face border-face))
            lines)
      (string-join (nreverse lines) "\n"))))

(cl-defun lispm-ui-card (body &key title subtitle header-right footer width
                              (padding 'md) (surface :surface-1) (border :border-subtle))
  "Render BODY inside a styled card with optional TITLE, HEADER-RIGHT, and FOOTER."
  (if (lispm-ui-tty-p)
      (lispm-ui-layout--card-tty body title subtitle header-right footer width)
    (let* ((pad-px (lispm-ui-space padding))
           (w (or width 420))
           (inner-w (max 80 (- w (* 2 pad-px))))
           (bg (or (lispm-ui-token surface) (lispm-ui-color :content)))
           (bc (or (lispm-ui-token border) (lispm-ui-color :muted) "#cccccc"))
           (pad-sp (if (> pad-px 0) (lispm-ui-spacer :width pad-px) ""))
           (rule (lispm-ui-divider :width inner-w :color bc))
           (head (when (or title subtitle header-right)
                   (lispm-ui-hstack
                    (list (when title (propertize title 'face 'lispm-ui-h3))
                          (when subtitle (propertize subtitle 'face 'lispm-ui-caption))
                          'flex
                          header-right)
                    :gap 'sm :width inner-w)))
           (sections (delq nil (list rule
                                     head
                                     (and head (lispm-ui-divider :width inner-w :color bc))
                                     (or body "")
                                     (and footer (lispm-ui-divider :width inner-w :color bc))
                                     footer
                                     rule)))
           (joined (lispm-ui-vstack sections :gap 'xs)))
      (mapconcat (lambda (ln)
                   (let ((row (concat pad-sp ln pad-sp)))
                     (if bg
                         (propertize row 'font-lock-face `(:background ,bg :extend t))
                       row)))
                 (split-string joined "\n")
                 "\n"))))

(cl-defun lispm-ui-insert-card (body &rest args)
  "Insert a rendered `lispm-ui-card' for BODY and ARGS at point."
  (insert (apply #'lispm-ui-card body args) "\n"))

;;;; Responsive grid

(defun lispm-ui-grid-columns (total-width cell-width &optional gap)
  "Return number of grid columns fitting CELL-WIDTH cells into TOTAL-WIDTH."
  (let ((g (max 0 (or gap 0)))
        (cw (max 1 (or cell-width 1))))
    (max 1 (/ (+ (max 0 (or total-width cw)) g) (+ cw g)))))

(defun lispm-ui-grid-move (index direction count columns)
  "Return next item index moving from INDEX in DIRECTION across COUNT/COLUMNS."
  (when (> count 0)
    (let* ((cols (max 1 columns))
           (idx (if (integerp index) (max 0 (min index (1- count))) 0)))
      (pcase direction
        ('right (min (1- count) (1+ idx)))
        ('left (max 0 (1- idx)))
        ('down (min (1- count) (+ idx cols)))
        ('up (max 0 (- idx cols)))
        ('home 0)
        ('end (1- count))
        (_ idx)))))

(defun lispm-ui-layout--grid-cell (item idx key selected-key render-fn cell-w on-select on-activate)
  "Render grid cell ITEM at IDX with selection and activation bindings."
  (let* ((sel (and key (equal key selected-key)))
         (raw (cond
               (render-fn (funcall render-fn item sel idx))
               ((stringp item) item)
               (t (format "%s" item))))
         (padded (if (lispm-ui-tty-p)
                     (truncate-string-to-width raw (max 4 cell-w) nil ?\s "…")
                   (let* ((rw (lispm-ui-text-width raw))
                          (pad (max 0 (- cell-w rw))))
                     (if (> pad 0) (concat raw (lispm-ui-spacer :width pad)) raw))))
         (map (let ((m (make-sparse-keymap)))
                (when (or on-select on-activate)
                  (keymap-set m "RET"
                              (lambda () (interactive)
                                (when on-activate (funcall on-activate item idx))))
                  (keymap-set m "<down-mouse-1>"
                              (lambda (ev)
                                (interactive "e")
                                (if (memq 'double (event-modifiers ev))
                                    (when on-activate (funcall on-activate item idx))
                                  (when on-select (funcall on-select item idx))))))
                (keymap-set m "<mouse-1>" #'ignore)
                m)))
    (propertize padded
                'lispm-ui-item-key key
                'lispm-ui-grid-index idx
                'lispm-ui-focusable t
                'pointer (when (or on-select on-activate) 'hand)
                'keymap map
                'face (when sel `(:background ,(or (lispm-ui-token :selected-bg) "#2563eb")
                                  :foreground ,(or (lispm-ui-token :selected-fg) "#ffffff"))))))

(cl-defun lispm-ui-grid-render (items &key width (cell-width 120) (gap 'sm) render-item
                                      selected-key key-fn on-select on-activate columns)
  "Render ITEMS as a responsive multi-column grid and return the string."
  (if (null items)
      ""
    (let* ((tty (lispm-ui-tty-p))
           (total (or width (if tty (max 20 (window-body-width)) 600)))
           (cw (if tty (max 4 (if (<= cell-width 40) cell-width (lispm-ui-px cell-width)))
                 cell-width))
           (gap-units (lispm-ui-layout--gap-units gap))
           (cols (or columns (lispm-ui-grid-columns total cw gap-units)))
           (kfn (or key-fn #'identity))
           (rows nil)
           (row nil)
           (idx 0))
      (dolist (item items)
        (push (lispm-ui-layout--grid-cell
               item idx (funcall kfn item) selected-key render-item cw on-select on-activate)
              row)
        (setq idx (1+ idx))
        (when (= (length row) cols)
          (push (lispm-ui-hstack (nreverse row) :gap (if tty gap-units gap)) rows)
          (setq row nil)))
      (when row
        (push (lispm-ui-hstack (nreverse row) :gap (if tty gap-units gap)) rows))
      (lispm-ui-vstack (nreverse rows) :gap gap))))

;;;; Look-customizable Toolbar & FooterBar

(cl-defgeneric lispm-ui-draw-toolbar (palette width &optional label height)
  "Return SVG of a toolbar background WIDTH pixels wide in PALETTE."
  (let* ((h (max 16 (or height 24)))
         (w (max 8 width))
         (svg (svg-create w h))
         (grey (lambda (level) (lispm-ui-grey palette level))))
    (lispm-ui-svg-fill svg 0 0 w h (funcall grey #xDD))
    (lispm-ui-svg-row svg 0 (1- w) 0 (funcall grey #xFF))
    (lispm-ui-svg-row svg 0 (1- w) (- h 2) (funcall grey #xAA))
    (lispm-ui-svg-row svg 0 (1- w) (1- h) (or (plist-get palette :outline) "#000000"))
    (when (and label (not (string-empty-p label)))
      (let ((x (max 8 (/ (- w (lispm-ui-text-width label 'lispm-ui-small)) 2))))
        (lispm-ui-svg-content svg label x 1 (- h 3)
                              (or (plist-get palette :text) "#000000")
                              'lispm-ui-small)))
    svg))

(cl-defgeneric lispm-ui-draw-footer-bar (palette width &optional label height)
  "Return SVG of a footer bar background WIDTH pixels wide in PALETTE."
  (lispm-ui-draw-toolbar palette width label height))

(defun lispm-ui-layout--bar-item (item)
  "Render one toolbar/footer ITEM (string or plist spec)."
  (if (not (and (listp item) (keywordp (car item))))
      (lispm-ui-layout--as-string item)
    (let* ((label (or (plist-get item :label) ""))
           (icon (when-let* ((name (plist-get item :icon)))
                   (lispm-ui-icon name)))
           (active (plist-get item :active))
           (disabled (plist-get item :disabled))
           (action (plist-get item :action))
           (text (string-join (delq nil (list icon (unless (string-empty-p label) label))) " "))
           (map (when (and action (not disabled))
                  (let ((m (make-sparse-keymap)))
                    (keymap-set m "RET" (lambda () (interactive) (funcall action)))
                    (keymap-set m "SPC" (lambda () (interactive) (funcall action)))
                    (keymap-set m "<down-mouse-1>"
                                (lambda (_ev) (interactive "e") (funcall action)))
                    (keymap-set m "<mouse-1>" #'ignore)
                    (keymap-set m "<header-line> <mouse-1>"
                                (lambda (_ev) (interactive "e") (funcall action)))
                    (keymap-set m "<mode-line> <mouse-1>"
                                (lambda (_ev) (interactive "e") (funcall action)))
                    m))))
      (propertize (concat " " text " ")
                  'face (cond
                         (disabled 'shadow)
                         (active 'lispm-ui-button-pressed)
                         (t 'lispm-ui-button))
                  'lispm-ui-focusable (and action (not disabled) t)
                  'pointer (when (and action (not disabled)) 'hand)
                  'help-echo (or (plist-get item :help) label)
                  'keymap map))))

(defun lispm-ui-layout--bar-group (items)
  "Join toolbar/footer ITEMS with compact gaps."
  (when items
    (let* ((list (if (or (atom items) (keywordp (car items)))
                     (list items)
                   items))
           (rendered (mapcar #'lispm-ui-layout--bar-item list)))
      (lispm-ui-hstack rendered :gap 'xs))))

(cl-defun lispm-ui-toolbar (&key left center right label width (height 24))
  "Compose a horizontal toolbar string with LEFT, CENTER, and RIGHT groups.
When only LABEL is given on a graphical display with a palette, draw a
Look-styled toolbar image via `lispm-ui-draw-toolbar'."
  (let* ((tty (lispm-ui-tty-p))
         (total (or width (if tty (max 20 (window-body-width)) 640))))
    (if (and (not tty) lispm-ui-current-palette
             (null left) (null right) (null center))
        (propertize " " 'display
                    (lispm-ui-look-image
                     (list 'toolbar total label height)
                     (lambda ()
                       (lispm-ui-draw-toolbar lispm-ui-current-palette total label height))))
      (let* ((l (lispm-ui-layout--bar-group left))
             (c (lispm-ui-layout--bar-group (or center label)))
             (r (lispm-ui-layout--bar-group right))
             (content (lispm-ui-hstack (delq nil (list l 'flex c 'flex r))
                                       :gap 'sm :width total))
             (bg (or (lispm-ui-color :header) (lispm-ui-token :surface-2))))
        (if (and (not tty) bg)
            (propertize content 'face `(:background ,bg :extend t))
          content)))))

(cl-defun lispm-ui-footer-bar (&key left center right label width (height 24))
  "Compose a horizontal footer bar string with LEFT, CENTER, and RIGHT groups."
  (let* ((tty (lispm-ui-tty-p))
         (total (or width (if tty (max 20 (window-body-width)) 640))))
    (if (and (not tty) lispm-ui-current-palette
             (null left) (null right) (null center))
        (propertize " " 'display
                    (lispm-ui-look-image
                     (list 'footer-bar total label height)
                     (lambda ()
                       (lispm-ui-draw-footer-bar lispm-ui-current-palette total label height))))
      (lispm-ui-toolbar :left left :center center :right right
                        :label label :width total :height height))))

;;;; General SplitPane controller

(cl-defstruct (lispm-ui-split
               (:constructor lispm-ui-split--create)
               (:copier nil)
               (:predicate lispm-ui-split-p))
  "Multi-pane split layout controller (2-pane or 3-pane master-detail shell)."
  (frame nil)
  (main-window nil)
  (left-window nil)
  (right-window nil)
  (bottom-window nil)
  (left-width 28 :type integer)
  (right-width 40 :type integer)
  (bottom-height 12 :type integer)
  (min-main-width 40 :type integer)
  (panes nil :type list))

(cl-defun lispm-ui-split-create (&key frame main-window (left-width 28)
                                      (right-width 40) (bottom-height 12)
                                      (min-main-width 40))
  "Create a multi-pane `lispm-ui-split' controller around MAIN-WINDOW on FRAME."
  (let ((win (or main-window (selected-window))))
    (lispm-ui-split--create
     :frame (or frame (and (window-live-p win) (window-frame win)) (selected-frame))
     :main-window win
     :left-width left-width
     :right-width right-width
     :bottom-height bottom-height
     :min-main-width min-main-width)))

(defun lispm-ui-split-pane-window (split side)
  "Return live window assigned to SIDE (`left', `right', `bottom') in SPLIT."
  (let ((win (pcase side
               ('left (lispm-ui-split-left-window split))
               ('right (lispm-ui-split-right-window split))
               ('bottom (lispm-ui-split-bottom-window split))
               ('main (lispm-ui-split-main-window split)))))
    (and (window-live-p win) win)))

(defun lispm-ui-split--set-pane-window (split side win)
  "Store WIN as SIDE's window in SPLIT."
  (pcase side
    ('left (setf (lispm-ui-split-left-window split) win))
    ('right (setf (lispm-ui-split-right-window split) win))
    ('bottom (setf (lispm-ui-split-bottom-window split) win))))

(defun lispm-ui-split-pane-visible-p (split side)
  "Return non-nil when SIDE pane has a live displayed window in SPLIT."
  (not (null (lispm-ui-split-pane-window split side))))

(defun lispm-ui-split--default-size (split side)
  "Return configured dimension for SIDE in SPLIT."
  (pcase side
    ('left (lispm-ui-split-left-width split))
    ('right (lispm-ui-split-right-width split))
    ('bottom (lispm-ui-split-bottom-height split))))

(defun lispm-ui-split-open (split side buffer &optional size)
  "Open or update SIDE (`left', `right', `bottom') pane showing BUFFER in SPLIT."
  (when (and (lispm-ui-split-p split) (buffer-live-p buffer))
    (let* ((anchor (if (window-live-p (lispm-ui-split-main-window split))
                       (lispm-ui-split-main-window split)
                     (selected-window)))
           (req (or size
                    (plist-get (alist-get side (lispm-ui-split-panes split)) :size)
                    (lispm-ui-split--default-size split side)))
           (existing (lispm-ui-split-pane-window split side))
           (max-w (if (memq side '(left right))
                      (- (window-total-width anchor)
                         (lispm-ui-split-min-main-width split))
                    most-positive-fixnum))
           (eff-req (if (eq side 'bottom) (max 3 req) (min (max 8 req) (max 0 max-w))))
           (win (or existing
                    (when (or (eq side 'bottom) (>= max-w 8))
                      (condition-case nil
                          (pcase side
                            ('left (split-window anchor (- eff-req) 'left))
                            ('right (split-window anchor (- eff-req) 'right))
                            ('bottom (split-window anchor (- eff-req) 'below)))
                        (error
                         (ignore-errors
                           (split-window anchor nil (if (eq side 'bottom) 'below side)))))))))
      (when (window-live-p win)
        (set-window-parameter win 'lispm-ui-split split)
        (set-window-parameter win 'lispm-ui-split-side side)
        (set-window-buffer win buffer)
        (lispm-ui-split--set-pane-window split side win)
        (setf (alist-get side (lispm-ui-split-panes split))
              (list :buffer buffer :size req :visible t)))
      win)))

(defun lispm-ui-split-toggle (split side &optional buffer)
  "Toggle visibility of SIDE pane in SPLIT, displaying BUFFER when opening."
  (when (lispm-ui-split-p split)
    (if-let* ((win (lispm-ui-split-pane-window split side)))
        (let ((cur-buf (window-buffer win)))
          (setf (alist-get side (lispm-ui-split-panes split))
                (list :buffer cur-buf
                      :size (lispm-ui-split--default-size split side)
                      :visible nil))
          (lispm-ui-split--set-pane-window split side nil)
          (when (> (length (window-list (window-frame win) 'nomini)) 1)
            (ignore-errors (delete-window win)))
          nil)
      (let ((buf (or buffer (plist-get (alist-get side (lispm-ui-split-panes split)) :buffer))))
        (when (buffer-live-p buf)
          (lispm-ui-split-open split side buf))))))

(defun lispm-ui-split-set-buffer (split side buffer)
  "Display BUFFER in SIDE pane of SPLIT, opening the pane if needed."
  (if (eq side 'main)
      (when (and (window-live-p (lispm-ui-split-main-window split))
                 (buffer-live-p buffer))
        (set-window-buffer (lispm-ui-split-main-window split) buffer))
    (lispm-ui-split-open split side buffer)))

(defun lispm-ui-split-resize (split side size)
  "Update target SIZE (columns or rows) for SIDE pane in SPLIT."
  (when (and (lispm-ui-split-p split) (integerp size) (> size 0))
    (pcase side
      ('left (setf (lispm-ui-split-left-width split) size))
      ('right (setf (lispm-ui-split-right-width split) size))
      ('bottom (setf (lispm-ui-split-bottom-height split) size)))
    (when-let* ((entry (alist-get side (lispm-ui-split-panes split))))
      (plist-put entry :size size))
    (when-let* ((win (lispm-ui-split-pane-window split side)))
      (ignore-errors
        (if (eq side 'bottom)
            (window-resize win (- size (window-total-height win)) nil)
          (window-resize win (- size (window-total-width win)) t))))
    size))

(defun lispm-ui-split-dispose (split)
  "Close all side windows managed by SPLIT safely."
  (when (lispm-ui-split-p split)
    (dolist (side '(left right bottom))
      (when-let* ((win (lispm-ui-split-pane-window split side)))
        (lispm-ui-split--set-pane-window split side nil)
        (when (> (length (window-list (window-frame win) 'nomini)) 1)
          (ignore-errors (delete-window win)))))
    (setf (lispm-ui-split-panes split) nil))
  nil)

(provide 'lispm-ui-layout)
;;; lispm-ui-layout.el ends here
