;;; lispm-ui-menu.el --- Menus, drawn as Mac OS 9 drew them  -*- lexical-binding: t; -*-

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

;; A menu is a panel popped up where it is asked for, as Mac OS 9's were:
;; items in the system font, the one under the mouse in the highlight,
;; check marks, icons, separators, key equivalents on the right, and
;; submenus opening beside the items holding them.  It is modal: the
;; arrows, or C-n, C-p, C-f and C-b, move, a letter goes to the item it
;; starts, Return chooses, and Escape, C-g and Command-period cancel; the
;; mouse chooses where it is let go.  On a text terminal a menu is text,
;; and where no popup can show, it is chosen from in the minibuffer.
;;
;; `lispm-ui-menu' pops one up and runs what is chosen, and
;; `lispm-ui-menu-choose' returns it.  `lispm-ui-choose-default' shows a
;; pop-up menu's choices in one, as `lispm-ui-choose' asks it to.  A menu
;; bar walked from the keyboard binds `lispm-ui-menu-bar-step', and its
;; menus step to their neighbours.  A look draws menus its own way with
;; methods of `lispm-ui-draw-menu', laid out by the same rows.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'lispm-ui-base)
(require 'lispm-ui-i18n)
(require 'lispm-ui-palette)
(require 'lispm-ui-text)
(require 'lispm-ui-draw)
(require 'lispm-ui-controls)
(require 'lispm-ui-modal)
(require 'lispm-ui-popup)

(defface lispm-ui-menu-item '((t :inherit lispm-ui-title))
  "Face of a menu item, the system font's, as a menu bar title's."
  :group 'lispm-ui)

(defface lispm-ui-menu-item-highlight '((t :inherit lispm-ui-menu-item :inverse-video t))
  "Face of the menu item under the mouse."
  :group 'lispm-ui)

(defface lispm-ui-menu-item-disabled '((t :inherit lispm-ui-menu-item))
  "Face of a menu item that cannot be chosen: dimmed, as Mac OS 9 greyed it."
  :group 'lispm-ui)

(defun lispm-ui-menu--apply-faces ()
  "Colour the menu item faces in the palette: items in its text on the
chrome, the one under the mouse in the highlight, and those that cannot
be chosen in its dim colour, Mac OS 9's grey for them."
  (when lispm-ui-current-palette
    (set-face-attribute 'lispm-ui-menu-item nil
                        :foreground (lispm-ui-color :text) :background (lispm-ui-color :chrome)
                        :inverse-video nil :box nil)
    (set-face-attribute 'lispm-ui-menu-item-highlight nil
                        :foreground (lispm-ui-color :highlight-text)
                        :background (lispm-ui-color :highlight)
                        :inverse-video nil :box nil)
    (set-face-attribute 'lispm-ui-menu-item-disabled nil
                        :foreground (lispm-ui-color :dim))))

(defconst lispm-ui-menu-item-height 16
  "Height in pixels of a menu item, Mac OS 9's in the system font.")

(defconst lispm-ui-menu-separator-height 6
  "Height in pixels of a menu's separator.")

(defconst lispm-ui-menu-label-x 21
  "Pixels from a menu's inside left edge to its labels: 22 from its
outline, as Mac OS 9 left room for a check mark.")

(defconst lispm-ui-menu-key-margin 16
  "Pixels from the end of a menu's key equivalents to its inside right edge.")

(defvar lispm-ui-menu-small nil
  "Non-nil while menus pop up small, as the Control Strip's modules popped
theirs up on Mac OS 9: in the small system font, items 13 pixels tall,
labels 15 pixels in.  Bind it around `lispm-ui-menu'.")

(defvar lispm-ui-menu-parent nil
  "The frame menus pop up over, or nil for the selected frame.
The Control Strip binds it to the screen, so its menus open there while
the window in front keeps the keyboard.")

(defun lispm-ui-menu--item-height ()
  "Return the height in pixels of a menu item, see `lispm-ui-menu-small';
a line on a text terminal."
  (cond ((lispm-ui-tty-p) 1)
        (lispm-ui-menu-small 13)
        (t lispm-ui-menu-item-height)))

(defun lispm-ui-menu--separator-height ()
  "Return the height in pixels of a menu's separator, a line on a text terminal."
  (if (lispm-ui-tty-p) 1 lispm-ui-menu-separator-height))

(defun lispm-ui-menu--label-x ()
  "Return how far in a menu's labels start, see `lispm-ui-menu-small';
on a text terminal, past a check mark and a blank."
  (cond ((lispm-ui-tty-p) 2)
        (lispm-ui-menu-small 15)
        (t lispm-ui-menu-label-x)))

(defconst lispm-ui-menu-icon-size 16
  "Size in pixels of a menu item's icon, as Mac OS 9's Apple menu drew them.")

(defun lispm-ui-menu--icon-room (items)
  "Return the pixels before the labels of a menu of ITEMS that its items'
icons take: an icon and a gap when one of them has one, else none."
  (cond ((not (seq-some (lambda (item) (and (consp item) (plist-get item :icon))) items)) 0)
        ((lispm-ui-tty-p) 2)
        (t (+ lispm-ui-menu-icon-size 4))))

(defun lispm-ui-menu--key-margin ()
  "Return the blank pixels after a menu's longest label or key, see
`lispm-ui-menu-small'; a cell on a text terminal."
  (cond ((lispm-ui-tty-p) 1)
        (lispm-ui-menu-small 11)
        (t lispm-ui-menu-key-margin)))

(defun lispm-ui-menu--face (enabled)
  "Return the face of a menu item's label, ENABLED or not."
  (cond (lispm-ui-menu-small 'lispm-ui-small)
        (enabled 'lispm-ui-menu-item)
        (t 'lispm-ui-menu-item-disabled)))

(defun lispm-ui-menu--separator-p (item)
  "Return non-nil when menu ITEM is a separator."
  (eq item 'separator))

(defun lispm-ui-menu--enabled-p (item)
  "Return non-nil when menu ITEM can be chosen.
It needs an action, a submenu or a value to return; an item without
:enabled is enabled, one with :enabled nil is not."
  (and (not (lispm-ui-menu--separator-p item))
       (or (plist-get item :action) (plist-get item :submenu) (plist-member item :value))
       (or (not (plist-member item :enabled)) (plist-get item :enabled))))

(defun lispm-ui-menu--super-key-p ()
  "Return non-nil when a key of the keyboard is Super.
On macOS LispM makes Command Meta, which leaves Super to no key, unless
a modifier is made Super; a text terminal sends no Super."
  (or (and (not (lispm-ui-tty-p)) (not (eq window-system 'ns)))
      (seq-some (lambda (variable) (and (boundp variable) (eq (symbol-value variable) 'super)))
                '(ns-command-modifier ns-right-command-modifier ns-alternate-modifier
                                      ns-right-alternate-modifier ns-control-modifier ns-function-modifier))))

(defconst lispm-ui-menu-typed-keys '(return tab escape backspace delete)
  "The keys besides characters that a menu shows beside its items.
LispM is used from an HHKB, which has keys for characters and these
alone: arrows, function keys and the like are Fn chords there, or
missing.")

(defun lispm-ui-menu-keyboard-p (keys)
  "Return non-nil when KEYS is typed on the keyboard alone, as a menu
shows it beside an item: characters, with modifiers or not, and the keys of
`lispm-ui-menu-typed-keys'.  Mouse and menu bar events do not count, nor
arrows or function keys, nor Super keys where no key is Super."
  (let ((super (lispm-ui-menu--super-key-p)))
    (seq-every-p (lambda (event)
                   (let ((base (event-basic-type event)))
                     (and (or (integerp base) (memq base lispm-ui-menu-typed-keys))
                          (or super (not (memq 'super (event-modifiers event)))))))
                 keys)))

(defun lispm-ui-menu--key (item)
  "Return the key equivalent shown for menu ITEM, or nil.
It is the first key sequence typed on the keyboard that runs the item's
command, as Mac OS 9 showed Command keys beside items; an item holding a
submenu shows the triangle pointing to it."
  (or (and (plist-get item :submenu) "▸")
      (plist-get item :key)
      (let ((action (plist-get item :action)))
        (when (and (symbolp action) (commandp action))
          (when-let* ((keys (seq-find #'lispm-ui-menu-keyboard-p (where-is-internal action))))
            (key-description keys))))))

(defun lispm-ui-menu--label-width (label)
  "Return the width in pixels of LABEL in a menu item's face."
  (lispm-ui-text-width label (lispm-ui-menu--face t)))

(defun lispm-ui-menu--width (items)
  "Return the pixel width a menu of ITEMS needs, its edges included."
  (let ((label 0) (key 0))
    (dolist (item items)
      (unless (lispm-ui-menu--separator-p item)
        (setq label (max label (lispm-ui-menu--label-width (plist-get item :label))))
        (setq key (max key (cond ((plist-get item :submenu) (lispm-ui-px 6))
                                 ((lispm-ui-menu--key item)
                                  (lispm-ui-menu--label-width (lispm-ui-menu--key item)))
                                 (t 0))))))
    (+ (lispm-ui-menu--label-x) (lispm-ui-menu--icon-room items)
       label (if (> key 0) (+ (lispm-ui-px 24) key) 0) (lispm-ui-menu--key-margin))))

(defun lispm-ui-menu--rows (items)
  "Return where the rows of a menu of ITEMS start, as (TOPS . HEIGHT).
TOPS lists each item's top in pixels; HEIGHT is the menu's."
  (let ((y 0) (tops nil))
    (dolist (item items)
      (push y tops)
      (setq y (+ y (if (lispm-ui-menu--separator-p item)
                       (lispm-ui-menu--separator-height)
                     (lispm-ui-menu--item-height)))))
    (cons (nreverse tops) y)))

(cl-defgeneric lispm-ui-draw-menu (palette items width highlight)
  "Return an SVG of a menu WIDTH pixels wide listing ITEMS, in PALETTE.
It is drawn as Mac OS 9 drew its menus, inside the outline the menu's
frame draws: lit along its top and left, shaded along its bottom and
right; items sixteen pixels tall, or 13 in a small menu (see
`lispm-ui-menu-small'), a check mark before the checked ones, the
icon of those with an :icon before their label, as the Apple menu's,
their key equivalents flush right and a black triangle on those holding
a submenu; separators engraved across the whole width; disabled items
greyed.  HIGHLIGHT is the index of the item under the mouse, drawn in the
variation colour, light on dark, its edges lit and shaded in it too."
  (pcase-let* ((`(,tops . ,height) (lispm-ui-menu--rows items))
               (svg (svg-create width height))
               (icons (lispm-ui-menu--icon-room items))
               (grey (lambda (level) (lispm-ui-grey palette level)))
               (right (1- width))
               (bottom (1- height)))
    (lispm-ui-svg-fill svg 0 0 width height (funcall grey #xDD))
    (lispm-ui-svg-row svg 0 (1- right) 0 (funcall grey #xFF))
    (lispm-ui-svg-column svg 0 0 (1- bottom) (funcall grey #xFF))
    (lispm-ui-svg-column svg right 0 bottom (funcall grey #x99))
    (lispm-ui-svg-row svg 0 right bottom (funcall grey #x99))
    (seq-do-indexed
     (lambda (item index)
       (let ((top (nth index tops)))
         (if (lispm-ui-menu--separator-p item)
             (progn
               (lispm-ui-svg-row svg 0 right (+ top 2) (funcall grey #x88))
               (lispm-ui-svg-row svg 0 right (+ top 3) (funcall grey #xFF)))
           (let* ((h (lispm-ui-menu--item-height))
                  (lit (eql index highlight))
                  (ink (cond (lit (plist-get palette :highlight-text))
                             ((lispm-ui-menu--enabled-p item) (plist-get palette :text))
                             ;; Mac OS 9's #777777; a dark theme's own
                             ;; dim colour, which its menus show.
                             (t (plist-get palette :dim))))
                  (face (lispm-ui-menu--face (or lit (lispm-ui-menu--enabled-p item))))
                  (key (and (not (plist-get item :submenu)) (lispm-ui-menu--key item))))
             (when lit
               (lispm-ui-svg-fill svg 0 top width h (plist-get palette :highlight))
               (lispm-ui-svg-column svg 0 top (+ top h -1) (plist-get palette :thumb-dark))
               (lispm-ui-svg-column svg right top (+ top h -1) (plist-get palette :highlight-dark))
               (when (= top 0)
                 (lispm-ui-svg-row svg 0 (1- right) 0 (plist-get palette :thumb-dark)))
               (when (= (+ top h) height)
                 (lispm-ui-svg-row svg 0 right bottom (plist-get palette :highlight-dark))))
             (when (plist-get item :checked)
               (lispm-ui-svg-content svg "✓" (if lispm-ui-menu-small 3 4) top h ink face))
             (when-let* ((icon (plist-get item :icon))
                         (size (min lispm-ui-menu-icon-size h)))
               (lispm-ui-svg-emacs-image svg icon (lispm-ui-menu--label-x)
                                         (+ top (/ (- h size) 2)) size size))
             (lispm-ui-svg-content svg (plist-get item :label)
                                   (+ (lispm-ui-menu--label-x) icons)
                                   top h ink face)
             (cond ((plist-get item :submenu)
                    (lispm-ui-svg-pixel-map
                     svg (- width 12) (+ top 2)
                     '("#" "##" "###" "####" "#####" "######" "#####" "####" "###" "##" "#")
                     `((?# . ,ink))))
                   (key
                    (lispm-ui-svg-content
                     svg key (- width (lispm-ui-menu--key-margin) (lispm-ui-menu--label-width key))
                     top h ink face)))))))
     items)
    svg))

(defun lispm-ui-menu--image (items width highlight)
  "Return the image of a menu of ITEMS, see `lispm-ui-draw-menu'."
  (svg-image (lispm-ui-draw-menu lispm-ui-current-palette items width highlight)
             :ascent 100 :scale 1))

(defun lispm-ui-menu--text (items width highlight)
  "Return a menu WIDTH cells wide listing ITEMS on a text terminal, as text.
It holds what `lispm-ui-draw-menu' draws, a line an item: a
check mark before the checked ones, the icon of those with one, the
label, and the key equivalent flush right, or a triangle on an item
holding a submenu; a separator is a rule; disabled items are greyed and
item HIGHLIGHT is in the highlight colour."
  (let ((icons (lispm-ui-menu--icon-room items))
        (margin (lispm-ui-menu--key-margin)))
    (mapconcat
     (lambda (index)
       (let ((item (nth index items)))
         (if (lispm-ui-menu--separator-p item)
             (propertize (make-string width ?─) 'face 'lispm-ui-menu-item-disabled)
           (let* ((face (cond ((eql index highlight) 'lispm-ui-menu-item-highlight)
                              ((lispm-ui-menu--enabled-p item) 'lispm-ui-menu-item)
                              (t 'lispm-ui-menu-item-disabled)))
                  (key (lispm-ui-menu--key item))
                  (lead (concat (if (plist-get item :checked) "✓" " ") " "
                                (when (> icons 0)
                                  (concat (if (stringp (plist-get item :icon)) (plist-get item :icon) " ")
                                          " "))))
                  (room (- width (string-width lead) margin
                           (if key (+ (string-width key) 1) 0)))
                  (label (truncate-string-to-width (plist-get item :label) (max 0 room) nil ?\s "…"))
                  (line (concat lead label (when key (concat " " key)) (make-string margin ?\s))))
             (add-face-text-property 0 (length line) face t line)
             line))))
     (number-sequence 0 (1- (length items)))
     "\n")))

(defun lispm-ui-menu--content (items width highlight)
  "Return what a menu of ITEMS WIDTH pixels wide shows, item HIGHLIGHT lit:
its image, see `lispm-ui-draw-menu', or its text on a text
terminal, see `lispm-ui-menu--text'."
  (if (lispm-ui-tty-p)
      (lispm-ui-menu--text items width highlight)
    (propertize " " 'display (lispm-ui-menu--image items width highlight))))

(defun lispm-ui-menu-open (items &optional x y align)
  "Show a menu of ITEMS placed by X, Y and ALIGN, see `lispm-ui-menu'.
Return its frame; nothing is read from the user."
  (let* ((popup (lispm-ui-popup-make-frame (or lispm-ui-menu-parent (selected-frame))))
         (width (lispm-ui-menu--width items)))
    (lispm-ui-popup-fill popup (lispm-ui-menu--content items width nil))
    (lispm-ui-popup-set-size popup width (cdr (lispm-ui-menu--rows items)))
    (pcase-let ((`(,mx . ,my) (or (and x y (cons x y))
                                  (let ((mouse (cdr (mouse-pixel-position))))
                                    (and (car mouse) mouse))
                                  (cons 0 0))))
      (lispm-ui-popup-show popup (if (eq align 'right) (- mx width) mx) my 'menu))
    popup))

(defun lispm-ui-menu--submenu (item)
  "Return the items of ITEM's submenu, or nil when it has none.
A submenu given as a function is asked for its items only now."
  (let ((submenu (plist-get item :submenu)))
    (if (functionp submenu) (funcall submenu) submenu)))

(defun lispm-ui-menu--step (items from direction)
  "Return the index of the next enabled item of ITEMS from FROM in DIRECTION."
  (lispm-ui-modal-cycle from direction (length items)
                        (lambda (index) (lispm-ui-menu--enabled-p (nth index items)))))

(defun lispm-ui-menu--submenu-place (level index submenu room)
  "Return where the SUBMENU of LEVEL's item INDEX opens, as (X Y ALIGN) for
`lispm-ui-menu--level': beside the item, overlapping the menu's
edge, or on its left when ROOM, the parent frame's width in pixels, has
no room on its right, as Mac OS 9 turned a submenu at the screen's edge."
  (let* ((tty (lispm-ui-tty-p))
         ;; On a text terminal the menus' outlines are a cell outside
         ;; them, and the submenu's stands beside its menu's.
         (right (+ (plist-get level :x) (plist-get level :width) (if tty 1 -2)))
         (y (+ (plist-get level :y) (lispm-ui-menu--row-y level index) -1)))
    (if (<= (+ right (lispm-ui-menu--width submenu)) (- room 2))
        (list right y nil)
      (list (+ (plist-get level :x) (if tty -1 2)) y 'right))))

;; An open menu is a level: its frame, items, width, the item under the
;; mouse, and its place in the parent frame.  A submenu opens as another
;; level beside the item that holds it; the deepest level comes first.

(defun lispm-ui-menu--level (items x y &optional align)
  "Open a menu level of ITEMS at X, Y, flush right at X when ALIGN is `right'."
  (let* ((frame (lispm-ui-menu-open items x y align))
         (position (frame-position frame)))
    (list :frame frame :items items :width (lispm-ui-menu--width items) :highlight nil
          :x (car position) :y (cdr position))))

(defun lispm-ui-menu--draw-level (level)
  "Redraw menu LEVEL with its item under the mouse highlighted."
  (lispm-ui-popup-fill (plist-get level :frame)
                       (lispm-ui-menu--content (plist-get level :items)
                                               (plist-get level :width)
                                               (plist-get level :highlight))))

(defun lispm-ui-menu--row-y (level index)
  "Return how far down menu LEVEL's frame its item INDEX starts, in pixels."
  (nth index (car (lispm-ui-menu--rows (plist-get level :items)))))

(defun lispm-ui-menu--index-at (items y)
  "Return the index of the enabled item of a menu of ITEMS Y pixels down, or nil."
  (let ((index (seq-position (car (lispm-ui-menu--rows items)) y
                             (lambda (top y) (> top y)))))
    (setq index (1- (or index (length items))))
    (when (and (< -1 index (length items)) (< y (cdr (lispm-ui-menu--rows items)))
               (lispm-ui-menu--enabled-p (nth index items)))
      index)))

(defun lispm-ui-menu--level-index (event level)
  "Return the index of the enabled item of LEVEL under EVENT, or nil."
  (when-let* ((frame (plist-get level :frame))
              (position (lispm-ui-modal-over event frame))
              (xy (posn-x-y position)))
    (lispm-ui-menu--index-at (plist-get level :items) (cdr xy))))

(defvar lispm-ui-menu-bar-step nil
  "Non-nil while the menu bar is walked from the keyboard.
Left in a menu's top level, and Right on an item without a submenu,
then leave the menu for the one beside it in the menu bar: the menu
throws `lispm-ui-menu-bar-step' with -1 or 1.")

(defun lispm-ui-menu--item-starting (items from char)
  "Return the index of the next enabled item of ITEMS after FROM whose
label starts with CHAR, ignoring case, or nil."
  (let ((count (length items))
        (prefix (downcase (char-to-string char))))
    (cl-loop for step from 1 to count
             for index = (mod (+ (or from -1) step) count)
             for item = (nth index items)
             when (and (lispm-ui-menu--enabled-p item)
                       (string-prefix-p prefix (downcase (string-trim-left
                                                          (plist-get item :label)))))
             return index)))

(defun lispm-ui-menu (items &optional x y align)
  "Pop up ITEMS as a Mac OS 9 menu and run the action of the one chosen.
X and Y place the menu in the selected frame, by default under the
mouse; ALIGN `right' puts its right edge at X.  Each item is a plist
\(:label STRING :action FUNCTION [:enabled BOOL] [:checked BOOL]
[:key STRING] [:icon IMAGE] [:submenu ITEMS-OR-FUNCTION]) or the symbol
`separator'.
An item with a submenu opens it beside itself, as Mac OS 9's
hierarchical menus did, when the mouse rests on it or Right is typed.
Return the item chosen, or nil when the menu was cancelled."
  (if (not (lispm-ui-popup-available-p))
      (lispm-ui-menu--fallback items)
    (lispm-ui-menu--run (lispm-ui-menu-choose items x y align))))

(defun lispm-ui-menu-choose (items &optional x y align)
  "Pop up ITEMS as `lispm-ui-menu' does, and return the item chosen.
Nothing is run; nil means the menu was cancelled, or had no items."
  (when items
    (let ((levels nil))
      (unwind-protect
          (progn
            (push (lispm-ui-menu--level items x y align) levels)
            (lispm-ui-menu--loop (lambda () levels) (lambda (new) (setq levels new))))
        (dolist (level levels)
          (lispm-ui-popup-dispose (plist-get level :frame)))))))

(defun lispm-ui-menu--loop (get set)
  "Read events for the open menu levels until one item is chosen; return it.
GET returns the levels, deepest first, and SET replaces them; a level
closed is disposed of here.  The keys act on the level in focus, which
the mouse takes along and Right and Left move into a submenu and out:
moving through a menu shows a submenu beside its item, as Mac OS 9 did,
without entering it."
  (let ((focus (car (funcall get))))
    (cl-labels
        ((levels () (funcall get))
         (redraw (level) (lispm-ui-menu--draw-level level) (redisplay t))
         (close-to (level)
           ;; Close every level deeper than LEVEL.
           (let ((open (levels)))
             (while (and open (not (eq (car open) level)))
               (lispm-ui-popup-dispose (plist-get (car open) :frame))
               (setq open (cdr open)))
             (funcall set open)))
         (highlight (level index)
           (unless (eql (plist-get level :highlight) index)
             (close-to level)
             (plist-put level :highlight index)
             (redraw level)
             (let ((item (and index (nth index (plist-get level :items)))))
               (when-let* ((submenu (and item (lispm-ui-menu--submenu item))))
                 (funcall set (cons (apply #'lispm-ui-menu--level submenu
                                           (lispm-ui-menu--submenu-place
                                            level index submenu
                                            (frame-pixel-width (or lispm-ui-menu-parent (selected-frame)))))
                                    (levels)))))))
         (item (level)
           (when-let* ((index (plist-get level :highlight)))
             (nth index (plist-get level :items))))
         (child (level)
           ;; The level opened from LEVEL's highlighted item, if any.
           (let ((open (levels)) (found nil))
             (while (and open (cdr open))
               (when (eq (cadr open) level) (setq found (car open)))
               (setq open (cdr open)))
             found))
         (enter (level)
           ;; Move the focus into the submenu LEVEL's item opened.
           (when-let* ((inner (child level)))
             (setq focus inner)
             (highlight inner (lispm-ui-menu--step (plist-get inner :items) nil 1))))
         (under (event)
           (seq-find (lambda (level) (lispm-ui-modal-over event (plist-get level :frame)))
                     (levels))))
      ;; A menu opened from the keyboard starts on its first item, so
      ;; Return chooses at once.
      (when (lispm-ui-modal-typed-p last-input-event)
        (highlight focus (lispm-ui-menu--step (plist-get focus :items) nil 1)))
      (track-mouse
        (catch 'done
          (while t
            (let* ((event (lispm-ui-modal-read))
                   (role (lispm-ui-modal-role event)))
              (cond
               ((eq role 'cancel) (throw 'done nil))
               ((eq role 'down)
                (highlight focus (lispm-ui-menu--step (plist-get focus :items)
                                                      (plist-get focus :highlight) 1)))
               ((eq role 'up)
                (highlight focus (lispm-ui-menu--step (plist-get focus :items)
                                                      (plist-get focus :highlight) -1)))
               ((eq role 'right)
                (cond ((child focus) (enter focus))
                      (lispm-ui-menu-bar-step (throw 'lispm-ui-menu-bar-step 1))))
               ((eq role 'left)
                (let ((parent (cadr (memq focus (levels)))))
                  (cond (parent
                         (close-to focus)
                         (plist-put focus :highlight nil)
                         (redraw focus)
                         (setq focus parent))
                        (lispm-ui-menu-bar-step (throw 'lispm-ui-menu-bar-step -1)))))
               ((and (characterp event) (>= event ?\s) (/= event ?\d))
                ;; Typing an item's first letter goes to it, as in lists.
                (when-let* ((index (lispm-ui-menu--item-starting
                                    (plist-get focus :items) (plist-get focus :highlight) event)))
                  (highlight focus index)))
               ((eq role 'confirm)
                (let ((chosen (item focus)))
                  (cond ((null chosen))
                        ((lispm-ui-menu--submenu chosen) (enter focus))
                        (t (throw 'done chosen)))))
               ((mouse-movement-p event)
                (when-let* ((level (under event)))
                  (setq focus level)
                  (highlight level (lispm-ui-menu--level-index event level))))
               ((lispm-ui-modal-mouse-down-p event)
                (unless (under event)
                  (throw 'done nil)))
               ((lispm-ui-modal-mouse-up-p event)
                (when-let* ((level (under event))
                            (index (lispm-ui-menu--level-index event level))
                            (chosen (nth index (plist-get level :items))))
                  (unless (lispm-ui-menu--submenu chosen)
                    ;; As Mac OS 9 did, the chosen item blinks once.
                    (lispm-ui-modal-flash 'menu-item
                                          (lambda () (plist-put level :highlight nil) (redraw level))
                                          (lambda () (plist-put level :highlight index) (redraw level)))
                    (sit-for (alist-get 'menu-item lispm-ui-modal-flashes))
                    (throw 'done chosen))))))))))))

(defun lispm-ui-menu--run (item)
  "Run the action of menu ITEM, when there is one; return ITEM.
A menu the action pops up is its own, not a step of a menu bar walk."
  (when-let* ((action (and item (plist-get item :action))))
    (let ((lispm-ui-menu-bar-step nil)
          (lispm-ui-menu-small nil)
          (lispm-ui-menu-parent nil))
      (if (commandp action) (call-interactively action) (funcall action))))
  item)

(defun lispm-ui-menu--fallback (items)
  "Choose among ITEMS in the minibuffer, as a menu where none can be drawn.
Submenus are followed down, their items' labels joined with \" › \"."
  (let ((choices nil))
    (cl-labels ((collect (items prefix)
                  (dolist (item items)
                    (cond ((lispm-ui-menu--submenu item)
                           (collect (lispm-ui-menu--submenu item)
                                    (concat prefix (plist-get item :label) " › ")))
                          ((lispm-ui-menu--enabled-p item)
                           (push (cons (concat prefix (plist-get item :label)) item)
                                 choices))))))
      (collect items ""))
    (let ((label (completing-read (lispm-ui-tr "Menu: ") (reverse choices) nil t)))
      (lispm-ui-menu--run (cdr (assoc label choices))))))

(add-hook 'lispm-ui-palette-changed-hook #'lispm-ui-menu--apply-faces)
(lispm-ui-menu--apply-faces)

;;;; Choices

(defun lispm-ui-choose-default (choices &optional current position label)
  "Open a menu of CHOICES, a list of (VALUE . TEXT), at POSITION, by default
point, CURRENT's item checked; return (VALUE) of the item chosen, or nil.
Where no menu can be drawn, the choices are read in the minibuffer, after
LABEL.  This is how `lispm-ui-choose' shows choices, unless
`lispm-ui-choose-function' says otherwise."
  (if (not (lispm-ui-popup-available-p))
      (let ((text (completing-read (format-prompt (or label "") (cdr (assoc current choices)))
                                   (mapcar #'cdr choices) nil t nil nil
                                   (cdr (assoc current choices)))))
        (when-let* ((choice (rassoc text choices)))
          (list (car choice))))
    (let* ((position (or position (point)))
           (xy (and (pos-visible-in-window-p position nil t)
                    (posn-x-y (posn-at-point position))))
           (item (lispm-ui-menu-choose
                  (mapcar (pcase-lambda (`(,value . ,text))
                            (list :label text :value value :checked (equal value current)))
                          choices)
                  (and xy (+ (car xy) (car (window-inside-pixel-edges))))
                  (and xy (+ (cdr xy) (cadr (window-inside-pixel-edges))
                             lispm-ui-button-height)))))
      (and item (list (plist-get item :value))))))

(provide 'lispm-ui-menu)
;;; lispm-ui-menu.el ends here
