;;; lispm-ui-palette.el --- The colours LispM UI draws in, from the theme  -*- lexical-binding: t; -*-

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

;; Every control is drawn in the colours of a palette, derived from the
;; colour theme's faces: the editor's background and text, its dimmed
;; text and its highlight.  The palette is a plist, see
;; `lispm-ui-derive-palette'; `lispm-ui-color' reads one of its colours,
;; and `lispm-ui-grey' the counterpart of one of the classic greys.  What
;; shows LispM UI may name colours outright, as a theme in a system's own
;; colours does, with a function of `lispm-ui-palette-functions'.
;;
;; `lispm-ui-palette-update' derives the palette again, as when a theme
;; is enabled (`lispm-ui-palette-follow-theme' has that done), sets the
;; colours of LispM UI's faces and runs `lispm-ui-palette-changed-hook'.
;; Each palette is a generation later than the one before, and what is
;; drawn in it is memoized under its generation, so a theme enabled
;; draws everything anew.  Until a palette is derived there is none.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'seq)
(require 'subr-x)
(require 'lispm-ui-base)

;;;; Colours

(defun lispm-ui-rgb (color)
  "Return COLOR as a list of red, green and blue between 0 and 1, or nil.
A colour written #RRGGBB is read as written, whatever the display can
show; any other name is resolved by the display."
  (if (and (stringp color)
           (string-match (concat "\\`#\\([[:xdigit:]]\\{2\\}\\)"
                                 "\\([[:xdigit:]]\\{2\\}\\)\\([[:xdigit:]]\\{2\\}\\)\\'")
                         color))
      (mapcar (lambda (group) (/ (string-to-number (match-string group color) 16) 255.0))
              '(1 2 3))
    (and color (color-name-to-rgb color))))

(defun lispm-ui-blend (from to ratio)
  "Return the colour RATIO of the way from colour FROM to colour TO.
Each component is rounded to the nearest of its 256 values, so a blend
falling on one of them comes back exactly.  Return nil when either
colour cannot be resolved, as on a text terminal."
  (when-let* ((a (lispm-ui-rgb from))
              (b (lispm-ui-rgb to)))
    (apply #'format "#%02x%02x%02x"
           (cl-mapcar (lambda (x y) (round (* 255 (+ x (* ratio (- y x)))))) a b))))

(defun lispm-ui-luminance (color)
  "Return the relative luminance of COLOR, between 0 and 1."
  (pcase-let ((`(,r ,g ,b) (or (lispm-ui-rgb color) '(0 0 0))))
    (+ (* 0.2126 r) (* 0.7152 g) (* 0.0722 b))))

(defun lispm-ui-legible (background &rest colors)
  "Return whichever of COLORS stands out most from BACKGROUND."
  (let ((ground (lispm-ui-luminance background)))
    (car (seq-sort-by (lambda (color) (abs (- (lispm-ui-luminance color) ground)))
                      #'> colors))))

;;;; The palette

(defvar lispm-ui-current-palette nil
  "The colours LispM UI draws in, a plist derived from the theme, or nil
before one is.  See `lispm-ui-derive-palette'.")

(defvar lispm-ui-palette-generation 0
  "Count of palettes derived so far; memoized drawings carry it in their key.")

(defconst lispm-ui-palette-darkest 0.12
  "The least luminance a palette's chrome and frames are derived from.
They are darker than the content, and a bevel's shaded edge and a
control's outline darker still; a theme on black has no darker to give,
so its content is lifted toward its text until it is this light, and
they are derived from that.")

(defun lispm-ui--palette-ground (content text)
  "Return what a palette's chrome and frames are derived from: CONTENT, or,
darker than `lispm-ui-palette-darkest', CONTENT lifted toward TEXT until
it is as light as that."
  (let ((low (lispm-ui-luminance content))
        (high (lispm-ui-luminance text)))
    (if (or (null (lispm-ui-rgb content))
            (>= low lispm-ui-palette-darkest)
            (<= high lispm-ui-palette-darkest))
        content
      (lispm-ui-blend content text (/ (- lispm-ui-palette-darkest low) (- high low))))))

(defun lispm-ui-derive-palette (content text dim accent)
  "Return the palette for a theme drawing CONTENT, TEXT, DIM and ACCENT.
CONTENT and TEXT are the editor's background and foreground, DIM its
dimmed text, ACCENT its highlight colour.  The chrome is the content a
shade darker; a bevel's lit edge leans toward the text, its shaded edge
toward black, and frames are darker still; controls are outlined in
near black.  The desktop behind the windows is the content tinted with
the accent.  A theme darker than `lispm-ui-palette-darkest', as one on
black, has its chrome and frames derived from its content lifted toward
its text, so that a control's dark edges stand out from them.

As Mac OS 9's variation colour was a ramp, so is the accent: menus are
highlighted in it darkened, scroll thumbs are it lightened, and the
keyboard focus is ringed in it.  The Control Strip has greys of its own,
a little darker than the chrome; alerts are framed in a light red.

Other looks than the classic one draw a few more parts: active captions
\(:title, :title-text) in the highlight, inactive ones (:title-inactive,
:title-inactive-text) in the chrome, sizing borders (:border,
:border-inactive) in the chrome, menus (:menu, :menu-text) on the
document's ground."
  (let* ((ground (lispm-ui--palette-ground content text))
         (chrome (lispm-ui-blend ground "#000000" 0.25))
         (light (lispm-ui-blend chrome text 0.3))
         (outline (lispm-ui-blend ground "#000000" 0.85))
         (highlight (lispm-ui-blend accent "#000000" 0.4))
         (palette (list :content content :text text :dim dim :accent accent
                        :chrome chrome :light light :outline outline)))
    (append
     palette
     (list :desktop (lispm-ui-blend content accent 0.25)
           :header (lispm-ui-blend chrome content 0.5)
           :shade (lispm-ui-blend chrome "#000000" 0.6)
           :frame (lispm-ui-blend ground "#000000" 0.65)
           :muted (lispm-ui-blend dim text 0.35)
           :highlight highlight
           :highlight-dark (lispm-ui-blend highlight "#000000" 0.45)
           :highlight-text (lispm-ui-legible highlight text content)
           :thumb (lispm-ui-blend accent "#FFFFFF" 0.35)
           :thumb-light (lispm-ui-blend accent "#FFFFFF" 0.65)
           :thumb-dark accent
           :focus accent
           :alert (lispm-ui-blend light "#FF0000" 0.4)
           :alert-dark (lispm-ui-blend light "#FF0000" 0.6)
           :strip (lispm-ui-grey palette #xC0)
           :strip-light (lispm-ui-grey palette #xFF)
           :strip-shade (lispm-ui-grey palette #xA0)
           :strip-dark (lispm-ui-grey palette #x80)
           ;; What other looks draw: captions in the highlight, an
           ;; inactive one in the chrome; sizing borders in the chrome;
           ;; menus on the document's own ground.
           :title highlight
           :title-text (lispm-ui-legible highlight text content)
           :title-inactive chrome
           :title-inactive-text (lispm-ui-blend dim text 0.35)
           :border chrome
           :border-inactive chrome
           :menu content
           :menu-text text))))

(defun lispm-ui-grey (palette level)
  "Return the colour of PALETTE standing where the classic grey LEVEL stands.
LEVEL is one of Mac OS 9's greys as a byte, #xDD its chrome, #xFF its
lit edges and #x00 its outlines: the greys between the chrome and white
are taken between PALETTE's :chrome and :light, those below the chrome
between its :chrome and :outline.  With the classic palette every level
comes back as itself, so a control is drawn with the greys Mac OS 9
drew it with, and a theme of other colours gets their counterparts."
  (let ((chrome #xDD))
    (if (>= level chrome)
        (lispm-ui-blend (plist-get palette :chrome) (plist-get palette :light)
                        (/ (- level chrome) (float (- #xFF chrome))))
      (lispm-ui-blend (plist-get palette :chrome) (plist-get palette :outline)
                      (/ (- chrome level) (float chrome))))))

(defun lispm-ui-merge-palette (derived named)
  "Return the palette DERIVED with the colours NAMED put in their place."
  (let ((palette (copy-sequence derived)))
    (cl-loop for (key value) on named by #'cddr
             do (setq palette (plist-put palette key value)))
    palette))

(defun lispm-ui-color (name)
  "Return the colour NAME of the current palette, or nil."
  (plist-get lispm-ui-current-palette name))

;;;; Faces

(defface lispm-ui-title '((t :weight bold))
  "Face of the system font: titles, push buttons, menus."
  :group 'lispm-ui)

(defface lispm-ui-title-inactive '((t :weight bold :inherit shadow))
  "Face of an inactive window's title: the system font, greyed."
  :group 'lispm-ui)

(defface lispm-ui-list nil
  "Face of the application font: lists, labels and fields."
  :group 'lispm-ui)

(defface lispm-ui-small nil
  "Face of the small system font, as Mac OS 9's Geneva 9."
  :group 'lispm-ui)

(defface lispm-ui-field '((t :box (:line-width (1 . 1))))
  "Face of an edit text field: framed in black, in the list colour."
  :group 'lispm-ui)

(defface lispm-ui-field-focused '((t :inherit lispm-ui-field))
  "Face of an edit text field with the keyboard: ringed in the focus colour."
  :group 'lispm-ui)

(defface lispm-ui-button '((t :box (:line-width (1 . 1) :style released-button)))
  "Face of a bevel button."
  :group 'lispm-ui)

(defface lispm-ui-button-pressed '((t :box (:line-width (1 . 1) :style pressed-button)))
  "Face of a bevel button pushed in: a toggle that is on."
  :group 'lispm-ui)

(defun lispm-ui--set-faces ()
  "Colour LispM UI's faces in the current palette.
Their fonts are left as they are: what shows LispM UI sets them, as a
look's families."
  (let ((text (lispm-ui-color :text))
        (chrome (lispm-ui-color :chrome)))
    (set-face-attribute 'lispm-ui-title nil :foreground text)
    (set-face-attribute 'lispm-ui-title-inactive nil
                        :foreground (lispm-ui-color :muted) :inherit 'unspecified)
    (set-face-attribute 'lispm-ui-small nil :inherit 'lispm-ui-list :height 90 :weight 'normal)
    ;; A field is drawn over a theme's own `widget-field': none of the
    ;; lines that theme draws about it are its.
    (set-face-attribute 'lispm-ui-field nil
                        :inherit 'lispm-ui-list :background (lispm-ui-color :content)
                        :foreground text
                        :box `(:line-width (1 . 1) :color ,(lispm-ui-color :outline))
                        :underline nil :overline nil :strike-through nil)
    ;; Emacs draws one box about text, so the ring stands for the frame
    ;; within it while the field has the keyboard.
    (set-face-attribute 'lispm-ui-field-focused nil
                        :inherit 'lispm-ui-field
                        :box `(:line-width (2 . 2) :color ,(lispm-ui-color :focus)))
    (set-face-attribute 'lispm-ui-button nil
                        :inherit 'lispm-ui-list :background chrome :foreground text
                        :box '(:line-width (1 . 1) :style released-button))
    (set-face-attribute 'lispm-ui-button-pressed nil
                        :inherit 'lispm-ui-list :background (lispm-ui-color :shade)
                        :foreground (lispm-ui-color :accent)
                        :box '(:line-width (1 . 1) :style pressed-button))))

;;;; Following the theme

(defvar lispm-ui-palette-functions nil
  "Functions naming colours of the palette outright, as what shows LispM
UI does for a theme in a system's own colours.  Each is called with no
arguments and returns a plist of colours, by the keys of
`lispm-ui-derive-palette', which take the place of those derived; a
later function's colour wins.")

(defvar lispm-ui-palette-changed-hook nil
  "Hook run once a palette is derived, and LispM UI's faces coloured in it,
for what shows LispM UI to colour its own faces.")

(defun lispm-ui-palette-update (&rest _)
  "Derive the palette, and colour LispM UI's faces in it, from the theme.
The colours come from the faces `default', `shadow' and
`font-lock-constant-face', which this never sets, so it may run again,
as on every theme enabled.  Return the palette, or nil, leaving the
current one, when the theme's colours cannot be read."
  (let ((content (face-background 'default nil t))
        (text (face-foreground 'default nil t))
        (dim (face-foreground 'shadow nil t))
        (accent (face-foreground 'font-lock-constant-face nil t)))
    (when (and content text dim accent (color-name-to-rgb content))
      (setq lispm-ui-current-palette
            (lispm-ui-merge-palette (lispm-ui-derive-palette content text dim accent)
                                    (apply #'append (mapcar #'funcall lispm-ui-palette-functions)))
            lispm-ui-palette-generation (1+ lispm-ui-palette-generation))
      (lispm-ui--set-faces)
      (run-hooks 'lispm-ui-palette-changed-hook)
      lispm-ui-current-palette)))

(defun lispm-ui-palette-follow-theme ()
  "Derive the palette now, and again whenever a theme is enabled."
  (add-hook 'enable-theme-functions #'lispm-ui-palette-update)
  (lispm-ui-palette-update))

(provide 'lispm-ui-palette)
;;; lispm-ui-palette.el ends here
