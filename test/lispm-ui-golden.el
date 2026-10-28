;;; lispm-ui-golden.el --- What LispM UI draws, as the desktop drew it  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
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

;; LispM UI's palette, drawing and text came from LispM Desktop, where
;; they were lispm-desktop-platinum-*, lispm-desktop-look-* and
;; lispm-desktop-i18n-*.  What they make, palettes, SVG documents and
;; lines, must be what the desktop's made, so that the desktop draws the
;; same once it draws with LispM UI.
;;
;; `lispm-ui-golden-cases' are written in LispM UI's names.
;; `lispm-ui-golden-write' evaluates each in the desktop's names, by
;; `lispm-ui-golden-names', in a desktop checkout from before the move,
;; and writes what they make to test/golden.eld:
;;
;;   make golden LISPM_DESKTOP=../lispm-desktop/lisp
;;
;; with LispM Desktop at 42a18a9, its last commit drawing on its own, and
;; LispM UI on the load path, whose modal kit it requires.  The
;; tests evaluate the same cases in LispM UI's names and find the same.
;; Whatever depends on the fonts installed is drawn with none, so that
;; any computer makes the same.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'svg)

(defconst lispm-ui-golden-names
  '(;; lispm-ui-palette.el
    (lispm-ui-rgb . lispm-desktop-platinum-rgb)
    (lispm-ui-blend . lispm-desktop-platinum-blend)
    (lispm-ui-luminance . lispm-desktop-platinum--luminance)
    (lispm-ui-legible . lispm-desktop-platinum-legible)
    (lispm-ui-current-palette . lispm-desktop-platinum-current-palette)
    (lispm-ui-palette-generation . lispm-desktop-platinum-generation)
    (lispm-ui-derive-palette . lispm-desktop-platinum-derive-palette)
    (lispm-ui-grey . lispm-desktop-platinum-grey)
    (lispm-ui-merge-palette . lispm-desktop-platinum--merge-palette)
    (lispm-ui-color . lispm-desktop-platinum-color)
    (lispm-ui-title . lispm-desktop-platinum-title)
    (lispm-ui-list . lispm-desktop-platinum-list)
    (lispm-ui-small . lispm-desktop-platinum-small)
    (lispm-ui-field . lispm-desktop-platinum-field)
    (lispm-ui-field-focused . lispm-desktop-platinum-field-focused)
    (lispm-ui-button . lispm-desktop-platinum-button)
    (lispm-ui-button-pressed . lispm-desktop-platinum-button-pressed)
    ;; lispm-ui-look.el
    (lispm-ui-looks . lispm-desktop-looks)
    (lispm-ui-look-define . lispm-desktop-look-define)
    (lispm-ui-look-get . lispm-desktop-look-get)
    (lispm-ui-look-title . lispm-desktop-look-title)
    (lispm-ui-look-family . lispm-desktop-look-family)
    (lispm-ui-look-metric . lispm-desktop-look-metric)
    (lispm-ui-look-families . lispm-desktop-look-families)
    ;; lispm-ui-text.el
    (lispm-ui-text-width . lispm-desktop-platinum-content-width)
    (lispm-ui-text-cjk-families . lispm-desktop-i18n-cjk-families)
    (lispm-ui-text-cjk-family . lispm-desktop-i18n-cjk-family)
    (lispm-ui-text-words . lispm-desktop-i18n-words)
    (lispm-ui-text-wrap . lispm-desktop-i18n-wrap)
    ;; lispm-ui-draw.el
    (lispm-ui-image . lispm-desktop-platinum-image)
    (lispm-ui-svg-image . lispm-desktop-platinum--svg-image)
    (lispm-ui-look-image . lispm-desktop-look-image)
    (lispm-ui-svg-fill . lispm-desktop-platinum-svg-fill)
    (lispm-ui-svg-dot . lispm-desktop-platinum--dot)
    (lispm-ui-svg-row . lispm-desktop-platinum-svg-row)
    (lispm-ui-svg-column . lispm-desktop-platinum-svg-column)
    (lispm-ui-svg-paint . lispm-desktop-look-svg-fill)
    (lispm-ui-svg-edges . lispm-desktop-look-svg-edges)
    (lispm-ui-svg-outline . lispm-desktop-look-svg-outline)
    (lispm-ui-svg-arrow . lispm-desktop-look-svg-arrow)
    (lispm-ui-svg-dotted . lispm-desktop-look-svg-dotted)
    (lispm-ui-svg-pixels . lispm-desktop-look-svg-pixels)
    (lispm-ui-svg-font . lispm-desktop-platinum-svg-font)
    (lispm-ui-svg-label . lispm-desktop-platinum--label)
    (lispm-ui-svg-dom . lispm-desktop-platinum-svg-dom)
    (lispm-ui-svg-embed . lispm-desktop-platinum-svg-embed)
    (lispm-ui-svg-icon . lispm-desktop-platinum-svg-icon)
    (lispm-ui-svg-emacs-image . lispm-desktop-platinum-svg-emacs-image)
    (lispm-ui-svg-content . lispm-desktop-platinum-svg-content)
    (lispm-ui-content-key . lispm-desktop-platinum--content-key)
    ;; lispm-ui-controls.el
    (lispm-ui-bevel-button . lispm-desktop-platinum-bevel-button)
    (lispm-ui-bevel-button-colors . lispm-desktop-platinum-bevel-button-colors)
    (lispm-ui-bevel-padding . lispm-desktop-platinum-bevel-padding)
    (lispm-ui-button-height . lispm-desktop-platinum-button-height)
    (lispm-ui-check-box . lispm-desktop-platinum-check-box)
    (lispm-ui-draw-bevel-button . lispm-desktop-platinum-draw-bevel-button)
    (lispm-ui-draw-check-box . lispm-desktop-platinum-draw-check-box)
    (lispm-ui-draw-group-edge . lispm-desktop-platinum-draw-group-edge)
    (lispm-ui-draw-group-side . lispm-desktop-platinum-draw-group-side)
    (lispm-ui-draw-list-check-box . lispm-desktop-platinum-draw-list-check-box)
    (lispm-ui-draw-popup-button . lispm-desktop-platinum-draw-popup-button)
    (lispm-ui-draw-push-button . lispm-desktop-platinum-draw-push-button)
    (lispm-ui-draw-radio . lispm-desktop-platinum-draw-radio)
    (lispm-ui-focus . lispm-desktop-platinum-focus)
    (lispm-ui-focused . lispm-desktop-platinum-focused)
    (lispm-ui-group-edge . lispm-desktop-platinum-group-edge)
    (lispm-ui-group-side . lispm-desktop-platinum-group-side)
    (lispm-ui-group-title-height . lispm-desktop-platinum-group-title-height)
    (lispm-ui-image-of . lispm-desktop-platinum-image-of)
    (lispm-ui-insert-check-box . lispm-desktop-platinum-insert-check-box)
    (lispm-ui-insert-popup-button . lispm-desktop-platinum-insert-popup-button)
    (lispm-ui-insert-push-button . lispm-desktop-platinum-insert-push-button)
    (lispm-ui-insert-radio-button . lispm-desktop-platinum-insert-radio-button)
    (lispm-ui-label . lispm-desktop-platinum-label)
    (lispm-ui-list-check-box . lispm-desktop-platinum-list-check-box)
    (lispm-ui-plain . lispm-desktop-platinum-plain)
    (lispm-ui-popup-button . lispm-desktop-platinum-popup-button)
    (lispm-ui-popup-button-width . lispm-desktop-platinum-popup-button-width)
    (lispm-ui-pressed . lispm-desktop-platinum-pressed)
    (lispm-ui-push . lispm-desktop-platinum-push)
    (lispm-ui-push-button . lispm-desktop-platinum-push-button)
    (lispm-ui-push-button-colors . lispm-desktop-platinum--push-button-colors)
    (lispm-ui-push-button-map . lispm-desktop-platinum-push-button-map)
    (lispm-ui-push-button-width . lispm-desktop-platinum-push-button-width)
    (lispm-ui-radio . lispm-desktop-platinum-radio)
    (lispm-ui-show-focus . lispm-desktop-platinum-show-focus)
    (lispm-ui-svg-bevel-button . lispm-desktop-platinum--draw-bevel-button)
    (lispm-ui-svg-focus-ring . lispm-desktop-platinum-svg-focus-ring)
    (lispm-ui-svg-pixel-map . lispm-desktop-platinum-svg-pixel-map)
    (lispm-ui-svg-popup-arrows . lispm-desktop-platinum-svg-popup-arrows)
    (lispm-ui-svg-push-button . lispm-desktop-platinum-svg-push-button)
    (lispm-ui-text-button . lispm-desktop-platinum-text-button)
    (lispm-ui-text-control . lispm-desktop-platinum-text-control))
  "LispM UI's names of what moved from LispM Desktop, (NEW . OLD) each:
the renames LispM Desktop makes, the other way round.")

(defconst lispm-ui-golden--classic
  '("#DDDDDD" "#000000" "#777777" "#6363CE")
  "The colours a classic theme gives a palette: its content, text, dimmed
text and accent.")

(defconst lispm-ui-golden--dark
  '("#282a36" "#f8f8f2" "#6272a4" "#bd93f9")
  "The colours a dark theme gives a palette.")

(defun lispm-ui-golden--svg (width height draw)
  "Return the SVG text of a WIDTH by HEIGHT document DRAW draws on, a
function of the document."
  (let ((svg (svg-create width height)))
    (funcall draw svg)
    (with-temp-buffer (svg-print svg) (buffer-string))))

(defun lispm-ui-golden--print (svg)
  "Return the SVG text of SVG, a document of svg.el's."
  (with-temp-buffer (svg-print svg) (buffer-string)))

(defun lispm-ui-golden--intervals (string)
  "Return STRING with its properties, which `equal' alone ignores, an image
it displays by its SVG text: an Emacs without SVG makes the image
otherwise, of the same text."
  (cons (substring-no-properties string)
        (mapcar (pcase-lambda (`(,start ,end ,properties))
                  (let ((display (plist-get properties 'display)))
                    (list start end
                          (if (eq (car-safe display) 'image)
                              (plist-put (copy-sequence properties) 'display
                                         (list 'svg (plist-get (cdr display) :data)))
                            properties))))
                (object-intervals string))))

(defconst lispm-ui-golden-cases
  `((palette-classic (apply #'lispm-ui-derive-palette ',lispm-ui-golden--classic))
    (palette-dark (apply #'lispm-ui-derive-palette ',lispm-ui-golden--dark))
    (greys (let ((palette (apply #'lispm-ui-derive-palette ',lispm-ui-golden--dark)))
             (mapcar (lambda (level) (lispm-ui-grey palette level))
                     '(#x00 #x11 #x22 #x44 #x55 #x77 #x88 #x99 #xAA #xBB #xCC #xDD #xEE #xFF))))
    (blends (list (lispm-ui-blend "#000000" "#FFFFFF" 0.5) (lispm-ui-blend "red" "#00ff00" 0.25)
                  (lispm-ui-blend nil "#FFFFFF" 0.5) (lispm-ui-rgb "#336699")))
    (legible (list (lispm-ui-legible "#000080" "#000000" "#FFFFFF")
                   (lispm-ui-legible "#EEEEEE" "#000000" "#FFFFFF")
                   (lispm-ui-luminance "#6363CE")))
    (merged (lispm-ui-merge-palette (apply #'lispm-ui-derive-palette ',lispm-ui-golden--classic)
                                    '(:chrome "#CCCCCC" :focus "#3366FF")))
    (color (let ((lispm-ui-current-palette '(:chrome "#ABCDEF")))
             (list (lispm-ui-color :chrome) (lispm-ui-color :outline))))
    (rows-and-columns
     (lispm-ui-golden--svg 20 20 (lambda (svg)
                                   (lispm-ui-svg-fill svg 1 2 3 4 "#123456")
                                   (lispm-ui-svg-dot svg 5 6 "#FF0000")
                                   (lispm-ui-svg-row svg 2 9 3 "#00FF00")
                                   (lispm-ui-svg-row svg 9 2 3 "#00FF00")
                                   (lispm-ui-svg-column svg 4 1 7 "#0000FF")
                                   (lispm-ui-svg-column svg 4 7 1 "#0000FF"))))
    (paint
     (lispm-ui-golden--svg 20 20 (lambda (svg)
                                   (lispm-ui-svg-paint svg 1 1 3 3 "#123456")
                                   (lispm-ui-svg-paint svg 1 1 0 3 "#123456")
                                   (lispm-ui-svg-paint svg 1 1 3 3 nil))))
    (edges
     (lispm-ui-golden--svg 30 30 (lambda (svg)
                                   (lispm-ui-svg-edges svg 2 2 20 12 '("#FFFFFF" "#DDDDDD") '("#000000" nil))
                                   (lispm-ui-svg-outline svg 0 16 10 8 "#808080"))))
    (arrows
     (lispm-ui-golden--svg 40 40 (lambda (svg)
                                   (lispm-ui-svg-arrow svg 0 0 4 'up "#000000")
                                   (lispm-ui-svg-arrow svg 10 0 4 'down "#000000")
                                   (lispm-ui-svg-arrow svg 0 10 3 'left "#000000")
                                   (lispm-ui-svg-arrow svg 10 10 3 'right "#000000"))))
    (dotted
     (lispm-ui-golden--svg 20 20 (lambda (svg) (lispm-ui-svg-dotted svg 1 1 9 6 "#000000"))))
    (pixels
     (lispm-ui-golden--svg 10 10 (lambda (svg)
                                   (lispm-ui-svg-pixels svg 1 1 '("OOx.." ".LLLO" "O  OO")
                                                        '((?O . "#000000") (?L . "#FFFFFF"))))))
    (font (list (lispm-ui-svg-font) (lispm-ui-svg-font 'lispm-ui-list)))
    (label
     (lispm-ui-golden--svg 60 20 (lambda (svg)
                                   (lispm-ui-svg-label svg "Save & Quit" 30 14 "#000000"
                                                       (lispm-ui-svg-font))
                                   (lispm-ui-svg-label svg "x" 2 14 "#111111"
                                                       '(:family "A" :size 9 :weight "bold" :synthetic t
                                                         :style "italic" :decoration "underline")
                                                       "start"))))
    (content
     (lispm-ui-golden--svg 200 24
                           (lambda (svg)
                             (lispm-ui-svg-content
                              svg (concat "Plain "
                                          (propertize "bold" 'face 'bold)
                                          (propertize " " 'display '(space :width (10)))
                                          (propertize "it" 'face '(:slant italic :foreground "#FF0000"))
                                          (propertize "u" 'face '(:underline t :strike-through t))
                                          (propertize "big" 'face '(:height 1.5)))
                              4 0 24 "#000000" 'lispm-ui-title))))
    (content-image
     (lispm-ui-golden--svg 40 24
                           (lambda (svg)
                             (lispm-ui-svg-content
                              svg (propertize "i" 'display
                                              '(image :type svg :width 16 :height 16
                                                      :data "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"16\" height=\"16\"><!-- a box --><rect width=\"16\" height=\"16\" fill=\"#a&amp;b\"/></svg>"))
                              2 0 24 "#000000" 'lispm-ui-list))))
    (dom (lispm-ui-svg-dom nil "<?xml version=\"1.0\"?><!-- c --><svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 4 4\"><path d=\"M0 0h4\" title=\"a &lt; b\"/></svg>"))
    (embed
     (lispm-ui-golden--svg 20 20 (lambda (svg)
                                   (lispm-ui-svg-embed svg (lispm-ui-svg-dom nil "<svg xmlns=\"http://www.w3.org/2000/svg\"><circle r=\"2\"/></svg>")
                                                       3 4 10 12))))
    (content-key (lispm-ui-content-key (concat "a" (propertize "b" 'face 'bold))))
    (widths (list (lispm-ui-text-width "Hello, world" 'lispm-ui-title)
                  (lispm-ui-text-width (concat "a" (propertize "日本" 'face 'bold)) 'lispm-ui-list)))
    (words (lispm-ui-text-words "Words  and 日本語の文、ここ。 (paren) end."))
    (wrap (lispm-ui-text-wrap "A line broken between words, 中文的句子也可以断开。"
                              (lambda (line) (<= (string-width line) 12))))
    (cjk-family (lispm-ui-text-cjk-family))
    ;; lispm-ui-controls.el
    (push-buttons
     (let ((palette (apply #'lispm-ui-derive-palette ',lispm-ui-golden--classic)))
       (mapcar (lambda (options) (lispm-ui-golden--print (apply #'lispm-ui-draw-push-button palette "OK" 60 options)))
               '(() (:state pressed) (:state disabled) (:default t) (:focused t) (:default t :focused t)
                 (:default t :state disabled) (:pad 5)))))
    (popup-buttons
     (let ((palette (apply #'lispm-ui-derive-palette ',lispm-ui-golden--dark)))
       (mapcar (lambda (options) (lispm-ui-golden--print (apply #'lispm-ui-draw-popup-button palette "Choice" 100 options)))
               '(() (:state pressed) (:state disabled) (:focused t) (:pad 4)))))
    (bevel-buttons
     (let ((palette (apply #'lispm-ui-derive-palette ',lispm-ui-golden--classic)))
       (mapcar (lambda (options) (lispm-ui-golden--print (apply #'lispm-ui-draw-bevel-button palette 40 20 options)))
               '(() (:state pressed) (:state disabled) (:label "Go") (:label "Go" :face lispm-ui-title)))))
    (radios
     (let ((palette (apply #'lispm-ui-derive-palette ',lispm-ui-golden--classic)))
       (mapcar (lambda (options) (lispm-ui-golden--print (apply #'lispm-ui-draw-radio palette options)))
               '((nil) (t) (t :state pressed) (t :state disabled) (nil :focused t) (t :pad 2)))))
    (check-boxes
     (let ((palette (apply #'lispm-ui-derive-palette ',lispm-ui-golden--dark)))
       (mapcar (lambda (options) (lispm-ui-golden--print (apply #'lispm-ui-draw-check-box palette options)))
               '((nil) (t) (t :state pressed) (t :state disabled) (nil :focused t) (t :pad 2)))))
    (list-check-boxes
     (let ((palette (apply #'lispm-ui-derive-palette ',lispm-ui-golden--classic)))
       (mapcar (lambda (state) (lispm-ui-golden--print (lispm-ui-draw-list-check-box palette state)))
               '(on off mixed))))
    (group-boxes
     (let ((palette (apply #'lispm-ui-derive-palette ',lispm-ui-golden--classic)))
       (list (lispm-ui-golden--print (lispm-ui-draw-group-edge palette 120 "Group"))
             (lispm-ui-golden--print (lispm-ui-draw-group-edge palette 120))
             (lispm-ui-golden--print (lispm-ui-draw-group-edge palette 120 nil t))
             (lispm-ui-golden--print (lispm-ui-draw-group-side palette 16)))))
    (focus-ring
     (lispm-ui-golden--svg 40 30 (lambda (svg)
                                   (lispm-ui-svg-focus-ring svg 4 4 20 12 3
                                                            (apply #'lispm-ui-derive-palette ',lispm-ui-golden--classic)))))
    (pixel-map
     (lispm-ui-golden--svg 10 10 (lambda (svg)
                                   (lispm-ui-svg-pixel-map svg 1 1 '("OOx.." ".LLLO") '((?O . "#000000") (?L . "#FFFFFF"))))))
    (control-widths
     (list (mapcar #'lispm-ui-push-button-width '("OK" "Don’t Save" "A rather longer label than most"))
           (mapcar #'lispm-ui-popup-button-width '("A" "Medium choice" "A rather longer choice than most"))
           lispm-ui-button-height lispm-ui-group-title-height lispm-ui-bevel-padding))
    (control-colors
     (let ((palette (apply #'lispm-ui-derive-palette ',lispm-ui-golden--dark)))
       (list (mapcar (lambda (state) (lispm-ui-push-button-colors palette state)) '(nil pressed disabled))
             (mapcar (lambda (state) (lispm-ui-bevel-button-colors palette state)) '(nil pressed disabled)))))
    (controls-as-text
     (let ((lispm-ui-current-palette (apply #'lispm-ui-derive-palette ',lispm-ui-golden--classic)))
       (mapcar #'lispm-ui-golden--intervals
               (list (lispm-ui-text-button "OK") (lispm-ui-text-button "OK" 'disabled)
                     (lispm-ui-text-button "OK" 'pressed) (lispm-ui-text-button "OK" nil t)
                     (lispm-ui-text-control "[ ]" t) (lispm-ui-text-control "[ ]" nil 'disabled)
                     (lispm-ui-text-control "[ ]")))))
    (controls-as-strings
     (let ((lispm-ui-current-palette (apply #'lispm-ui-derive-palette ',lispm-ui-golden--classic)))
       (mapcar #'lispm-ui-golden--intervals
               (list (lispm-ui-push-button "OK" :default t) (lispm-ui-push-button "Cancel" :focused t)
                     (lispm-ui-popup-button "Choice" :state 'pressed) (lispm-ui-radio t nil t)
                     (lispm-ui-check-box nil 'disabled) (lispm-ui-list-check-box 'mixed)
                     (lispm-ui-bevel-button "Go" 'pressed 24) (lispm-ui-group-edge 100 "Box")
                     (lispm-ui-group-side 18))))))
  "The cases compared, (NAME FORM) each, FORM in LispM UI's names.")

(defun lispm-ui-golden--rename (form names)
  "Return FORM with each symbol of NAMES, (FROM . TO) each, made TO."
  (cond ((symbolp form) (or (alist-get form names) form))
        ((consp form) (cons (lispm-ui-golden--rename (car form) names)
                            (lispm-ui-golden--rename (cdr form) names)))
        ((vectorp form) (apply #'vector (mapcar (lambda (x) (lispm-ui-golden--rename x names)) form)))
        (t form)))

(defun lispm-ui-golden-eval (form &optional names)
  "Return what FORM makes, its symbols renamed by NAMES first, with no
font installed."
  (cl-letf (((symbol-function 'font-family-list) #'ignore))
    (eval (lispm-ui-golden--rename form names) t)))

(defconst lispm-ui-golden-file
  (expand-file-name "golden.eld" (file-name-directory (or load-file-name buffer-file-name)))
  "Where what the desktop made is kept.")

(defun lispm-ui-golden-read ()
  "Return what the desktop made, (NAME . VALUE) each."
  (with-temp-buffer
    (insert-file-contents lispm-ui-golden-file)
    (read (current-buffer))))

(defun lispm-ui-golden-write ()
  "Write what the desktop's functions make of `lispm-ui-golden-cases' to
`lispm-ui-golden-file'.  Run with the desktop's lisp/ on the load path."
  (require 'lispm-desktop-platinum-draw)
  (require 'lispm-desktop-platinum-controls)
  (require 'lispm-desktop-look)
  (require 'lispm-desktop-i18n)
  (let ((names (mapcar (pcase-lambda (`(,new . ,old)) (cons new old)) lispm-ui-golden-names))
        (print-length nil)
        (print-level nil)
        (print-escape-newlines t))
    (with-temp-file lispm-ui-golden-file
      (insert ";;; golden.eld --- What LispM Desktop drew, for LispM UI's tests  -*- lisp-data -*-\n"
              ";; SPDX-License-Identifier: AGPL-3.0-or-later\n\n"
              ";; Written by `lispm-ui-golden-write' (make golden); see lispm-ui-golden.el.\n\n"
              "(")
      (pcase-dolist (`(,name ,form) lispm-ui-golden-cases)
        (insert "\n (" (symbol-name name) " . ")
        (prin1 (lispm-ui-golden-eval form names) (current-buffer))
        (insert ")"))
      (insert ")\n"))
    (message "Wrote %s" lispm-ui-golden-file)))

(provide 'lispm-ui-golden)
;;; lispm-ui-golden.el ends here
