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

;; LispM UI's palette, drawing, text, controls, the dressing of its
;; forms, lists' headings, scroll bars and title bars came from LispM
;; Desktop, where they were lispm-desktop-platinum-*,
;; lispm-desktop-look-*, lispm-desktop-i18n-* and lispm-desktop-form-*.  What they make, palettes, SVG documents,
;; lines and forms as they show, must be what the desktop's made, so
;; that the desktop draws the same once it draws with LispM UI.
;;
;; `lispm-ui-golden-cases' are written in LispM UI's names.
;; `lispm-ui-golden-write' evaluates each in the desktop's names, by
;; `lispm-ui-golden-names', in a desktop checkout from before the move,
;; and writes what they make to test/golden.eld, in LispM UI's names:
;;
;;   make golden LISPM_DESKTOP=../lispm-desktop/lisp LISPM_AGENT=../lisp_machine/lisp
;;
;; with LispM Desktop at 42a18a9, its last commit drawing on its own,
;; LispM's agent, which it requires, and LispM UI on the load path,
;; whose modal kit and forms it requires: a form is drawn by LispM UI's
;; own code either way, dressed by the desktop's or by LispM UI's.  The
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
    (lispm-ui-text-control . lispm-desktop-platinum-text-control)
    ;; lispm-ui-form.el, its dressing
    (lispm-ui-form--control-at . lispm-desktop-form--control-at)
    (lispm-ui-form--faces . lispm-desktop-form--faces)
    (lispm-ui-form--field-overlay . lispm-desktop-form--field-overlay)
    (lispm-ui-form--focus . lispm-desktop-form--focus)
    (lispm-ui-form--follow . lispm-desktop-form--follow)
    (lispm-ui-form--image . lispm-desktop-form--image)
    (lispm-ui-form--remaps . lispm-desktop-form--remaps)
    (lispm-ui-form--show . lispm-desktop-form--show)
    (lispm-ui-form--text-areas . lispm-desktop-form--text-areas)
    (lispm-ui-form--well . lispm-desktop-form--well)
    (lispm-ui-form-control-properties . lispm-desktop-form-control-properties)
    (lispm-ui-form-dress . lispm-desktop-form-dress)
    (lispm-ui-form-glyph . lispm-desktop-form-glyph)
    (lispm-ui-form-group . lispm-desktop-form-group)
    (lispm-ui-form-redraw-all . lispm-desktop-form-redraw-all)
    (lispm-ui-form-text-area . lispm-desktop-form-text-area)
    (lispm-ui-form-well-size . lispm-desktop-form-well-size)
    ;; lispm-ui-list.el
    (lispm-ui-list-header-height . lispm-desktop-platinum-list-header-height)
    (lispm-ui-list--sort-glyph . lispm-desktop-platinum--sort-glyph)
    (lispm-ui-draw-list-header . lispm-desktop-platinum-draw-list-header)
    (lispm-ui-list--triangle . lispm-desktop-platinum--triangle)
    (lispm-ui-draw-triangle . lispm-desktop-platinum-draw-triangle)
    (lispm-ui-triangle . lispm-desktop-platinum-triangle)
    ;; lispm-ui-scroll.el
    (lispm-ui-scroll-width . lispm-desktop-platinum-scroll-width)
    (lispm-ui-scroll-arrow . lispm-desktop-platinum-scroll-arrow)
    (lispm-ui-scroll--track . lispm-desktop-platinum--scroll-track)
    (lispm-ui-scroll-layout . lispm-desktop-platinum-scroll-layout)
    (lispm-ui-scroll-thumb . lispm-desktop-platinum-scroll-thumb)
    (lispm-ui-scroll--draw-arrow . lispm-desktop-platinum--draw-scroll-arrow)
    (lispm-ui-draw-scroll-bar . lispm-desktop-platinum-draw-scroll-bar)
    ;; lispm-ui-window.el
    (lispm-ui-title-inactive . lispm-desktop-platinum-title-inactive)
    (lispm-ui-title-bar-height . lispm-desktop-platinum-title-bar-height)
    (lispm-ui-title-box-size . lispm-desktop-platinum-box-size)
    (lispm-ui-title-box-margin . lispm-desktop-platinum-box-margin)
    (lispm-ui-svg-title-ground . lispm-desktop-platinum--draw-title-ground)
    (lispm-ui-svg-title-ridges . lispm-desktop-platinum--draw-ridges)
    (lispm-ui-draw-title-part . lispm-desktop-platinum-draw-title-part)
    (lispm-ui-title-layout . lispm-desktop-platinum-title-layout)
    ;; lispm-ui-base.el
    (lispm-ui-tty-p . lispm-desktop-tty-p)
    ;; lispm-ui-raster.el
    (lispm-ui-raster . lispm-desktop-platinum-raster)
    (lispm-ui-raster-color . lispm-desktop-platinum-raster-color)
    (lispm-ui-raster-darker . lispm-desktop-platinum--darker)
    (lispm-ui-raster-dom . lispm-desktop-platinum-raster-dom)
    (lispm-ui-raster-pixel-size . lispm-desktop-platinum-raster-pixel-size)
    (lispm-ui-raster-png . lispm-desktop-platinum-raster-png)
    (lispm-ui-raster-samples . lispm-desktop-platinum-raster-samples)
    (lispm-ui-text-cell . lispm-desktop-platinum-text-cell)
    (lispm-ui-text-compose . lispm-desktop-platinum-text-compose)
    (lispm-ui-text-picture . lispm-desktop-platinum-text-picture)
    (lispm-ui-text-size . lispm-desktop-platinum-text-size)
    ;; lispm-ui-icons.el
    (lispm-ui-icon . lispm-desktop-platinum-icon)
    (lispm-ui-icon-file . lispm-desktop-platinum-icon-file)
    (lispm-ui-icon-picture . lispm-desktop-platinum-icon-picture)
    (lispm-ui-text-icon . lispm-desktop-platinum--text-icon)
    ;; lispm-ui-popup.el
    (lispm-ui-popup-available-p . lispm-desktop-popup-available-p)
    (lispm-ui-popup-dispose . lispm-desktop-popup-dispose)
    (lispm-ui-popup-fill . lispm-desktop-popup-fill)
    (lispm-ui-popup-graphic-p . lispm-desktop-popup-graphic-p)
    (lispm-ui-popup-hold-size . lispm-desktop-popup-hold-size)
    (lispm-ui-popup-make-frame . lispm-desktop-popup-make-frame)
    (lispm-ui-popup-set-size . lispm-desktop-popup-set-size)
    (lispm-ui-popup-show . lispm-desktop-popup-show)
    (lispm-ui-popup-text-p . lispm-desktop-popup-text-p)
    ;; lispm-ui-alert.el
    (lispm-ui-alert . lispm-desktop-popup-alert)
    (lispm-ui-alert--text . lispm-desktop-popup--text-alert)
    (lispm-ui-alert--text-button-width . lispm-desktop-popup--text-button-width)
    (lispm-ui-alert--text-icon-size . lispm-desktop-popup--text-alert-icon-size)
    (lispm-ui-alert--text-layout . lispm-desktop-popup--text-alert-layout)
    (lispm-ui-alert--text-x . lispm-desktop-popup--text-alert-x)
    (lispm-ui-alert--wrap . lispm-desktop-popup--wrap)
    (lispm-ui-alert-button-gap . lispm-desktop-popup-button-gap)
    (lispm-ui-alert-layout . lispm-desktop-popup--alert-layout)
    (lispm-ui-alert-open . lispm-desktop-popup-open-alert)
    (lispm-ui-alert-text-x . lispm-desktop-popup-alert-text-x)
    (lispm-ui-alert-width . lispm-desktop-popup-alert-width)
    (lispm-ui-draw-alert . lispm-desktop-popup-draw-alert)
    (lispm-ui-svg-alert-contents . lispm-desktop-popup-svg-alert-contents)
    (lispm-ui-text-alert-width . lispm-desktop-popup-text-alert-width)
    ;; lispm-ui-menu.el
    (lispm-ui-choose-default . lispm-desktop-platinum-choose)
    (lispm-ui-draw-menu . lispm-desktop-popup-draw-menu)
    (lispm-ui-menu . lispm-desktop-popup-menu)
    (lispm-ui-menu--enabled-p . lispm-desktop-popup--enabled-p)
    (lispm-ui-menu--icon-room . lispm-desktop-popup--icon-room)
    (lispm-ui-menu--index-at . lispm-desktop-popup--index-at)
    (lispm-ui-menu--item-height . lispm-desktop-popup--item-height)
    (lispm-ui-menu--item-starting . lispm-desktop-popup--item-starting)
    (lispm-ui-menu--key . lispm-desktop-popup--key)
    (lispm-ui-menu--key-margin . lispm-desktop-popup--key-margin)
    (lispm-ui-menu--label-x . lispm-desktop-popup--label-x)
    (lispm-ui-menu--rows . lispm-desktop-popup--rows)
    (lispm-ui-menu--step . lispm-desktop-popup--step)
    (lispm-ui-menu--submenu-place . lispm-desktop-popup--submenu-place)
    (lispm-ui-menu--text . lispm-desktop-popup--text-menu)
    (lispm-ui-menu--width . lispm-desktop-popup--menu-width)
    (lispm-ui-menu-bar-step . lispm-desktop-popup-menu-bar-step)
    (lispm-ui-menu-choose . lispm-desktop-popup-choose)
    (lispm-ui-menu-icon-size . lispm-desktop-popup-icon-size)
    (lispm-ui-menu-item . lispm-desktop-popup-item)
    (lispm-ui-menu-item-disabled . lispm-desktop-popup-item-disabled)
    (lispm-ui-menu-item-height . lispm-desktop-popup-item-height)
    (lispm-ui-menu-item-highlight . lispm-desktop-popup-item-highlight)
    (lispm-ui-menu-key-margin . lispm-desktop-popup-key-margin)
    (lispm-ui-menu-keyboard-p . lispm-desktop-popup-keyboard-p)
    (lispm-ui-menu-label-x . lispm-desktop-popup-label-x)
    (lispm-ui-menu-open . lispm-desktop-popup-open-menu)
    (lispm-ui-menu-parent . lispm-desktop-popup-parent)
    (lispm-ui-menu-separator-height . lispm-desktop-popup-separator-height)
    (lispm-ui-menu-small . lispm-desktop-popup-small)
    (lispm-ui-menu-typed-keys . lispm-desktop-popup-typed-keys)
    (lispm-ui-modal-areas . lispm-desktop-area-prefixes)
    (lispm-ui-modal-typed-p . lispm-desktop-typed-event-p))
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

(defun lispm-ui-golden--image (image)
  "Return IMAGE by its SVG text, or IMAGE itself when it is no image: an
Emacs without SVG makes the image otherwise, of the same text."
  (if (eq (car-safe image) 'image)
      (list 'svg (plist-get (cdr image) :data))
    image))

(defun lispm-ui-golden--xml (text)
  "Return the SVG document TEXT parsed, as libxml2 parses it."
  (with-temp-buffer
    (insert text)
    (libxml-parse-xml-region (point-min) (point-max))))

(defconst lispm-ui-golden--shapes
  (concat "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\""
          " viewBox=\"0 0 32 32\">"
          "<defs><linearGradient id=\"g\" x1=\"0\" y1=\"0\" x2=\"1\" y2=\"1\">"
          "<stop offset=\"0\" stop-color=\"#ff0000\"/><stop offset=\"100%\" style=\"stop-color:#0000ff\"/>"
          "</linearGradient>"
          "<linearGradient id=\"h\" xlink:href=\"#g\" gradientUnits=\"userSpaceOnUse\""
          " x1=\"0\" y1=\"0\" x2=\"32\" y2=\"0\"/>"
          "<radialGradient id=\"r\" cx=\"50%\" cy=\"50%\" r=\"50%\">"
          "<stop offset=\"0\" stop-color=\"white\"/><stop offset=\"1\" stop-color=\"rgb(0,128,0)\" stop-opacity=\"0.5\"/>"
          "</radialGradient></defs>"
          "<rect x=\"1\" y=\"1\" width=\"30\" height=\"6\" fill=\"url(#g)\" stroke=\"#000\" stroke-width=\"1\"/>"
          "<circle cx=\"8\" cy=\"16\" r=\"6\" fill=\"url(#r)\"/>"
          "<ellipse cx=\"24\" cy=\"16\" rx=\"6\" ry=\"4\" fill=\"url(#h)\" opacity=\"0.75\"/>"
          "<g fill=\"#336699\">"
          "<path d=\"M2 22C4 18 8 18 10 22S16 26 18 22Q20 19 22 22T26 22L26 30H2Z\" fill-rule=\"evenodd\"/>"
          "<path d=\"m4 24a2 2 0 1 0 4 0a2 2 0 1 0 -4 0z\" fill=\"#ffffff\"/>"
          "</g>"
          "<polygon points=\"26,22 31,31 21,31\" style=\"fill:#FF00FF;fill-opacity:0.5\"/>"
          "<polyline points=\"20,20 24,24 20,28\" stroke=\"#00ffff\" stroke-width=\"2\" fill=\"red\"/>"
          "<line x1=\"0\" y1=\"31\" x2=\"31\" y2=\"20\" stroke=\"#808080\"/>"
          "<g opacity=\"0.5\"><rect x=\"14\" y=\"10\" width=\"4\" height=\"4\" fill=\"#000\"/></g>"
          "<rect x=\"0\" y=\"0\" width=\"8\" height=\"8\" fill=\"#ff0000\" style=\"display:none\"/>"
          "<text x=\"0\" y=\"10\">left out</text>"
          "</svg>")
  "An SVG of every shape and paint a picture drawn in text follows.")

(defconst lispm-ui-golden--crisp
  (concat "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"8\" height=\"8\" viewBox=\"0 0 8 8\""
          " shape-rendering=\"crispEdges\">"
          "<rect width=\"8\" height=\"8\" fill=\"#000000\"/>"
          "<rect x=\"1\" y=\"1\" width=\"6\" height=\"6\" fill=\"#FFCC00\"/>"
          "<rect x=\"2\" y=\"2\" width=\"1\" height=\"4\" fill=\"#FFFFFF\"/>"
          "<rect x=\"4\" y=\"3\" width=\"3\" height=\"1\" fill=\"#3366FF\"/>"
          "</svg>")
  "An SVG of pixels drawn as rectangles, asking for crisp edges, as Mac OS
9's icons do.")

(defconst lispm-ui-golden--alerts
  '(("Save changes to the document “notes.org” before closing?"
     "If you don’t save, your changes will be lost."
     (("Don’t Save" discard nil ?\M-d) ("Cancel" cancel cancel) ("Save" save default))
     caution)
    ("The disk is full." "Throw away what you do not need, then try again."
     (("Cancel" cancel cancel) ("Save" save default))
     stop)
    ("Done." nil (("OK" ok default)) note)
    ("„Test“ deinstallieren?" "Alles, was es installiert hat, wird entfernt."
     (("Alles deinstallieren" all nil) ("Abbrechen" cancel cancel) ("Deinstallieren" item default))
     caution))
  "Alerts drawn, (MESSAGE DETAIL BUTTONS ICON) each: one of three buttons,
one standing apart; a stop of two; a note of one; and one whose words are
too long for Mac OS 9's width.")

(defconst lispm-ui-golden--menus
  `(((:label "Undo" :action ignore :key "C-/")
     separator
     (:label "Cut" :action ignore :enabled nil :key "C-w")
     (:label "Paste" :action ignore :key "C-y")
     (:label "Paste Again" :value again :checked t)
     (:label "A label longer than the rest of them" :action ignore)
     (:label "More" :submenu ((:label "Inner" :action ignore)))
     (:label "Off")
     separator
     (:label "Preferences…" :action ignore :key "C-c ,"))
    ((:label "Ada" :action ignore
      :icon (image :type svg :width 16 :height 16
                   :data "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"16\" height=\"16\"><rect width=\"16\" height=\"16\" fill=\"#336699\"/></svg>"))
     (:label "Store" :action ignore :checked t)
     (:label "日本語のメニュー" :action ignore :key "M-RET")))
  "Menus drawn: one of every kind of item, separators doubled at neither
end; one with an icon, a check and Japanese words.")

(defun lispm-ui-golden--intervals (string &optional only)
  "Return STRING with its properties, which `equal' alone ignores, an image
it displays by its SVG text.  With ONLY, a list of properties, only
those, a run of text with the same of them as one interval, whatever
others split it into."
  (let ((intervals nil))
    (pcase-dolist (`(,start ,end ,properties) (object-intervals string))
      (let ((properties (if only
                            (cl-loop for (property value) on properties by #'cddr
                                     when (memq property only) append (list property value))
                          (copy-sequence properties))))
        (when (plist-member properties 'display)
          (setq properties (plist-put properties 'display
                                      (lispm-ui-golden--image (plist-get properties 'display)))))
        (if (and only intervals
                 (= (cadr (car intervals)) start)
                 (equal (caddr (car intervals)) properties))
            (setf (cadr (car intervals)) end)
          (push (list start end properties) intervals))))
    (cons (substring-no-properties string) (nreverse intervals))))

(defun lispm-ui-golden--shown ()
  "Return the current buffer as it is shown: its text with its faces and
what it displays, the faces and line prefixes its overlays give, in an
order of their own, and its faces' remappings."
  (list (lispm-ui-golden--intervals (buffer-string) '(face display))
        (sort (delq nil (mapcar (lambda (overlay)
                                  (let ((face (overlay-get overlay 'face))
                                        (prefix (overlay-get overlay 'line-prefix)))
                                    (when (or face prefix)
                                      (list (overlay-start overlay) (overlay-end overlay) face prefix))))
                                (overlays-in (point-min) (point-max))))
              (lambda (a b) (string< (prin1-to-string a) (prin1-to-string b))))
        face-remapping-alist))

(defun lispm-ui-golden--form (make)
  "Return how the form MAKE makes, a function returning its buffer, shows
dressed, see `lispm-ui-golden--shown'; then, for each place TAB takes
point to, from the form's start, what is displayed there and the face of
the text field there, if one is: what the keyboard is on drawn focused.
Emacs's widgets show the pictures they are given, as on a graphical
display, which a batch Emacs is not."
  (cl-letf (((symbol-function 'widget-image-find)
             (lambda (image) (and (eq (car-safe image) 'image) image))))
    (let ((buffer (funcall make)))
      (unwind-protect
          (with-current-buffer buffer
            (let ((shown (lispm-ui-golden--shown))
                  (stops nil))
              (goto-char (point-min))
              (dotimes (_ 12)
                (widget-forward 1)
                (run-hooks 'post-command-hook)
                (push (list (point)
                            (lispm-ui-golden--image (get-text-property (point) 'display))
                            (when-let* ((field (widget-field-at (point)))
                                        (overlay (widget-get field :field-overlay)))
                              (overlay-get overlay 'face)))
                      stops))
              (list shown (nreverse stops))))
        (kill-buffer buffer)))))

(defun lispm-ui-golden--as-a-window-shows ()
  "Lay the form in this buffer out as a desktop's window shows it: titled
with its title, its fields as wide as a narrower face makes them."
  (setq-local lispm-ui-form-show-header nil
              lispm-ui-form-width 44))

(defconst lispm-ui-golden--fields
  '((name :type text :label "Name" :required t :value "Ada" :help "What it is called")
    (looks :type heading :label "Looks")
    (avatar :type picture :label "Picture")
    (mood :type choice :label "Mood" :choices ((calm . "Calm") (bright . "Bright")) :value calm)
    (thinks :type radio :label "Model" :choices ((lispm . "LispM's") (own . "Its own")) :value lispm)
    (talks :type check :label "Talks" :text "Speaks first" :value t)
    (soul :type text-area :label "Soul" :lines 3 :value "Kind.")
    (more :type buttons :label "More" :buttons (("Test" . ignore))))
  "The fields of the form drawn dressed: one of each type.")

(defun lispm-ui-golden--foreign-form (glyph dress)
  "Return how a form drawn with Emacs's widgets, not LispM UI's, shows once
DRESS, a function, dresses it, its controls' pictures given by GLYPH, as
the glyph function gives them: see `lispm-ui-golden--form'."
  (lispm-ui-golden--form
   (lambda ()
     (let ((buffer (generate-new-buffer "*foreign form*")))
       (with-current-buffer buffer
         (widget-insert "Server: ")
         (widget-create 'editable-field :size 20 "imap.example.com")
         (widget-insert "\nNotes:\n")
         (widget-create 'text :format "%v" :value "One\nTwo\n")
         (widget-insert "\n")
         (widget-create 'toggle :format "%[%v%]" :on "[X]" :off "[ ]"
                        :on-glyph (funcall glyph 'check 'on nil)
                        :off-glyph (funcall glyph 'check 'off nil)
                        :lispm-form-control 'check :value t)
         (widget-insert " Keep mail\n")
         (widget-create 'push-button :tag "Test" :tag-glyph (funcall glyph 'button nil "Test")
                        :lispm-form-control '(button nil "Test"))
         (widget-insert "\n")
         (widget-setup)
         (funcall dress))
       buffer))))

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
                                                      :data "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"16\" height=\"16\" viewBox=\"0 0 16 16\"><!-- a box --><rect width=\"16\" height=\"16\" fill=\"#a&amp;b\"/></svg>"))
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
                     (lispm-ui-group-side 18)))))
    ;; lispm-ui-form.el, its dressing
    (form-glyphs
     (let ((lispm-ui-current-palette (apply #'lispm-ui-derive-palette ',lispm-ui-golden--classic)))
       (append (mapcar (lambda (arguments) (lispm-ui-golden--image (apply #'lispm-ui-form-glyph arguments)))
                       '((check on nil) (check off nil) (radio on nil) (radio off nil)
                         (button nil "Cancel") (button default "Create") (popup nil "Calm")
                         (picture nil nil)))
               (mapcar (lambda (arguments) (lispm-ui-golden--image (apply #'lispm-ui-form--image arguments)))
                       '((check on nil t) (radio off nil t) (button default "Create" t)
                         (popup nil "Calm" t))))))
    (form-groups
     (let ((lispm-ui-current-palette (apply #'lispm-ui-derive-palette ',lispm-ui-golden--dark)))
       (mapcar #'lispm-ui-golden--intervals
               (list (lispm-ui-form-group 'top 240 "Looks") (lispm-ui-form-group 'bottom 240)
                     (lispm-ui-form-group 'side)))))
    (form-dressed
     (let ((lispm-ui-current-palette (apply #'lispm-ui-derive-palette ',lispm-ui-golden--classic))
           (lispm-ui-form-glyph-function #'lispm-ui-form-glyph)
           (lispm-ui-form-group-function #'lispm-ui-form-group)
           (lispm-ui-form-mode-hook (list #'lispm-ui-form--faces #'lispm-ui-golden--as-a-window-shows))
           (lispm-ui-form-drawn-hook (list #'lispm-ui-form-dress)))
       (lispm-ui-golden--form
        (lambda () (lispm-ui-form "New Character" lispm-ui-golden--fields :submit "Create")))))
    (foreign-form-dressed
     (let ((lispm-ui-current-palette (apply #'lispm-ui-derive-palette ',lispm-ui-golden--dark)))
       (lispm-ui-golden--foreign-form #'lispm-ui-form-glyph #'lispm-ui-form-dress)))
    ;; lispm-ui-list.el
    (list-headers
     (list (lispm-ui-golden--print
            (lispm-ui-draw-list-header (apply #'lispm-ui-derive-palette ',lispm-ui-golden--classic)
                                       '(("Name" 150 nil 28) ("Date Modified" 96 t 6))))
           (lispm-ui-golden--print
            (lispm-ui-draw-list-header (apply #'lispm-ui-derive-palette ',lispm-ui-golden--dark)
                                       '(("Name" 120 t) ("Kind" 80 nil)) 12))
           lispm-ui-list-header-height))
    (triangles
     (let ((classic (apply #'lispm-ui-derive-palette ',lispm-ui-golden--classic))
           (dark (apply #'lispm-ui-derive-palette ',lispm-ui-golden--dark)))
       (list (lispm-ui-golden--print (lispm-ui-draw-triangle classic nil))
             (lispm-ui-golden--print (lispm-ui-draw-triangle classic t))
             (lispm-ui-golden--print (lispm-ui-draw-triangle dark t)))))
    (triangles-as-strings
     (let ((lispm-ui-current-palette (apply #'lispm-ui-derive-palette ',lispm-ui-golden--classic)))
       (mapcar #'lispm-ui-golden--intervals (list (lispm-ui-triangle t) (lispm-ui-triangle nil)))))
    ;; lispm-ui-scroll.el
    (scroll-layouts
     (list (lispm-ui-scroll-layout 100) (lispm-ui-scroll-layout 300)
           (lispm-ui-scroll-thumb 400 1 1000 1 1000) (lispm-ui-scroll-thumb 330 1 101 1 1001)
           (lispm-ui-scroll-thumb 330 901 1001 1 1001) (lispm-ui-scroll-thumb 400 1 2 1 100000)
           (lispm-ui-scroll-thumb 50 1 2 1 100) (lispm-ui-scroll-thumb 141 3 10 0 12)
           lispm-ui-scroll-width lispm-ui-scroll-arrow))
    (scroll-bars
     (let ((classic (apply #'lispm-ui-derive-palette ',lispm-ui-golden--classic))
           (dark (apply #'lispm-ui-derive-palette ',lispm-ui-golden--dark)))
       (mapcar (pcase-lambda (`(,palette ,height ,thumb ,active))
                 (lispm-ui-golden--print (lispm-ui-draw-scroll-bar palette height thumb active)))
               `((,classic 200 (40 . 60) t) (,classic 200 nil t) (,classic 200 (40 . 60) nil)
                 (,classic 141 (0 . 12) t) (,classic 141 (98 . 12) t) (,dark 300 (0 . 30) t)))))
    ;; lispm-ui-window.el
    (title-layouts
     (list (lispm-ui-title-layout 600 100) (lispm-ui-title-layout 300 1000)
           (lispm-ui-title-layout 300 1000 '(on nil nil)) (lispm-ui-title-layout 600 100 nil '(12 . 15))
           (lispm-ui-title-layout 600 100 '(on nil nil) '(12 . 15)) (lispm-ui-title-layout 448 120 '(nil nil nil))
           (lispm-ui-title-layout 300 100 '(on nil on)) (lispm-ui-title-layout 40 30)
           lispm-ui-title-bar-height lispm-ui-title-box-size lispm-ui-title-box-margin))
    (title-parts
     (let ((classic (apply #'lispm-ui-derive-palette ',lispm-ui-golden--classic))
           (dark (apply #'lispm-ui-derive-palette ',lispm-ui-golden--dark)))
       (list (lispm-ui-golden--print
              (lispm-ui-draw-title-part classic 448 :active t :lead t :trail t
                                        :ridges '((7 . 150) (290 . 440)) :label "Choose a Picture"
                                        :label-x 162))
             (lispm-ui-golden--print (lispm-ui-draw-title-part classic 200 :label "Away" :label-x 80))
             (lispm-ui-golden--print (lispm-ui-draw-title-part dark 50 :active t :ridges '((0 . 49))))
             (lispm-ui-golden--print (lispm-ui-draw-title-part dark 16 :active t :trail t))
             (lispm-ui-golden--svg 60 21 (lambda (svg)
                                           (lispm-ui-svg-title-ground svg 4 50 classic nil)
                                           (lispm-ui-svg-title-ridges svg 10 9 classic)
                                           (lispm-ui-svg-title-ridges svg 10 40 dark))))))
    ;; lispm-ui-raster.el
    (raster-shapes
     (mapcar (lambda (size) (lispm-ui-raster-dom (lispm-ui-golden--xml ,lispm-ui-golden--shapes) size))
             '(16 8 5)))
    (raster-crisp
     (mapcar (lambda (size) (lispm-ui-raster-dom (lispm-ui-golden--xml ,lispm-ui-golden--crisp) size))
             '(8 4 3)))
    (raster-icons
     (mapcar (lambda (icon)
               (let ((file (expand-file-name (format "icons/%s.svg" icon) lispm-ui-directory)))
                 (list (lispm-ui-raster file 16) (lispm-ui-raster file 32)
                       (lispm-ui-raster-color (lispm-ui-raster file 16))
                       (lispm-ui-raster-pixel-size file))))
             '(caution stop note)))
    (text-pictures
     (mapcar (lambda (arguments)
               (mapcar #'lispm-ui-golden--intervals
                       (apply #'lispm-ui-text-picture
                              (lispm-ui-raster (expand-file-name (format "icons/%s.svg" (car arguments))
                                                                 lispm-ui-directory)
                                               16)
                              (cdr arguments))))
             '((caution nil "#DDDDDD") (stop t "#DDDDDD") (note nil "#282a36") (caution t nil))))
    (text-picture-blocks
     (mapcar #'lispm-ui-golden--intervals
             (lispm-ui-text-picture
              [["#ff0000" "#ff0000" "#0000ff" nil "#00ff00" "#00ff00"]
               ["#ff0000" nil "#0000ff" nil "#00ff00" "#00ff00"]
               [nil "#ff0000" "#0000ff" "#0000ff" "#00ff00" nil]
               ["#ff0000" "#ff0000" nil "#0000ff" "#00ff00" "#123456"]
               ["#ffffff" "#000000" "#ffffff" "#000000" nil nil]]
              nil "#DDDDDD")))
    (raster-png
     (list (base64-encode-string
            (lispm-ui-raster-png [["#ff0000" nil] [nil "#0000ff"]] 2 6 4 "#00ff00"))
           (base64-encode-string
            (lispm-ui-raster-png [["#ff0000" "#336699"] [nil "#0000ff"]] 3 8 7))))
    (text-compose
     (list (lispm-ui-text-compose 8 3 '((0 2 "ab") (2 0 "x") (2 6 "long") (1 3 "cd") (1 0 "ef") (5 0 "out")))
           (lispm-ui-text-size 16) (lispm-ui-text-size 13) lispm-ui-text-cell lispm-ui-raster-samples
           (lispm-ui-raster-darker "#88aaff")))
    ;; lispm-ui-icons.el
    (text-icons
     (let ((lispm-ui-current-palette (apply #'lispm-ui-derive-palette ',lispm-ui-golden--classic)))
       (cl-letf (((symbol-function 'lispm-ui-tty-p) (lambda (&optional _) t)))
         (mapcar (lambda (name) (lispm-ui-golden--intervals (lispm-ui-text-icon name)))
                 (list 'caution 'stop 'note 'emacs "folder-nowhere-32" "document-lisp-nowhere"
                       "no-such-icon" (expand-file-name "icons/stop.svg" lispm-ui-directory))))))
    (icon-pictures
     (mapcar (lambda (arguments)
               (mapcar #'lispm-ui-golden--intervals (apply #'lispm-ui-icon-picture arguments)))
             `((caution 16 nil "#DDDDDD") (stop 16 t "#DDDDDD") (note 32 nil "#282a36")
               (,(expand-file-name "icons/caution.svg" lispm-ui-directory) 16 nil "#DDDDDD"))))
    ;; lispm-ui-alert.el
    (alert-layouts
     (let ((lispm-ui-current-palette (apply #'lispm-ui-derive-palette ',lispm-ui-golden--classic)))
       (mapcar (lambda (alert) (apply #'lispm-ui-alert-layout (seq-take alert 3))) lispm-ui-golden--alerts)))
    (alerts-drawn
     (cl-mapcar (lambda (alert palette pressed focused)
                  (lispm-ui-golden--print
                   (lispm-ui-draw-alert palette (nth 3 alert) (nth 0 alert) (nth 1 alert) (nth 2 alert)
                                        pressed focused)))
                lispm-ui-golden--alerts
                (list (apply #'lispm-ui-derive-palette ',lispm-ui-golden--classic)
                      (apply #'lispm-ui-derive-palette ',lispm-ui-golden--dark)
                      (apply #'lispm-ui-derive-palette ',lispm-ui-golden--classic)
                      (apply #'lispm-ui-derive-palette ',lispm-ui-golden--dark))
                '(nil save nil all) '(nil nil ok cancel)))
    (alert-contents
     (let ((palette (apply #'lispm-ui-derive-palette ',lispm-ui-golden--dark)))
       (lispm-ui-golden--svg 400 160
                             (lambda (svg)
                               (lispm-ui-svg-alert-contents
                                svg (apply #'lispm-ui-alert-layout (seq-take (car lispm-ui-golden--alerts) 3))
                                40 10 palette 'discard 'save "#FF0000")))))
    (alert-wrap
     (list (lispm-ui-alert--wrap "Save changes to the document “notes.org” before closing?" 120 'lispm-ui-title)
           (lispm-ui-alert--wrap "保存对文稿“笔记”的更改吗？若不保存，更改将会丢失。" 60 'lispm-ui-list)
           lispm-ui-alert-width lispm-ui-alert-text-x lispm-ui-alert-button-gap lispm-ui-text-alert-width
           lispm-ui-alert--text-x lispm-ui-alert--text-icon-size
           (lispm-ui-alert--text-button-width "Don’t Save")))
    (text-alerts
     (let ((lispm-ui-current-palette (apply #'lispm-ui-derive-palette ',lispm-ui-golden--classic)))
       (cl-letf (((symbol-function 'lispm-ui-tty-p) (lambda (&optional _) t)))
         (cl-mapcar (lambda (alert pressed focused)
                      (list (apply #'lispm-ui-alert--text-layout (seq-take alert 3))
                            (lispm-ui-golden--intervals
                             (lispm-ui-alert--text (nth 3 alert) (nth 0 alert) (nth 1 alert) (nth 2 alert)
                                                   pressed focused))))
                    lispm-ui-golden--alerts '(nil save nil all) '(nil nil ok cancel)))))
    ;; lispm-ui-menu.el
    (menu-layouts
     (let ((lispm-ui-current-palette (apply #'lispm-ui-derive-palette ',lispm-ui-golden--classic)))
       (append
        (mapcar (lambda (items)
                  (list (lispm-ui-menu--rows items) (lispm-ui-menu--width items)
                        (lispm-ui-menu--icon-room items) (mapcar #'lispm-ui-menu--key items)
                        (mapcar #'lispm-ui-menu--enabled-p items)
                        (lispm-ui-menu--index-at items 20) (lispm-ui-menu--index-at items 40)
                        (lispm-ui-menu--step items nil 1) (lispm-ui-menu--step items 0 -1)
                        (lispm-ui-menu--item-starting items nil ?p)
                        (lispm-ui-menu--item-starting items 3 ?p)))
                lispm-ui-golden--menus)
        (let ((lispm-ui-menu-small t))
          (list (lispm-ui-menu--rows (car lispm-ui-golden--menus))
                (lispm-ui-menu--width (car lispm-ui-golden--menus))
                (lispm-ui-menu--item-height) (lispm-ui-menu--label-x) (lispm-ui-menu--key-margin)))
        (list (lispm-ui-menu--submenu-place (list :items (car lispm-ui-golden--menus) :x 100 :y 20 :width 140)
                                            6 '((:label "Inner" :action ignore)) 1000)
              (lispm-ui-menu--submenu-place (list :items (car lispm-ui-golden--menus) :x 100 :y 20 :width 140)
                                            6 '((:label "Inner" :action ignore)) 300)
              lispm-ui-menu-item-height lispm-ui-menu-separator-height lispm-ui-menu-label-x
              lispm-ui-menu-key-margin lispm-ui-menu-icon-size))))
    (menus-drawn
     (append
      (cl-mapcar (lambda (items palette highlight)
                   (lispm-ui-golden--print (lispm-ui-draw-menu palette items (lispm-ui-menu--width items)
                                                               highlight)))
                 (append lispm-ui-golden--menus lispm-ui-golden--menus)
                 (list (apply #'lispm-ui-derive-palette ',lispm-ui-golden--classic)
                       (apply #'lispm-ui-derive-palette ',lispm-ui-golden--classic)
                       (apply #'lispm-ui-derive-palette ',lispm-ui-golden--dark)
                       (apply #'lispm-ui-derive-palette ',lispm-ui-golden--dark))
                 '(nil 0 6 1))
      (let ((lispm-ui-menu-small t)
            (palette (apply #'lispm-ui-derive-palette ',lispm-ui-golden--classic)))
        (list (lispm-ui-golden--print (lispm-ui-draw-menu palette (car lispm-ui-golden--menus) 120 4))))))
    (text-menus
     (let ((lispm-ui-current-palette (apply #'lispm-ui-derive-palette ',lispm-ui-golden--classic)))
       (cl-letf (((symbol-function 'lispm-ui-tty-p) (lambda (&optional _) t)))
         (append
          (cl-mapcar (lambda (items highlight)
                       (list (lispm-ui-menu--rows items) (lispm-ui-menu--width items)
                             (lispm-ui-golden--intervals
                              (lispm-ui-menu--text items (lispm-ui-menu--width items) highlight))))
                     lispm-ui-golden--menus '(3 0))
          (list (lispm-ui-golden--intervals (lispm-ui-menu--text (car lispm-ui-golden--menus) 18 6))))))))
  "The cases compared, (NAME FORM) each, FORM in LispM UI's names.")

(defconst lispm-ui-golden-xml-cases
  '(dom embed content-image raster-shapes raster-crisp raster-icons text-pictures icon-pictures
        text-icons alerts-drawn text-alerts menus-drawn)
  "The cases reading XML, which an Emacs built without libxml2 cannot.")

(defun lispm-ui-golden--rename (form names)
  "Return FORM with each symbol of NAMES, (FROM . TO) each, made TO."
  (cond ((symbolp form) (or (alist-get form names) form))
        ((consp form)
         ;; A list along, not down its cdrs: what a form drew is long.
         (let ((renamed nil))
           (while (consp form)
             (push (lispm-ui-golden--rename (pop form) names) renamed))
           (nconc (nreverse renamed) (and form (lispm-ui-golden--rename form names)))))
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

(defconst lispm-ui-golden-widget-cases '(form-dressed foreign-form-dressed)
  "The cases drawing forms with Emacs's widgets, which Emacs 31 draws
otherwise than Emacs 30, a text area's last newline in an overlay of its
own: compared in the major version of Emacs that wrote them.")

(defun lispm-ui-golden-read ()
  "Return what the desktop made, (NAME . VALUE) each, and the major version
of Emacs that wrote it, as (:emacs . VERSION)."
  (with-temp-buffer
    (insert-file-contents lispm-ui-golden-file)
    (read (current-buffer))))

(defun lispm-ui-golden-write ()
  "Write what the desktop's functions make of `lispm-ui-golden-cases' to
`lispm-ui-golden-file'.  Run with the desktop's lisp/ on the load path.
What they make is written in LispM UI's names, as a face it remaps."
  (require 'lispm-desktop-platinum-draw)
  (require 'lispm-desktop-platinum-controls)
  (require 'lispm-desktop-look)
  (require 'lispm-desktop-i18n)
  (require 'lispm-desktop-form)
  (require 'lispm-desktop-platinum)
  (require 'lispm-desktop-platinum-scroll)
  (require 'lispm-desktop-platinum-titlebar)
  (require 'lispm-desktop-platinum-raster)
  (require 'lispm-desktop-platinum-icons)
  (require 'lispm-desktop-popup)
  (require 'lispm-ui-base)
  (let ((names (mapcar (pcase-lambda (`(,new . ,old)) (cons new old)) lispm-ui-golden-names))
        (back (mapcar (pcase-lambda (`(,new . ,old)) (cons old new)) lispm-ui-golden-names))
        (print-length nil)
        (print-level nil)
        (print-escape-newlines t))
    (with-temp-file lispm-ui-golden-file
      (insert ";;; golden.eld --- What LispM Desktop drew, for LispM UI's tests  -*- lisp-data -*-\n"
              ";; SPDX-License-Identifier: AGPL-3.0-or-later\n\n"
              ";; Written by `lispm-ui-golden-write' (make golden); see lispm-ui-golden.el.\n\n"
              "((:emacs . " (number-to-string emacs-major-version) ")")
      (pcase-dolist (`(,name ,form) lispm-ui-golden-cases)
        (insert "\n (" (symbol-name name) " . ")
        (prin1 (lispm-ui-golden--rename (lispm-ui-golden-eval form names) back) (current-buffer))
        (insert ")"))
      (insert ")\n"))
    (message "Wrote %s" lispm-ui-golden-file)))

(provide 'lispm-ui-golden)
;;; lispm-ui-golden.el ends here
