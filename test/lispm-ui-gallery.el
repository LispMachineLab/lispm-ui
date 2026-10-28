;;; lispm-ui-gallery.el --- LispM UI's controls, drawn to look at  -*- lexical-binding: t; -*-

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

;; `make gallery' draws every control and part LispM UI draws, in each
;; of its states, in a light palette and a dark one, as SVG files in
;; gallery/, and gallery/index.html showing them all, to be looked at in
;; a browser after a change.  The tests check the drawings themselves.
;;
;; A batch Emacs measures text in characters, not in the pixels of a
;; font, so its labels stand where a font's would not; for labels where
;; they belong, load this file in a graphical Emacs, LispM UI on its load
;; path, and run `lispm-ui-gallery-write' there.

;;; Code:

(require 'cl-lib)
(require 'svg)
(require 'lispm-ui)

(defconst lispm-ui-gallery-palettes
  '(("Light" "#DDDDDD" "#000000" "#777777" "#6363CE")
    ("Dark" "#282a36" "#f8f8f2" "#6272a4" "#bd93f9"))
  "The palettes the gallery draws in: (NAME CONTENT TEXT DIM ACCENT).")

(defun lispm-ui-gallery--controls (palette)
  "Return the gallery's rows in PALETTE: (TITLE (CAPTION . SVG)...) each."
  `(("Push buttons"
     ,@(mapcar (pcase-lambda (`(,caption . ,options))
                 (cons caption (apply #'lispm-ui-draw-push-button palette "Button" 72 :pad 5 options)))
               '(("plain") ("pressed" :state pressed) ("disabled" :state disabled)
                 ("default" :default t) ("focused" :focused t) ("default, focused" :default t :focused t)
                 ("default, disabled" :default t :state disabled))))
    ("Pop-up menus"
     ,@(mapcar (pcase-lambda (`(,caption . ,options))
                 (cons caption (apply #'lispm-ui-draw-popup-button palette "Choice" 110 :pad 2 options)))
               '(("plain") ("pressed" :state pressed) ("disabled" :state disabled) ("focused" :focused t))))
    ("Bevel buttons"
     ,@(mapcar (pcase-lambda (`(,caption . ,options))
                 (cons caption (apply #'lispm-ui-draw-bevel-button palette 64 22 :label "Bevel" options)))
               '(("plain") ("pressed" :state pressed) ("disabled" :state disabled))))
    ("Radio buttons"
     ,@(mapcar (pcase-lambda (`(,caption . ,options))
                 (cons caption (apply #'lispm-ui-draw-radio palette (car options) :pad 2 (cdr options))))
               '(("off" nil) ("on" t) ("pressed" t :state pressed) ("disabled" t :state disabled)
                 ("focused" nil :focused t))))
    ("Check boxes"
     ,@(mapcar (pcase-lambda (`(,caption . ,options))
                 (cons caption (apply #'lispm-ui-draw-check-box palette (car options) :pad 2 (cdr options))))
               '(("off" nil) ("on" t) ("pressed" t :state pressed) ("disabled" t :state disabled)
                 ("focused" t :focused t)))
     ,@(mapcar (lambda (state)
                 (cons (format "in a list, %s" state) (lispm-ui-draw-list-check-box palette state)))
               '(on off mixed)))
    ("Group boxes"
     ("top edge" . ,(lispm-ui-draw-group-edge palette 160 "Group"))
     ("bottom edge" . ,(lispm-ui-draw-group-edge palette 160 nil t))
     ("side" . ,(lispm-ui-draw-group-side palette 18)))
    ("Lists"
     ("headings, sorted by date" . ,(lispm-ui-draw-list-header palette '(("Name" 150 nil 28)
                                                                         ("Date Modified" 96 t 6))))
     ("headings, sorted by name" . ,(lispm-ui-draw-list-header palette '(("Name" 150 t 28)
                                                                         ("Date Modified" 96 nil 6))))
     ("triangle, closed" . ,(lispm-ui-draw-triangle palette nil))
     ("triangle, open" . ,(lispm-ui-draw-triangle palette t)))
    ("Scroll bars"
     ("active" . ,(lispm-ui-draw-scroll-bar palette 141 (lispm-ui-scroll-thumb 141 2 9 0 20) t))
     ("at the end" . ,(lispm-ui-draw-scroll-bar palette 141 (lispm-ui-scroll-thumb 141 13 20 0 20) t))
     ("nothing to scroll" . ,(lispm-ui-draw-scroll-bar palette 141 nil t))
     ("inactive" . ,(lispm-ui-draw-scroll-bar palette 141 (lispm-ui-scroll-thumb 141 2 9 0 20) nil)))
    ("Title bars"
     ,@(let ((layout (lispm-ui-title-layout 300 (lispm-ui-text-width "Choose a Picture" 'lispm-ui-title)
                                            '(nil nil nil))))
         `(("a dialog's" . ,(lispm-ui-draw-title-part palette 300 :active t :lead t :trail t
                                                      :ridges (plist-get layout :ridges)
                                                      :label "Choose a Picture"
                                                      :label-x (plist-get layout :label-x)))
           ("inactive" . ,(lispm-ui-draw-title-part palette 300 :label "Choose a Picture"
                                                    :label-x (plist-get layout :label-x))))))))

(defun lispm-ui-gallery-write (&optional directory)
  "Draw the gallery in DIRECTORY, by default gallery/ beside test/, or
where the environment's GALLERY says."
  (interactive)
  (let* ((directory (file-name-as-directory
                     (or directory
                         (getenv "GALLERY")
                         (expand-file-name "gallery" (file-name-directory
                                                      (directory-file-name
                                                       (file-name-directory (or load-file-name buffer-file-name))))))))
         (html nil)
         (count 0))
    (make-directory directory t)
    (pcase-dolist (`(,name . ,colors) lispm-ui-gallery-palettes)
      (let ((palette (apply #'lispm-ui-derive-palette colors)))
        (push (format "<section style=\"background:%s;color:%s;padding:12px\"><h2>%s</h2>"
                      (plist-get palette :chrome) (plist-get palette :text) name)
              html)
        (pcase-dolist (`(,title . ,cells) (lispm-ui-gallery--controls palette))
          (push (format "<h3>%s</h3><div style=\"display:flex;flex-wrap:wrap;gap:16px;align-items:end\">" title) html)
          (pcase-dolist (`(,caption . ,svg) cells)
            (let ((file (format "%s-%d.svg" (downcase name) (setq count (1+ count)))))
              (with-temp-file (expand-file-name file directory)
                (svg-print svg))
              (push (format "<figure style=\"margin:0\"><img src=\"%s\" style=\"zoom:2;image-rendering:pixelated\"><figcaption style=\"font:11px sans-serif\">%s</figcaption></figure>"
                            file caption)
                    html)))
          (push "</div>" html))
        (push "</section>" html)))
    (with-temp-file (expand-file-name "index.html" directory)
      (insert "<!doctype html><meta charset=\"utf-8\"><title>LispM UI's controls</title><body style=\"margin:0\">"
              (apply #'concat (nreverse html))
              "</body>\n"))
    (message "Drew %d controls in %s" count directory)))

(provide 'lispm-ui-gallery)
;;; lispm-ui-gallery.el ends here
