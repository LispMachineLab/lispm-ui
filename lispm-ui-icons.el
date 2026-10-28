;;; lispm-ui-icons.el --- Icons, found by name and drawn anywhere  -*- lexical-binding: t; -*-

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

;; Icons are named, and found by their name: an interface with icons of
;; its own, as a desktop's, finds them through `lispm-ui-icon-functions';
;; LispM UI has those its alerts show, icons/caution.svg, stop.svg and
;; note.svg.  An icon is shown as an image, or on a text terminal as a
;; character in its colour, or drawn in block characters, see
;; lispm-ui-raster.el.
;; Unlike controls, icons keep their own colours, as Mac OS 9's did
;; whatever the appearance.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'lispm-ui-base)
(require 'lispm-ui-palette)
(require 'lispm-ui-raster)

(defvar lispm-ui--icons (make-hash-table :test #'equal)
  "Icon images by name.")

(defvar lispm-ui-icon-functions nil
  "Functions finding the file of an icon by its name, tried in turn before
LispM UI's own icons: each is called with the name, as
`lispm-ui-icon-file' takes it, and returns a file, or nil to leave it to
the next.  An interface drawing its own icons, as a desktop's look
does, adds one.")

(defun lispm-ui-icon-file (name)
  "Return the file of icon NAME: an absolute file name as it is, as an
application's icon; for `emacs', the icon Emacs ships with, read from
this Emacs's own data directory, else the document icon; else the file
the first of `lispm-ui-icon-functions' finds, else LispM UI's own,
icons/NAME.svg, whose alert icons are `caution', `stop' and `note'."
  (pcase name
    ((pred (lambda (name) (and (stringp name) (file-name-absolute-p name)))) name)
    ('emacs (let ((file (expand-file-name "images/icons/hicolor/scalable/apps/emacs.svg"
                                          data-directory)))
              (if (file-readable-p file)
                  file
                (lispm-ui-icon-file "document"))))
    (_ (or (run-hook-with-args-until-success 'lispm-ui-icon-functions name)
           (expand-file-name (format "%s.svg" name) (expand-file-name "icons" lispm-ui-directory))))))

(defvar lispm-ui-text-icons
  '(("folder" . "▪") ("disk" . "▬") ("hard-disk" . "▬") ("computer" . "▣")
    ("document-lisp" . "λ") ("document" . "▫") ("note" . "▫")
    ("robot" . "◉") ("lispm" . "◆") ("lambda" . "λ") ("emacs" . "◆")
    ("applications" . "◆") ("app-store" . "◈") ("control-panels" . "▤")
    ("globe" . "○") ("netscape" . "○") ("trash" . "▿") ("guide" . "?")
    ("mail" . "@") ("caution" . "!") ("stop" . "×")
    ("appearance" . "◐") ("desktop-picture" . "▦") ("keyboard" . "▥") ("language" . "¶")
    ("network" . "⇄") ("sandbox" . "▧") ("software-update" . "↻") ("startup" . "▸")
    ("find" . "⌕") ("terminal" . "λ"))
  "What stands for each icon on a text terminal, one cell wide.
Each entry is (NAME . STRING): NAME is an icon's name as
`lispm-ui-icon' takes it, or the start of one, so that \"folder\" also
stands for \"folder-git\", and the longest start that matches is the one
used.  An icon matched by none is a middle dot.  A terminal whose font
is a Nerd Font can be given its icons here.")

(defvar lispm-ui--icon-colors (make-hash-table :test #'equal)
  "The colour each icon is drawn in most, by name, see
`lispm-ui-text-icon'.")

(defun lispm-ui-text-icon (name)
  "Return what stands for icon NAME on a text terminal, see
`lispm-ui-text-icons', in the colour the icon is drawn in most, see
`lispm-ui-raster-color'."
  (let* ((color (with-memoization (gethash name lispm-ui--icon-colors)
                  (when-let* ((pixels (lispm-ui-raster
                                       (lispm-ui-icon-file name) 16)))
                    (lispm-ui-raster-color pixels))))
         (name (if (symbolp name) (symbol-name name) (file-name-base name)))
         (match (car (sort (seq-filter (lambda (entry) (string-prefix-p (car entry) name))
                                       lispm-ui-text-icons)
                           (lambda (a b) (> (length (car a)) (length (car b))))))))
    (propertize (truncate-string-to-width (or (cdr match) "·") 1 nil ?\s)
                'face (if color
                          `(:inherit lispm-ui-list :foreground ,color)
                        'lispm-ui-list))))

(defun lispm-ui--icon-source (name size)
  "Return the file to draw icon NAME from at SIZE pixels, 16 or less: its
own sixteen-pixel drawing where it has one beside the 32-pixel NAME-32,
so that it comes out pixel for pixel, else NAME's.  An icon drawn at a
size that is no whole part of its own would smear."
  (let ((file (lispm-ui-icon-file name)))
    (or (when-let* (((<= size 16))
                    ((string-match "-32\\.svg\\'" file))
                    (small (replace-match ".svg" t t file))
                    ((file-readable-p small)))
          small)
        (when-let* (((<= size 16))
                    ((stringp name))
                    ((string-suffix-p "-32" name))
                    (small (lispm-ui-icon-file (string-remove-suffix "-32" name)))
                    ((file-readable-p small)))
          small)
        file)))

(defvar lispm-ui--icon-pictures (make-hash-table :test #'equal)
  "Icons drawn in text, by file, size, selection and background, see
`lispm-ui-icon-picture'.")

(defun lispm-ui-icon-picture (name size &optional selected background)
  "Return icon NAME drawn on a text terminal SIZE pixels a side, best 16, as
lines of text, see `lispm-ui-text-picture', darkened when SELECTED, over
BACKGROUND, the colour where it is drawn; or nil where it cannot be
drawn, as an icon that is no SVG.  NAME is as `lispm-ui-icon-file' takes
it; it is drawn from its own drawing of that size or twice it, see
`lispm-ui--icon-source'.  Each is drawn once."
  (let ((file (lispm-ui--icon-source name size)))
    (with-memoization (gethash (list file size (and selected t) background)
                               lispm-ui--icon-pictures)
      (when-let* ((pixels (lispm-ui-raster file size)))
        (lispm-ui-text-picture pixels selected background)))))

(defun lispm-ui-icon (name)
  "Return icon NAME as a string one icon wide.
NAME is as `lispm-ui-icon-file' takes it.  On a text terminal a
character stands in, see `lispm-ui-text-icons'; elsewhere where images
cannot be shown, a blank."
  (let ((file (lispm-ui-icon-file name)))
    ;; The icon carries a face of its own, so a button it sits in, such as
    ;; a list row, does not underline it.
    (cond
     ((lispm-ui-tty-p) (lispm-ui-text-icon name))
     ((and (display-images-p) (file-readable-p file))
      (propertize " " 'face 'lispm-ui-list
                  'display (or (gethash file lispm-ui--icons)
                               (puthash file (create-image file nil nil :height 16
                                                           :ascent 'center)
                                        lispm-ui--icons))))
     (t (propertize " " 'face 'lispm-ui-list 'display '(space :width (16)))))))

;;;; Images on a text terminal

(defvar lispm-ui-tty-image-function nil
  "Function showing an icon as an image on a text terminal, or nil where
none can: a terminal of a graphics protocol, as a desktop shows its
icons on.  It is called with KEY, what tells the icon from others of its
window, the icon's NAME, whether it is SELECTED, and the COLUMNS and
ROWS it takes, and returns as many blank lines, marked for
`lispm-ui-tty-image-show-function' to find, or nil where it cannot show
NAME, which is then drawn in text.")

(defvar lispm-ui-tty-image-show-function nil
  "Function showing as images the icons of a window that
`lispm-ui-tty-image-function' marked, and no others: called with the
window once its text is laid out.")

(provide 'lispm-ui-icons)
;;; lispm-ui-icons.el ends here
