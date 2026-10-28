;;; lispm-ui-text.el --- Text as LispM UI lays it out  -*- lexical-binding: t; -*-

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

;; Labels, lists and fields are laid out in pixels, measured in the faces
;; their text is shown in, so that a proportional font lines them up:
;;
;;   `lispm-ui-text-width'   how wide text is, in its faces and a face
;;                           beneath them
;;   `lispm-ui-text-fit'     text cut to a width, ending in an ellipsis
;;   `lispm-ui-text-space'   a space so many pixels wide
;;   `lispm-ui-text-to'      a space reaching so many pixels from the left
;;                           or the right
;;   `lispm-ui-text-wrap'    text broken into lines, each as long as a
;;                           function allows, between words, or between
;;                           the characters of Chinese and Japanese
;;
;; On a text terminal a cell counts as a pixel.  Chinese and Japanese are
;; drawn in a proportional family for the language spoken,
;; `lispm-ui-text-cjk-family', as the words around them are.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-ui-base)
(require 'lispm-ui-i18n)

;;;; Lengths

(defun lispm-ui-text-width (text &optional face)
  "Return the width in pixels of TEXT shown in its faces, FACE beneath them.
On a text terminal, in cells."
  (let ((text (copy-sequence text)))
    (when face
      (add-face-text-property 0 (length text) face t text))
    (string-pixel-width text)))

(defun lispm-ui-text-fit (text pixels &optional face)
  "Return TEXT cut to PIXELS wide in its faces, FACE beneath them, ending in
an ellipsis when it was cut.  The ellipsis takes the face TEXT starts
with."
  (if (<= (lispm-ui-text-width text face) pixels)
      text
    (let ((low 0) (high (length text)))
      ;; The longest start of TEXT that fits with its ellipsis.
      (while (< low high)
        (let ((middle (/ (+ low high 1) 2)))
          (if (<= (lispm-ui-text-width (concat (substring text 0 middle) "…") face) pixels)
              (setq low middle)
            (setq high (1- middle)))))
      (concat (string-trim-right (substring text 0 low))
              (propertize "…" 'face (get-text-property 0 'face text))))))

(defun lispm-ui-text-space (pixels)
  "Return a space PIXELS wide."
  (propertize " " 'display `(space :width (,(max 0 pixels)))))

(defun lispm-ui-text-to (pixels &optional from-right)
  "Return a space reaching to PIXELS from the left, or, FROM-RIGHT, from
the right."
  (propertize " " 'display (if from-right
                               `(space :align-to (- right (,pixels)))
                             `(space :align-to (,pixels)))))

;;;; Chinese and Japanese type

(defconst lispm-ui-text-cjk-families
  '((ja "Hiragino Sans" "Hiragino Kaku Gothic ProN" "Noto Sans CJK JP")
    (zh-Hant "PingFang TC" "Noto Sans CJK TC")
    (zh-Hans "PingFang SC" "Noto Sans CJK SC"))
  "The proportional families Chinese and Japanese are drawn in, by
language: (TAG FAMILY...), the first installed taken.  Another language
draws them as Simplified Chinese does.  The fonts of the classic look
have no ideographs, and Emacs's own fallback for them is the terminals'
monospace family, its ideographs a cell apart.")

(defun lispm-ui-text-cjk-family ()
  "Return the family Chinese and Japanese are drawn in, for the language
LispM UI speaks, or nil when none is installed."
  (let ((installed (font-family-list)))
    (seq-find (lambda (family) (member family installed))
              (or (alist-get (lispm-ui-current-language) lispm-ui-text-cjk-families)
                  (alist-get 'zh-Hans lispm-ui-text-cjk-families)))))

;;;; Breaking lines

(defun lispm-ui-text-words (text)
  "Return TEXT in the pieces a line may break between.
A piece is a word, after the blank before it if any, or a character of a
script written without blanks, as Chinese and Japanese; punctuation that
may not start a line stays on the piece before it, and punctuation that
may not end one on the piece after it.  Concatenated, the pieces are TEXT
with its blanks made single spaces; a line starting at a piece drops its
blank, see `lispm-ui-text-wrap'.  The pieces keep TEXT's properties, a
blank those of the last blank it stands for."
  (require 'kinsoku)
  (let ((pieces nil) (start nil) (end nil) (lead nil)
        (blank nil) (joined nil) (breakable nil))
    (cl-flet ((piece ()
                (concat (if lead (apply #'propertize " " (text-properties-at (1- start) text)) "")
                        (substring text start end))))
      (dotimes (index (length text))
        (let ((char (aref text index)))
          (if (memq char '(?\s ?\t ?\n ?\r))
              (setq blank t)
            (let* ((categories (char-category-set char))
                   (here (aref categories ?|)))
              (unless (and start (not blank)
                           (or joined (aref categories ?>) (not (or here breakable))))
                (when start (push (piece) pieces))
                (setq lead (and blank start t)
                      start index))
              (setq end (1+ index)
                    joined (aref categories ?<) breakable here blank nil)))))
      (nreverse (if start (cons (piece) pieces) pieces)))))

(defun lispm-ui-text-wrap (text fits)
  "Return TEXT broken into lines, each as long as FITS allows.
FITS is called with a line and returns non-nil when it is short enough;
lines break between the pieces of `lispm-ui-text-words', and a piece too
long for a line stands alone."
  (let ((lines nil) (line nil))
    (dolist (piece (lispm-ui-text-words text))
      (let ((longer (if line (concat line piece) (string-trim-left piece))))
        (if (or (null line) (funcall fits longer))
            (setq line longer)
          (push line lines)
          (setq line (string-trim-left piece)))))
    (nreverse (if line (cons line lines) lines))))

(provide 'lispm-ui-text)
;;; lispm-ui-text.el ends here
