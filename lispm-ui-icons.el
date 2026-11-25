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

;; Icons are named, and found by their name: an interface with icons
;; of its own, as a desktop's, finds them through
;; `lispm-ui-icon-functions'; LispM UI has those its alerts show,
;; icons/caution.svg, stop.svg and note.svg, and those its file dialog
;; shows, folder.svg, disk.svg and a document's, document.svg or one
;; of its kind's, which `lispm-ui-file-icon-name' tells.  An icon is
;; shown as an image, or on a text terminal as a character in its
;; colour, or drawn in block characters, see lispm-ui-raster.el.
;; Unlike controls, icons keep their own colours, as Mac OS 9's did
;; whatever the appearance.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-ui-base)
(require 'lispm-ui-palette)
(require 'lispm-ui-tokens)
(require 'lispm-ui-icons-catalog)
(require 'lispm-ui-raster)

(defvar lispm-ui--icons (make-hash-table :test #'equal)
  "Icon images by name or `(FILE-OR-NAME SIZE TINT)' key.")

(defvar lispm-ui--icon-registry (make-hash-table :test #'equal)
  "Declarative icon registry mapping normalized name to (:file :svg :tty).")

(defvar lispm-ui-icon-functions nil
  "Functions finding the file of an icon by its name, tried in turn before
LispM UI's own icons: each is called with the name, as
`lispm-ui-icon-file' takes it, and returns a file, or nil to leave it to
the next.  An interface drawing its own icons, as a desktop's look
does, adds one.")

(defun lispm-ui--icon-key (name)
  "Return normalized string key for icon NAME."
  (if (symbolp name) (symbol-name name) (or name "")))

(defun lispm-ui--evict-icon-cache (key)
  "Remove cached rendered icons matching KEY from `lispm-ui--icons'."
  (remhash key lispm-ui--icons)
  (maphash (lambda (k _v)
             (when (and (consp k) (equal (car k) key))
               (remhash k lispm-ui--icons)))
           lispm-ui--icons))

(cl-defun lispm-ui-icon-register (name &key file svg tty)
  "Register icon NAME with FILE path, raw SVG string, and/or TTY glyph.
Return normalized string name."
  (let ((key (lispm-ui--icon-key name)))
    (puthash key (list :file file :svg svg :tty tty) lispm-ui--icon-registry)
    (lispm-ui--evict-icon-cache key)
    key))

(defun lispm-ui-icon-unregister (name)
  "Remove custom registration for icon NAME."
  (let ((key (lispm-ui--icon-key name)))
    (prog1 (remhash key lispm-ui--icon-registry)
      (lispm-ui--evict-icon-cache key))))

(defun lispm-ui-icon-names ()
  "Return sorted list of all available icon names."
  (let ((names (mapcar #'car lispm-ui-icons-catalog))
        (dir (expand-file-name "icons" lispm-ui-directory)))
    (maphash (lambda (k _v) (push k names)) lispm-ui--icon-registry)
    (when (file-directory-p dir)
      (dolist (f (directory-files dir nil "\\.svg\\'"))
        (push (file-name-sans-extension f) names)))
    (sort (delete-dups names) #'string<)))

(defun lispm-ui-icon-p (name)
  "Return non-nil when icon NAME exists in registry, catalog, or bundled SVG."
  (let ((key (lispm-ui--icon-key name)))
    (or (not (null (gethash key lispm-ui--icon-registry)))
        (not (null (lispm-ui-icons-catalog-entry key)))
        (file-readable-p (lispm-ui-icon-file name)))))

(defun lispm-ui--resolve-tint (tint)
  "Resolve TINT (keyword token, color string, or nil) to a `#rrggbb' string."
  (let ((raw (cond
              ((keywordp tint) (lispm-ui-token tint))
              ((stringp tint) tint)
              (t (lispm-ui-color :text)))))
    (or (and raw (lispm-ui-color-hex raw))
        raw
        "#000000")))

(cl-defun lispm-ui-icon-svg (name &key (size 16) tint)
  "Return rendered SVG string for icon NAME at SIZE pixels tinted to TINT."
  (let* ((key (lispm-ui--icon-key name))
         (reg (gethash key lispm-ui--icon-registry))
         (cat (lispm-ui-icons-catalog-entry key))
         (sz (or size 16))
         (color (lispm-ui--resolve-tint tint))
         (raw
          (cond
           ((plist-get reg :svg))
           ((and (plist-get reg :file) (file-readable-p (plist-get reg :file)))
            (with-temp-buffer
              (insert-file-contents (plist-get reg :file))
              (buffer-string)))
           ((plist-get cat :svg-body)
            (format
             "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%d\" height=\"%d\" viewBox=\"0 0 16 16\" fill=\"none\" stroke=\"%s\" stroke-width=\"1.5\" stroke-linecap=\"round\" stroke-linejoin=\"round\">%s</svg>"
             sz sz color (plist-get cat :svg-body)))
           ((file-readable-p (lispm-ui-icon-file name))
            (with-temp-buffer
              (insert-file-contents (lispm-ui-icon-file name))
              (buffer-string))))))
    (when raw
      (replace-regexp-in-string "currentColor" color raw t t))))

(defun lispm-ui-icon-file (name)
  "Return the file of icon NAME: an absolute file name as it is, as an
application's icon; for `emacs', the icon Emacs ships with, read from
this Emacs's own data directory, else the document icon; else the file
the first of `lispm-ui-icon-functions' finds, else LispM UI's own,
icons/NAME.svg, whose alert icons are `caution', `stop' and `note', and
whose file icons `folder', `disk', `document' and those
`lispm-ui-file-icon-name' names."
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

(defun lispm-ui-text-icon (name &optional tint)
  "Return what stands for icon NAME on a text terminal, see `lispm-ui-text-icons'.
Optional TINT specifies a color string or token overriding icon color inference."
  (let* ((key (lispm-ui--icon-key name))
         (reg (gethash key lispm-ui--icon-registry))
         (cat (and (not reg) (lispm-ui-icons-catalog-entry key)))
         (custom-glyph (or (plist-get reg :tty) (plist-get cat :tty)))
         (color (if tint
                    (lispm-ui--resolve-tint tint)
                  (with-memoization (gethash name lispm-ui--icon-colors)
                    (when-let* ((file (or (plist-get reg :file)
                                          (lispm-ui-icon-file name)))
                                ((file-readable-p file))
                                (pixels (lispm-ui-raster file 16)))
                      (lispm-ui-raster-color pixels)))))
         (base (if (symbolp name) (symbol-name name) (file-name-base name)))
         (match (and (not custom-glyph)
                     (car (sort (seq-filter (lambda (entry) (string-prefix-p (car entry) base))
                                            lispm-ui-text-icons)
                                (lambda (a b) (> (length (car a)) (length (car b))))))))
         (glyph (or custom-glyph (cdr match) "·")))
    (propertize (truncate-string-to-width glyph 1 nil ?\s)
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

(defun lispm-ui--icon-cached-svg-image (key sz color svg-fn)
  "Return cached SVG image for KEY at size SZ pixels in COLOR via SVG-FN."
  (let ((ckey (list key sz color)))
    (or (gethash ckey lispm-ui--icons)
        (when-let* ((svg (funcall svg-fn)))
          (when (> (hash-table-count lispm-ui--icons) 512)
            (clrhash lispm-ui--icons))
          (puthash ckey
                   (if (image-type-available-p 'svg)
                       (create-image svg 'svg t :width sz :height sz :ascent 'center)
                     (list 'image :type 'svg :data svg :width sz :height sz
                           :ascent 'center :scale 1))
                   lispm-ui--icons)))))

(cl-defun lispm-ui-icon (name &key size tint)
  "Return icon NAME as a propertized string one icon wide.
NAME is resolved via the icon registry, bundled files, or built-in catalog.
Optional SIZE (default 16) sets pixel dimensions; optional TINT colors
vector SVG icons or TTY fallback glyphs."
  (let* ((key (lispm-ui--icon-key name))
         (sz (or size 16))
         (reg (gethash key lispm-ui--icon-registry))
         (cat (lispm-ui-icons-catalog-entry key))
         (file (unless reg (lispm-ui-icon-file name))))
    (cond
     ((lispm-ui-tty-p) (lispm-ui-text-icon name tint))
     ((not (display-images-p))
      (propertize " " 'face 'lispm-ui-list 'display `(space :width (,sz))))
     ((plist-get reg :svg)
      (let ((color (lispm-ui--resolve-tint tint)))
        (propertize " " 'face 'lispm-ui-list
                    'display (lispm-ui--icon-cached-svg-image
                              key sz color
                              (lambda () (lispm-ui-icon-svg name :size sz :tint color))))))
     ((and (plist-get reg :file) (file-readable-p (plist-get reg :file)))
      (let* ((f (plist-get reg :file))
             (ckey (list f sz nil)))
        (propertize " " 'face 'lispm-ui-list
                    'display (or (gethash ckey lispm-ui--icons)
                                 (puthash ckey (create-image f nil nil :height sz :ascent 'center)
                                          lispm-ui--icons)))))
     ((and (or tint size) cat)
      (let ((color (lispm-ui--resolve-tint tint)))
        (propertize " " 'face 'lispm-ui-list
                    'display (lispm-ui--icon-cached-svg-image
                              key sz color
                              (lambda () (lispm-ui-icon-svg name :size sz :tint color))))))
     ((and file (file-readable-p file))
      (let ((cache-key (if size (list file sz nil) file)))
        (propertize " " 'face 'lispm-ui-list
                    'display (or (gethash cache-key lispm-ui--icons)
                                 (puthash cache-key (create-image file nil nil :height sz
                                                                  :ascent 'center)
                                          lispm-ui--icons)))))
     (cat
      (let ((color (lispm-ui--resolve-tint tint)))
        (propertize " " 'face 'lispm-ui-list
                    'display (lispm-ui--icon-cached-svg-image
                              key sz color
                              (lambda () (lispm-ui-icon-svg name :size sz :tint color))))))
     (t (propertize " " 'face 'lispm-ui-list 'display `(space :width (,sz)))))))

;;;; Files' icons

(defconst lispm-ui-lisp-extensions '("el" "lisp" "lsp" "cl" "scm" "ss" "rkt" "clj" "cljs"
                                     "cljc" "edn" "fnl")
  "Extensions of files shown with the Lisp document icon.")

(defconst lispm-ui-code-extensions
  '("c" "h" "cc" "cpp" "hpp" "m" "mm" "swift" "rs" "go" "zig" "py" "rb" "js" "jsx" "ts" "tsx"
    "java" "kt" "scala" "hs" "ml" "ex" "exs" "erl" "lua" "php" "sh" "bash" "zsh" "fish" "nix"
    "sol" "sql" "css" "scss" "html" "xml" "json" "yaml" "yml" "toml" "proto" "mk" "cmake")
  "Extensions of files shown with the code document icon.")

(defvar lispm-ui--image-file-regexp nil
  "Cached `(EXTENSIONS REGEXPS . REGEXP)' for `image-file-name-regexp'.")

(defun lispm-ui--image-file-p (file)
  "Return non-nil when FILE matches `image-file-name-regexp'."
  (let* ((exts (bound-and-true-p image-file-name-extensions))
         (rxps (bound-and-true-p image-file-name-regexps))
         (regexp (if (and lispm-ui--image-file-regexp
                          (equal (car lispm-ui--image-file-regexp) exts)
                          (equal (cadr lispm-ui--image-file-regexp) rxps))
                     (cddr lispm-ui--image-file-regexp)
                   (let ((r (image-file-name-regexp)))
                     (setq lispm-ui--image-file-regexp
                           (cons (copy-sequence exts)
                                 (cons (copy-sequence rxps) r)))
                     r))))
    (string-match-p regexp file)))

(defun lispm-ui-file-icon-name (file)
  "Return the name of the document icon of FILE, by its kind."
  (let ((extension (downcase (or (file-name-extension file) ""))))
    (cond ((lispm-ui--image-file-p file) "document-image")
          ((member extension lispm-ui-lisp-extensions) "document-lisp")
          ((member extension '("md" "markdown" "mkd")) "document-markdown")
          ((equal extension "org") "document-org")
          ((member extension lispm-ui-code-extensions) "document-code")
          (t "document"))))

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
