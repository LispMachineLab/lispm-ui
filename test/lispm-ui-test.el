;;; lispm-ui-test.el --- Tests of LispM UI  -*- lexical-binding: t; -*-

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

;; The tests of every part of LispM UI, run with `make test'; `make i18n'
;; runs `lispm-ui-test-report-catalogs'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'wid-edit)
(require 'lispm-ui)
(require 'lispm-kit-i18n-check)

(defconst lispm-ui-test--directory
  (file-name-directory (directory-file-name (file-name-directory (or load-file-name buffer-file-name))))
  "The directory of LispM UI's sources.")

(defun lispm-ui-test--wait (predicate &optional seconds)
  "Run the event loop until PREDICATE holds or SECONDS (default 5) pass."
  (let ((deadline (+ (float-time) (or seconds 5))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (funcall predicate)))

;;;; Standing alone

(defun lispm-ui-test--source-files ()
  "Return LispM UI's Lisp files."
  (directory-files lispm-ui-test--directory t "\\`lispm-.*\\.el\\'"))

(ert-deftest lispm-ui-test-knows-no-desktop ()
  "LispM UI knows of no desktop: no file of it but its tests names the
desktop's code or its look, as the App Store's tests check of every
package; a desktop dresses LispM UI, never the reverse."
  (dolist (file (directory-files-recursively lispm-ui-test--directory "" nil
                                             (lambda (dir)
                                               ;; .deps holds what CI checks out beside it.
                                               (not (member (file-name-nondirectory dir) '(".git" ".deps" "test"))))))
    (unless (member (file-name-nondirectory file) '("LICENSE"))
      (with-temp-buffer
        (insert-file-contents file)
        (let ((case-fold-search t))
          (should-not (and (re-search-forward (concat "lispm-" "desktop\\|" "plat" "inum") nil t)
                           (format "%s:%d" (file-relative-name file lispm-ui-test--directory)
                                   (line-number-at-pos)))))))))

(ert-deftest lispm-ui-test-requires-only-itself ()
  "LispM UI requires no LispM package but LispM Kit, whose engine
translates its words: each part requires Emacs's own libraries, parts of
LispM UI and of LispM Kit, and the main file every part."
  (dolist (file (lispm-ui-test--source-files))
    (with-temp-buffer
      (insert-file-contents file)
      (emacs-lisp-mode)
      (while (re-search-forward "(require '\\([^ )]+\\)" nil t)
        (let ((feature (match-string 1)))
          (unless (nth 8 (syntax-ppss))
            (should-not (and (string-prefix-p "lispm-" feature)
                             (not (string-prefix-p "lispm-ui" feature))
                             (not (string-prefix-p "lispm-kit" feature))
                             (format "%s requires %s" (file-name-nondirectory file) feature))))))))
  (let ((main (with-temp-buffer
                (insert-file-contents (expand-file-name "lispm-ui.el" lispm-ui-test--directory))
                (buffer-string))))
    (dolist (file (lispm-ui-test--source-files))
      (let ((feature (file-name-base file)))
        ;; lispm-form.el is required only by what asks for its names.
        (unless (member feature '("lispm-ui" "lispm-form"))
          (should (string-search (format "(require '%s)" feature) main)))))))

(ert-deftest lispm-ui-test-installed-without-tests ()
  "Installed as a package, LispM UI's tests are not byte-compiled with it:
they load what only a checkout has, as the goldens, and compiling them
would report an error where nothing is wrong."
  (require 'package)
  (let ((ignored (package--parse-elpaignore
                  (package-desc-create :name 'lispm-ui :dir lispm-ui-test--directory))))
    (should (seq-some (lambda (regexp)
                        (string-match-p regexp (expand-file-name "test" lispm-ui-test--directory)))
                      ignored))
    (should-not (seq-some (lambda (regexp)
                            (string-match-p regexp (expand-file-name "lispm-ui.el" lispm-ui-test--directory)))
                          ignored))))

;;;; lispm-ui-base

(ert-deftest lispm-ui-test-base ()
  "A batch Emacs is no text terminal, and lengths stay pixels there; a
text terminal counts a cell for eight pixels."
  (should (file-directory-p (expand-file-name "i18n" lispm-ui-directory)))
  (should-not (lispm-ui-tty-p))
  (should (= (lispm-ui-px 12) 12))
  (cl-letf (((symbol-function 'lispm-ui-tty-p) (lambda (&rest _) t)))
    (should (= (lispm-ui-px 12) 2))
    (should (= (lispm-ui-px 3) 0))))

;;;; What the desktop drew

(require 'lispm-ui-golden (expand-file-name "test/lispm-ui-golden.el" lispm-ui-test--directory))

(ert-deftest lispm-ui-test-draws-as-the-desktop-drew ()
  "LispM UI's palettes, drawings, lines and forms are those LispM Desktop
made before they moved here, see test/lispm-ui-golden.el.  What needs
XML parsed is left out where Emacs cannot parse it, and forms drawn with
Emacs's widgets where Emacs is of another version than wrote them."
  (let* ((golden (lispm-ui-golden-read))
         (emacs (alist-get :emacs golden)))
    (setq golden (assq-delete-all :emacs golden))
    (should (equal (mapcar #'car golden) (mapcar #'car lispm-ui-golden-cases)))
    (pcase-dolist (`(,name ,form) lispm-ui-golden-cases)
      (unless (or (and (memq name lispm-ui-golden-xml-cases)
                       (not (fboundp 'libxml-parse-xml-region)))
                  (and (memq name lispm-ui-golden-widget-cases)
                       (not (eql emacs emacs-major-version))))
        (should (equal (list name (lispm-ui-golden-eval form))
                       (list name (alist-get name golden))))))))

;;;; lispm-ui-palette

(defmacro lispm-ui-test--with-theme (colors &rest body)
  "Run BODY with the theme's faces giving COLORS, (CONTENT TEXT DIM
ACCENT), LispM UI's palette, its faces and its hooks as they are kept
apart from the rest of Emacs."
  (declare (indent 1))
  `(pcase-let ((`(,content ,text ,dim ,accent) ,colors))
     (let ((lispm-ui-current-palette nil)
           (lispm-ui-palette-generation 0)
           (lispm-ui-palette-functions nil)
           (lispm-ui-palette-changed-hook nil)
           (faces (mapcar (lambda (face) (cons face (copy-sequence (face-all-attributes face))))
                          '(lispm-ui-title lispm-ui-title-inactive lispm-ui-small lispm-ui-field lispm-ui-field-focused
                            lispm-ui-button lispm-ui-button-pressed))))
       (cl-letf (((symbol-function 'face-background)
                  (lambda (face &rest _) (and (eq face 'default) content)))
                 ((symbol-function 'face-foreground)
                  (lambda (face &rest _)
                    (pcase face ('default text) ('shadow dim) ('font-lock-constant-face accent)))))
         (unwind-protect (progn ,@body)
           (pcase-dolist (`(,face . ,attributes) faces)
             (apply #'set-face-attribute face nil
                    (apply #'append (mapcar (pcase-lambda (`(,key . ,value)) (list key value))
                                            attributes)))))))))

(ert-deftest lispm-ui-test-palette-update ()
  "The palette is derived from the theme's faces, the colours a function
of `lispm-ui-palette-functions' names put in place, a later function's
winning; each is a generation later, LispM UI's faces take its colours,
and `lispm-ui-palette-changed-hook' runs.  A theme whose colours cannot
be read leaves the palette as it was."
  (lispm-ui-test--with-theme '("#DDDDDD" "#000000" "#777777" "#6363CE")
    (let ((changed 0))
      (add-hook 'lispm-ui-palette-changed-hook (lambda () (setq changed (1+ changed))))
      (should (equal (lispm-ui-palette-update)
                     (lispm-ui-derive-palette "#DDDDDD" "#000000" "#777777" "#6363CE")))
      (should (= lispm-ui-palette-generation 1))
      (should (= changed 1))
      (should (equal (face-attribute 'lispm-ui-field :background) "#DDDDDD"))
      ;; Drawn over a theme's `widget-field', a field takes none of its lines.
      (dolist (line '(:underline :overline :strike-through))
        (should (null (face-attribute 'lispm-ui-field line)))
        (should (null (face-attribute 'lispm-ui-form-area line))))
      (should (equal (plist-get (face-attribute 'lispm-ui-field-focused :box) :color) "#6363CE"))
      (should (equal (face-attribute 'lispm-ui-button :background) (lispm-ui-color :chrome)))
      (should (eq (face-attribute 'lispm-ui-small :inherit) 'lispm-ui-list))
      (should (equal (face-attribute 'lispm-ui-title-inactive :foreground) (lispm-ui-color :muted)))
      (should (eq (face-attribute 'lispm-ui-title-inactive :inherit) 'unspecified))
      (setq lispm-ui-palette-functions (list (lambda () '(:chrome "#CCCCCC" :focus "#FF0000"))
                                             (lambda () '(:focus "#00FF00"))))
      (lispm-ui-palette-update)
      (should (equal (lispm-ui-color :chrome) "#CCCCCC"))
      (should (equal (lispm-ui-color :focus) "#00FF00"))
      (should (equal (lispm-ui-color :outline) "#212121"))
      (should (= lispm-ui-palette-generation 2))
      (let ((palette lispm-ui-current-palette))
        (cl-letf (((symbol-function 'face-background) #'ignore))
          (should-not (lispm-ui-palette-update)))
        (should (eq lispm-ui-current-palette palette))
        (should (= lispm-ui-palette-generation 2))
        (should (= changed 2))))))

(ert-deftest lispm-ui-test-palette-follows-theme ()
  "Following the theme derives the palette at once, and again as a theme
is enabled."
  (lispm-ui-test--with-theme '("#282a36" "#f8f8f2" "#6272a4" "#bd93f9")
    (let ((enable-theme-functions nil))
      (lispm-ui-palette-follow-theme)
      (should (memq #'lispm-ui-palette-update enable-theme-functions))
      (should (equal (lispm-ui-color :content) "#282a36"))
      (run-hook-with-args 'enable-theme-functions 'some-theme)
      (should (= lispm-ui-palette-generation 2)))))

(ert-deftest lispm-ui-test-greys ()
  "In the classic palette every grey is itself; in another, the greys
above the chrome lean toward its light, those below toward its outline."
  (let ((classic '(:chrome "#dddddd" :light "#ffffff" :outline "#000000")))
    (dolist (level '(#x00 #x44 #x88 #xCC #xDD #xEE #xFF))
      (should (equal (lispm-ui-grey classic level) (format "#%02x%02x%02x" level level level)))))
  (let ((other '(:chrome "#404040" :light "#808080" :outline "#000000")))
    (should (equal (lispm-ui-grey other #xDD) "#404040"))
    (should (equal (lispm-ui-grey other #xFF) "#808080"))
    (should (equal (lispm-ui-grey other #x00) "#000000"))))

(ert-deftest lispm-ui-test-palette-on-black ()
  "A theme on black has a chrome its controls' dark edges and outlines
stand out from, lifted toward its text; its content stays black, and a
theme light enough keeps the chrome its content gives."
  (let ((palette (lispm-ui-derive-palette "#000000" "#ffffff" "#989898" "#2fafff")))
    (should (equal (plist-get palette :content) "#000000"))
    (should (> (lispm-ui-luminance (plist-get palette :chrome))
               (+ (lispm-ui-luminance (plist-get palette :outline)) 0.05)))
    (should (> (lispm-ui-luminance (lispm-ui-grey palette #xEE))
               (+ (lispm-ui-luminance (lispm-ui-grey palette #x00)) 0.1))))
  (should (equal (plist-get (lispm-ui-derive-palette "#282a36" "#f8f8f2" "#6272a4" "#bd93f9") :chrome)
                 (lispm-ui-blend "#282a36" "#000000" 0.25)))
  ;; Colours a text terminal cannot resolve are left to it.
  (should (equal (lispm-ui--palette-ground "unspecified-bg" "#ffffff") "unspecified-bg")))

;;;; lispm-ui-look

(cl-defgeneric lispm-ui-test--drawn-by ()
  "Which method draws, for the look dispatch test."
  'classic)

(cl-defmethod lispm-ui-test--drawn-by (&context (lispm-ui-look (lispm-ui-look-family test-pc)))
  (list 'family (cl-call-next-method)))

(cl-defmethod lispm-ui-test--drawn-by (&context (lispm-ui-look (eql test-win)))
  (list 'own (cl-call-next-method)))

(ert-deftest lispm-ui-test-looks ()
  "A look is defined once, again replacing its properties; its title is
its own or its name; its family and metrics are read from it.  A look's
own method comes before its family's, which comes before the classic
default."
  (let ((lispm-ui-looks nil))
    (should (eq (lispm-ui-look-define 'test-win :title "Windows" :family 'test-pc
                                      :metrics '(:border 4))
                'test-win))
    (lispm-ui-look-define 'test-os2 :family 'test-pc :system-families '("Helv"))
    (lispm-ui-look-define 'test-win :title "Windows 3.2" :family 'test-pc :metrics '(:border 4))
    (should (equal (mapcar #'car lispm-ui-looks) '(test-win test-os2)))
    (should (equal (lispm-ui-look-title 'test-win) "Windows 3.2"))
    (should (equal (lispm-ui-look-title 'test-os2) "Test-Os2"))
    (should (eq (lispm-ui-look-family 'test-os2) 'test-pc))
    (should (= (lispm-ui-look-metric :border 'test-win) 4))
    (let ((lispm-ui-look 'test-os2))
      (should (equal (lispm-ui-look-families :system-families '("Default")) '("Helv")))
      (should (equal (lispm-ui-look-families :list-families '("Default")) '("Default")))
      (should (equal (lispm-ui-test--drawn-by) '(family classic))))
    (let ((lispm-ui-look 'test-win))
      (should (equal (lispm-ui-test--drawn-by) '(own (family classic)))))
    (let ((lispm-ui-look 'classic))
      (should (eq (lispm-ui-test--drawn-by) 'classic)))))

;;;; lispm-ui-text

(ert-deftest lispm-ui-test-text ()
  "Text is measured in its faces over a face beneath; fitted to a width,
it ends in an ellipsis in its first face once cut; a space is so wide, or
reaches so far from the left or the right."
  (should (= (lispm-ui-text-width "Hello") 5))
  (should (equal (lispm-ui-text-fit "Hello" 5) "Hello"))
  (let ((fitted (lispm-ui-text-fit (propertize "Hello, world" 'face 'bold) 6)))
    (should (equal fitted "Hello…"))
    (should (eq (get-text-property 5 'face fitted) 'bold)))
  (should (equal (lispm-ui-text-fit "ab cd" 4) "ab…"))
  (should (equal (get-text-property 0 'display (lispm-ui-text-space 7)) '(space :width (7))))
  (should (equal (get-text-property 0 'display (lispm-ui-text-space -2)) '(space :width (0))))
  (should (equal (get-text-property 0 'display (lispm-ui-text-to 40)) '(space :align-to (40))))
  (should (equal (get-text-property 0 'display (lispm-ui-text-to 8 t))
                 '(space :align-to (- right (8))))))

(ert-deftest lispm-ui-test-cjk-family ()
  "Chinese and Japanese are drawn in a family for the language spoken,
the first installed; another language takes Simplified Chinese's."
  (cl-letf (((symbol-function 'font-family-list)
             (lambda (&rest _) '("Noto Sans CJK JP" "PingFang SC" "Geneva"))))
    (let ((lispm-ui-language 'ja))
      (should (equal (lispm-ui-text-cjk-family) "Noto Sans CJK JP")))
    (let ((lispm-ui-language 'de))
      (should (equal (lispm-ui-text-cjk-family) "PingFang SC")))
    (let ((lispm-ui-language 'zh-Hant))
      (should-not (lispm-ui-text-cjk-family)))))

;;;; lispm-ui-draw

(ert-deftest lispm-ui-test-image-cache ()
  "An image is drawn once for its key in a palette, again in the next
palette, and apart for each look; the cache is emptied past 512 images."
  (let ((lispm-ui--images (make-hash-table :test #'equal))
        (lispm-ui-palette-generation 1)
        (drawn 0))
    (cl-flet ((draw () (setq drawn (1+ drawn)) "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"2\" height=\"2\"/>"))
      (let ((image (lispm-ui-image '(dot 1) #'draw)))
        (should (eq (car image) 'image))
        (should (eq (lispm-ui-image '(dot 1) #'draw) image)))
      (should (= drawn 1))
      (setq lispm-ui-palette-generation 2)
      (lispm-ui-image '(dot 1) #'draw)
      (should (= drawn 2))
      (let ((lispm-ui-look 'classic)) (lispm-ui-look-image '(dot 1) #'draw))
      (let ((lispm-ui-look 'other)) (lispm-ui-look-image '(dot 1) #'draw))
      (should (= drawn 4))
      (dotimes (i 520) (puthash (list 'filler i) t lispm-ui--images))
      (lispm-ui-image '(dot 2) #'draw)
      (should (= (hash-table-count lispm-ui--images) 1)))))

(ert-deftest lispm-ui-test-svg-named-twice ()
  "An SVG an editor wrote, an attribute of its own named as one of SVG's
once the parser drops its namespace, is embedded with each attribute
once, its own first, so that the picture is still a document."
  (skip-unless (fboundp 'libxml-parse-xml-region))
  (let* ((lispm-ui--svg-doms (make-hash-table :test #'equal))
         (dom (lispm-ui-svg-dom nil (concat "<svg xmlns=\"http://www.w3.org/2000/svg\""
                                            " xmlns:inkscape=\"http://www.inkscape.org/namespaces/inkscape\""
                                            " version=\"1.1\" inkscape:version=\"0.48.4\" width=\"4\">"
                                            "<rect width=\"4\" height=\"4\"/></svg>")))
         (svg (svg-create 8 8)))
    (should (equal (seq-filter (lambda (attribute) (eq (car attribute) 'version)) (dom-attributes dom))
                   '((version . "1.1"))))
    (lispm-ui-svg-embed svg dom 0 0 8 8)
    (with-temp-buffer
      (svg-print svg)
      (should (= (how-many "version=" (point-min) (point-max)) 2))
      (should (libxml-parse-xml-region (point-min) (point-max))))))

(ert-deftest lispm-ui-test-svg-scaled ()
  "An SVG sized in pixels without a viewBox, embedded smaller or larger, is
scaled to fit, not cut; one of its own size, or with a viewBox, or sized
in other units, is embedded as it is."
  (skip-unless (fboundp 'libxml-parse-xml-region))
  (let ((lispm-ui--svg-doms (make-hash-table :test #'equal)))
    (cl-flet ((embedded (source width height)
                (let ((svg (svg-create 64 64)))
                  (lispm-ui-svg-embed svg (lispm-ui-svg-dom nil source) 0 0 width height)
                  (dom-attr (car (dom-children svg)) 'viewBox))))
      (should (equal (embedded "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"333\" height=\"233px\"/>" 64 64)
                     "0 0 333 233"))
      (should-not (embedded "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"16\" height=\"16\"/>" 16 16))
      (should (equal (embedded "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"40\" height=\"40\" viewBox=\"0 0 4 4\"/>" 64 64)
                     "0 0 4 4"))
      (should-not (embedded "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"10mm\" height=\"10mm\"/>" 64 64)))))

;;;; lispm-ui-raster

(defun lispm-ui-test--svg (string)
  "Return the SVG document STRING parsed."
  (with-temp-buffer
    (insert string)
    (libxml-parse-xml-region (point-min) (point-max))))

(ert-deftest lispm-ui-test-raster-rectangles ()
  "An icon of rectangles comes out pixel for pixel at its own size, and at
half its size each pixel the average of the four it stands for; what no
shape covers is clear."
  (skip-unless (fboundp 'libxml-parse-xml-region))
  (let ((dom (lispm-ui-test--svg
              "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"4\" height=\"4\" viewBox=\"0 0 4 4\">
                 <rect x=\"0\" y=\"0\" width=\"2\" height=\"2\" fill=\"#FF0000\"/>
                 <rect x=\"2\" y=\"0\" width=\"2\" height=\"1\" fill=\"#0000FF\"/>
                 <rect x=\"2\" y=\"1\" width=\"2\" height=\"1\" fill=\"#FFFFFF\"/>
                 <rect x=\"0\" y=\"2\" width=\"4\" height=\"2\" fill=\"#00FF00\" display=\"none\"/>
               </svg>")))
    (let ((pixels (lispm-ui-raster-dom dom 4)))
      (should (equal (aref pixels 0) ["#ff0000" "#ff0000" "#0000ff" "#0000ff"]))
      (should (equal (aref pixels 1) ["#ff0000" "#ff0000" "#ffffff" "#ffffff"]))
      (should (equal (aref pixels 2) [nil nil nil nil])))
    (let ((pixels (lispm-ui-raster-dom dom 2)))
      (should (equal (aref pixels 0) ["#ff0000" "#8080ff"]))
      (should (equal (aref pixels 1) [nil nil])))))

(ert-deftest lispm-ui-test-raster-crisp ()
  "An icon asking for crisp edges keeps its colours at half its size: each
pixel takes the colour covering most of the four it stands for, the
darker of two covering as much, so a pixel-wide outline survives, but
the lighter amid the darker, so what close outlines outline survives."
  (skip-unless (fboundp 'libxml-parse-xml-region))
  (let ((pixels (lispm-ui-raster-dom
                 (lispm-ui-test--svg
                  "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"4\" height=\"4\" viewBox=\"0 0 4 4\" shape-rendering=\"crispEdges\">
                     <rect x=\"0\" y=\"0\" width=\"2\" height=\"2\" fill=\"#000000\"/>
                     <rect x=\"1\" y=\"1\" width=\"1\" height=\"1\" fill=\"#FFFFFF\"/>
                     <rect x=\"2\" y=\"0\" width=\"2\" height=\"1\" fill=\"#0000FF\"/>
                     <rect x=\"2\" y=\"1\" width=\"2\" height=\"1\" fill=\"#FFFFFF\"/>
                   </svg>")
                 2)))
    (should (equal (aref pixels 0) ["#000000" "#0000ff"]))
    (should (equal (aref pixels 1) [nil nil])))
  (let ((pixels (lispm-ui-raster-dom
                 (lispm-ui-test--svg
                  "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"4\" height=\"4\" viewBox=\"0 0 4 4\" shape-rendering=\"crispEdges\">
                     <rect x=\"0\" y=\"0\" width=\"4\" height=\"4\" fill=\"#000000\"/>
                     <rect x=\"1\" y=\"0\" width=\"1\" height=\"2\" fill=\"#FFFFFF\"/>
                   </svg>")
                 2)))
    (should (equal (aref pixels 0) ["#ffffff" "#000000"]))))

(ert-deftest lispm-ui-test-raster-shapes ()
  "Paths are filled, their arcs followed; a circle is round; an outline
stays a pixel wide however small the icon, and one of no width is none."
  (skip-unless (fboundp 'libxml-parse-xml-region))
  (let ((pixels (lispm-ui-raster-dom
                 (lispm-ui-test--svg
                  "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 8 8\">
                     <path d=\"M0 0H8L0 8Z\" fill=\"#000000\"/>
                   </svg>")
                 8)))
    ;; The upper left triangle: the corner in, the opposite corner out.
    (should (aref (aref pixels 0) 0))
    (should (aref (aref pixels 0) 6))
    (should-not (aref (aref pixels 7) 7)))
  (let ((pixels (lispm-ui-raster-dom
                 (lispm-ui-test--svg
                  "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 8 8\">
                     <path d=\"M0 4A4 4 0 0 1 8 4A4 4 0 0 1 0 4Z\" style=\"fill:#0000ff\"/>
                   </svg>")
                 8)))
    (should (equal (aref (aref pixels 4) 4) "#0000ff"))
    (should (aref (aref pixels 0) 4))
    (should-not (aref (aref pixels 0) 0)))
  (cl-flet ((outlined (width)
              (lispm-ui-raster-dom
               (lispm-ui-test--svg
                (format "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 64 64\">
                           <rect x=\"8\" y=\"8\" width=\"48\" height=\"48\" fill=\"none\"
                                 stroke=\"#000000\" stroke-width=\"%s\"/>
                         </svg>" width))
               8)))
    (should (equal (aref (aref (outlined "1") 1) 4) "#000000"))
    (should-not (aref (aref (outlined "0") 1) 4))))

(ert-deftest lispm-ui-test-text-picture ()
  "Pixels are drawn 2 by 4 a cell, in the block character and two of
their colours that draw them closest: one colour a blank on it, halves
the half blocks; what is clear keeps no colour where the character can
leave it so, and is the background where it cannot.  Darkened, every
colour is at half its brightness."
  (let* ((r "#ff0000") (b "#0000ff")
         (lines (lispm-ui-text-picture
                 (vector (vector r r r r nil nil r b nil nil)
                         (vector r r r r nil nil r b nil nil)
                         (vector r r nil nil b b r b nil nil)
                         (vector r r nil nil b b r b nil nil))
                 nil "#000000")))
    (should (equal (mapcar #'substring-no-properties lines) '(" ▀▄▌ ")))
    (should (equal (get-text-property 0 'face (car lines)) `(:background ,r)))
    (should (equal (get-text-property 1 'face (car lines)) `(:foreground ,r)))
    (should (equal (get-text-property 2 'face (car lines)) `(:foreground ,b)))
    (should (equal (get-text-property 3 'face (car lines)) `(:foreground ,r :background ,b)))
    (should-not (get-text-property 4 'face (car lines))))
  (let ((line (car (lispm-ui-text-picture
                    [["#00ff00" "#00ff00"] ["#00ff00" "#00ff00"] ["#00ff00" "#00ff00"] [nil nil]]
                    nil "#123456"))))
    (should (equal line "▂"))
    (should (equal (get-text-property 0 'face line) '(:foreground "#123456" :background "#00ff00"))))
  (should (equal (get-text-property 0 'face (car (lispm-ui-text-picture
                                                  [["#ff0000" "#ff0000"] ["#ff0000" "#ff0000"]
                                                   ["#ff0000" "#ff0000"] ["#ff0000" "#ff0000"]]
                                                  t)))
                 '(:background "#800000"))))

(ert-deftest lispm-ui-test-raster-png ()
  "Pixels make an opaque PNG: its chunks summed right, the pixels each a
square SCALE wide in the middle, the rest the background, clear pixels
too."
  (should (= (lispm-ui-raster--crc "IEND") #xAE426082))
  (let* ((png (lispm-ui-raster-png [["#ff0000" nil] [nil "#0000ff"]] 2 6 4 "#00ff00"))
         (data-at (string-search "IDAT" png))
         (length (string-to-number (mapconcat (lambda (byte) (format "%02x" byte))
                                              (substring png (- data-at 4) data-at) "")
                                   16))
         (raw (with-temp-buffer
                (set-buffer-multibyte nil)
                (insert (substring png (+ data-at 4) (+ data-at 4 length)))
                ;; An Emacs built without zlib has none of it.
                (and (fboundp 'zlib-available-p) (zlib-available-p)
                     (zlib-decompress-region (point-min) (point-max))
                     (buffer-string)))))
    (should (string-prefix-p "\x89PNG\r\n\x1a\n" png))
    (should-not (multibyte-string-p png))
    (should (equal (substring png 16 24) (unibyte-string 0 0 0 6 0 0 0 4)))
    (should (string-suffix-p (unibyte-string #xAE #x42 #x60 #x82) png))
    (when raw
      ;; Each row: no filter, then 6 pixels; the picture 4 by 4 from x 1.
      (should (= (length raw) (* 4 (1+ (* 3 6)))))
      (cl-flet ((pixel (x y) (let ((at (+ (* y 19) 1 (* 3 x)))) (append (substring raw at (+ at 3)) nil))))
        (should (equal (pixel 0 0) '(0 255 0)))
        (should (equal (pixel 1 0) '(255 0 0)))
        (should (equal (pixel 2 1) '(255 0 0)))
        (should (equal (pixel 3 0) '(0 255 0)))
        (should (equal (pixel 4 3) '(0 0 255)))
        (should (equal (pixel 5 3) '(0 255 0)))))))

(ert-deftest lispm-ui-test-text-compose ()
  "Pieces of text stand where they are put on blank lines as wide as asked,
cut at the right edge."
  (should (equal (lispm-ui-text-compose 8 3 '((0 2 "ab") (2 0 "x") (2 6 "long")))
                 '("  ab    " "        " "x     lo"))))

(ert-deftest lispm-ui-test-raster-icon-color ()
  "Where only a character's room is left, an icon is drawn in the colour it
is drawn in most, greys only where it has no other: the caution sign in
its yellow, the note in its white."
  (skip-unless (fboundp 'libxml-parse-xml-region))
  (should (equal (lispm-ui-raster-color (lispm-ui-raster (lispm-ui-icon-file 'caution) 16)) "#ffe863"))
  (should (equal (lispm-ui-raster-color (lispm-ui-raster (lispm-ui-icon-file 'note) 16)) "#ffffff"))
  (should-not (lispm-ui-raster (expand-file-name "no-such-icon.svg" lispm-ui-directory) 16))
  (should-not (lispm-ui-raster-color [[nil nil] [nil nil]])))

;;;; lispm-ui-icons

(ert-deftest lispm-ui-test-icon-file ()
  "An icon is found by its name: an absolute file name is itself; the
functions of `lispm-ui-icon-functions' are asked first, in turn; else it
is LispM UI's own, as the alerts' icons are."
  (let ((lispm-ui-icon-functions nil))
    (dolist (icon '(caution stop note))
      (should (file-readable-p (lispm-ui-icon-file icon)))
      (should (equal (lispm-ui-icon-file icon)
                     (expand-file-name (format "icons/%s.svg" icon) lispm-ui-directory))))
    (should (equal (lispm-ui-icon-file "/apps/ada/icon.png") "/apps/ada/icon.png"))
    (should (string-suffix-p "/emacs.svg" (lispm-ui-icon-file 'emacs)))
    (let ((lispm-ui-icon-functions
           (list (lambda (name) (and (equal name "folder") "/looks/folder.svg"))
                 (lambda (name) (and (member name '("folder" "trash")) "/os/trash.svg")))))
      (should (equal (lispm-ui-icon-file "folder") "/looks/folder.svg"))
      (should (equal (lispm-ui-icon-file "trash") "/os/trash.svg"))
      (should (equal (lispm-ui-icon-file 'caution)
                     (expand-file-name "icons/caution.svg" lispm-ui-directory)))
      (should (equal (lispm-ui-icon-file "/apps/ada/icon.png") "/apps/ada/icon.png")))
    (let ((lispm-ui-icon-functions (list (lambda (name) (and (equal name "document") "/os/document.svg"))))
          (data-directory "/nowhere/"))
      (should (equal (lispm-ui-icon-file 'emacs) "/os/document.svg")))))

(ert-deftest lispm-ui-test-icon-source ()
  "An icon drawn sixteen pixels a side is drawn from its own sixteen-pixel
drawing where it has one beside its thirty-two-pixel one."
  (let* ((directory (make-temp-file "lispm-ui-icons" t))
         (lispm-ui-icon-functions
          (list (lambda (name) (expand-file-name (format "%s.svg" name) directory)))))
    (unwind-protect
        (progn
          (dolist (file '("trash.svg" "trash-32.svg" "globe-32.svg"))
            (with-temp-file (expand-file-name file directory)
              (insert "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"16\" height=\"16\"/>")))
          (should (equal (lispm-ui--icon-source "trash-32" 16) (expand-file-name "trash.svg" directory)))
          (should (equal (lispm-ui--icon-source "trash-32" 32) (expand-file-name "trash-32.svg" directory)))
          (should (equal (lispm-ui--icon-source "globe-32" 16) (expand-file-name "globe-32.svg" directory)))
          (should (equal (lispm-ui--icon-source (expand-file-name "trash-32.svg" directory) 16)
                         (expand-file-name "trash.svg" directory))))
      (delete-directory directory t))))

(ert-deftest lispm-ui-test-icon ()
  "An icon is a string a sixteen-pixel image shows, kept for the next time,
or a blank as wide where images cannot be shown; in the list's face, so
that a button it sits in does not underline it."
  (let ((lispm-ui--icons (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) (image-type-available-p 'svg))))
      (when (display-images-p)
        (let ((icon (lispm-ui-icon 'caution)))
          (should (equal icon " "))
          (should (eq (get-text-property 0 'face icon) 'lispm-ui-list))
          (should (eq (car (get-text-property 0 'display icon)) 'image))
          (should (equal (plist-get (cdr (get-text-property 0 'display icon)) :height) 16))
          (should (eq (get-text-property 0 'display (lispm-ui-icon 'caution))
                      (get-text-property 0 'display icon))))))
    (cl-letf (((symbol-function 'display-images-p) #'ignore))
      (should (equal (get-text-property 0 'display (lispm-ui-icon 'caution)) '(space :width (16)))))))

(ert-deftest lispm-ui-test-tty-text-icons ()
  "On a text terminal an icon is a character a cell wide: the entry of
`lispm-ui-text-icons' starting its name longest, else a dot; in the
colour the icon is drawn in most, where it can be drawn."
  (let ((lispm-ui-current-palette (apply #'lispm-ui-derive-palette lispm-ui-test--classic))
        (lispm-ui--icon-colors (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'lispm-ui-tty-p) (lambda (&optional _) t)))
      (should (equal (lispm-ui-icon "folder-git-32") "▪"))
      (should (equal (lispm-ui-icon "document-lisp") "λ"))
      (should (equal (lispm-ui-icon "document-code") "▫"))
      (should (equal (lispm-ui-icon 'emacs) "◆"))
      (should (equal (lispm-ui-icon "/some/app/icon.svg") "·"))
      (should (eq (get-text-property 0 'face (lispm-ui-icon "folder-git-32")) 'lispm-ui-list))
      (when (fboundp 'libxml-parse-xml-region)
        (should (equal (lispm-ui-icon 'stop) "×"))
        (should (equal (get-text-property 0 'face (lispm-ui-icon 'stop))
                       '(:inherit lispm-ui-list :foreground "#ce3131")))))))

(ert-deftest lispm-ui-test-icon-picture ()
  "On a text terminal an icon can be drawn in block characters, eight
cells by four lines for sixteen pixels, darkened when selected; each is
drawn once.  One that is no SVG cannot be drawn."
  (skip-unless (fboundp 'libxml-parse-xml-region))
  (let ((lispm-ui--icon-pictures (make-hash-table :test #'equal)))
    (let ((picture (lispm-ui-icon-picture 'caution 16 nil "#DDDDDD")))
      (should (= (length picture) 4))
      (should (seq-every-p (lambda (line) (= (string-width line) 8)) picture))
      (should (eq (lispm-ui-icon-picture 'caution 16 nil "#DDDDDD") picture))
      (should-not (cl-every #'equal-including-properties
                            (lispm-ui-icon-picture 'caution 16 t "#DDDDDD") picture)))
    (should (equal (lispm-ui-text-size 16) '(8 . 4)))
    (should-not (lispm-ui-icon-picture "/apps/ada/icon.png" 16))))

;;;; lispm-ui-controls

(defmacro lispm-ui-test--with-controls (&rest body)
  "Run BODY in a buffer of its own, controls drawn in the classic palette."
  (declare (indent 0))
  `(let ((lispm-ui-current-palette (lispm-ui-derive-palette "#DDDDDD" "#000000" "#777777" "#6363CE"))
         (lispm-ui-palette-generation 1)
         (lispm-ui--images (make-hash-table :test #'equal)))
     (with-temp-buffer
       ,@body)))

(ert-deftest lispm-ui-test-controls-in-buffer ()
  "Controls inserted in a buffer are buttons Tab goes between, past a
disabled push button; the control point is on is drawn focused, the one
it left plain again."
  (lispm-ui-test--with-controls
    (let ((pressed nil))
      (lispm-ui-insert-push-button "OK" (lambda () (push 'ok pressed)) :default t)
      (insert " ")
      (lispm-ui-insert-push-button "No" #'ignore :disabled t)
      (insert " ")
      (lispm-ui-insert-radio-button "Red" t (lambda () (push 'red pressed)))
      (insert " ")
      (lispm-ui-insert-check-box "Bold" nil (lambda () (push 'bold pressed)))
      (goto-char (point-min))
      (should (button-at (point)))
      (should (equal (get-text-property (point) 'lispm-ui-label) "OK"))
      (let ((ok (point)))
        (should (equal (get-text-property ok 'display) (get-text-property ok 'lispm-ui-plain)))
        (lispm-ui-show-focus)
        (should (equal (get-text-property ok 'display) (get-text-property ok 'lispm-ui-focused)))
        (should (= lispm-ui-focus ok))
        (forward-button 1)
        ;; The disabled button is passed by.
        (should (string-prefix-p " Red" (buffer-substring (1+ (point)) (line-end-position))))
        (lispm-ui-show-focus)
        (should (equal (get-text-property ok 'display) (get-text-property ok 'lispm-ui-plain)))
        (should (equal (get-text-property (point) 'display) (get-text-property (point) 'lispm-ui-focused)))
        (push-button)
        (should (equal pressed '(red)))
        (forward-button 1)
        (push-button)
        (should (equal pressed '(bold red)))))))

(ert-deftest lispm-ui-test-push-from-keyboard ()
  "Return and Space press a push button, C-m being Return as the HHKB
types it: drawn pushed in for a button's blink, then plain again, its
action called."
  (should (eq (keymap-lookup lispm-ui-push-button-map "RET") #'lispm-ui-push))
  (should (eq (lookup-key lispm-ui-push-button-map (kbd "C-m")) #'lispm-ui-push))
  (should (eq (keymap-lookup lispm-ui-push-button-map "SPC") #'lispm-ui-push))
  (lispm-ui-test--with-controls
    (let ((pressed 0) (shown nil))
      (lispm-ui-insert-push-button "OK" (lambda () (setq pressed (1+ pressed))))
      (goto-char (point-min))
      (cl-letf (((symbol-function 'redisplay) #'ignore)
                ((symbol-function 'sit-for)
                 (lambda (seconds)
                   (push (list seconds (get-text-property (point-min) 'display)) shown))))
        (let ((last-input-event ?\r))
          (lispm-ui-push)))
      (should (= pressed 1))
      (should (equal shown (list (list (alist-get 'button lispm-ui-modal-flashes)
                                       (get-text-property (point-min) 'lispm-ui-pressed)))))
      (should (equal (get-text-property (point-min) 'display)
                     (get-text-property (point-min) 'lispm-ui-plain))))))

(ert-deftest lispm-ui-test-popup-in-buffer ()
  "A pop-up menu in a buffer asks with `lispm-ui-choose', its current
choice checked, and gives its action the value chosen; nothing when
none is."
  (lispm-ui-test--with-controls
    (let ((chosen nil) (asked nil) (answer '(blue)))
      (lispm-ui-insert-popup-button "Red" '((red . "Red") (blue . "Blue"))
                                    (lambda (value) (push value chosen))
                                    :current 'red :help "Colour")
      (goto-char (point-min))
      (should (eq (get-text-property (point) 'keymap) lispm-ui-push-button-map))
      (let ((lispm-ui-choose-function (lambda (&rest args) (push args asked) answer)))
        (push-button)
        (setq answer nil)
        (push-button))
      (should (equal chosen '(blue)))
      (should (equal (car asked) '(((red . "Red") (blue . "Blue")) red nil "Colour"))))))

(ert-deftest lispm-ui-test-controls-as-text ()
  "On a text terminal controls are text: a button between brackets, the
default one in the highlight; radio buttons and check boxes in
brackets; a group box's edges in lines; the focused control ringed in
the focus colour."
  (lispm-ui-test--with-controls
    (cl-letf (((symbol-function 'lispm-ui-tty-p) (lambda (&rest _) t)))
      (let ((button (get-text-property 0 'display (lispm-ui-push-button "OK" :default t))))
        (should (equal button "[ OK ]"))
        (should (equal (plist-get (get-text-property 1 'face button) :background)
                       (lispm-ui-color :highlight))))
      (should (equal (get-text-property 0 'display (lispm-ui-radio t)) "(•)"))
      (should (equal (get-text-property 0 'display (lispm-ui-check-box nil)) "[ ]"))
      (should (equal (get-text-property 0 'display (lispm-ui-list-check-box 'mixed)) "[-]"))
      (should (equal (get-text-property 0 'display (lispm-ui-popup-button "Red")) "[ Red ▾ ]"))
      (should (equal (lispm-ui-group-edge 12 "Box") "┌─ Box ────┐"))
      (should (equal (lispm-ui-group-edge 6 nil t) "└────┘"))
      (should (equal (lispm-ui-group-side 20) "│"))
      (let ((focused (lispm-ui-text-control "[x]" t)))
        (should (equal (plist-get (get-text-property 0 'face focused) :background)
                       (lispm-ui-color :focus)))))))

;;;; lispm-ui-list

(ert-deftest lispm-ui-test-list-header ()
  "A list's headings are as wide as their columns and the sort-direction
button; a disclosure triangle is an image, or a character on a text
terminal, pointing down when open."
  (lispm-ui-test--with-controls
    (let ((header (lispm-ui-draw-list-header lispm-ui-current-palette
                                             '(("Name" 150 nil 28) ("Date Modified" 96 t 6)))))
      (should (equal (dom-attr header 'width) 262))
      (should (equal (dom-attr header 'height) lispm-ui-list-header-height)))
    (should (eq (car (get-text-property 0 'display (lispm-ui-triangle t))) 'image))
    (cl-letf (((symbol-function 'lispm-ui-tty-p) (lambda (&rest _) t)))
      (should (equal (lispm-ui-triangle t) "▾"))
      (should (equal (lispm-ui-triangle nil) "▸")))))

;;;; lispm-ui-scroll

(ert-deftest lispm-ui-test-scroll-thumb ()
  "The thumb is as long as the share shown, and as far down as the start;
the arrows take the bar's bottom.  Everything shown, or a track too
short, has no thumb."
  (let ((arrows (1+ (* 2 lispm-ui-scroll-arrow))))
    (should-not (lispm-ui-scroll-thumb 400 1 1000 1 1000))
    (should-not (lispm-ui-scroll-thumb (+ 24 arrows) 1 2 1 100))
    (should (equal (lispm-ui-scroll-thumb (+ 300 arrows) 1 101 1 1001) '(0 . 30)))
    (pcase-let ((`(,top . ,length) (lispm-ui-scroll-thumb (+ 300 arrows) 901 1001 1 1001)))
      (should (= (+ top length) 300)))
    ;; A short thumb is never shorter than an arrow's box.
    (should (= (cdr (lispm-ui-scroll-thumb 400 1 2 1 100000)) 16))
    (let ((layout (lispm-ui-scroll-layout (+ 300 arrows))))
      (should (= (plist-get layout :track-length) 300))
      (should (= (plist-get layout :up) 300))
      (should (= (plist-get layout :down) (+ 300 lispm-ui-scroll-arrow))))))

(ert-deftest lispm-ui-test-scroll-bar-drawn-by-look ()
  "A scroll bar is drawn in the look in use: its own method, where it has
one, before the classic look's."
  (let ((palette (lispm-ui-derive-palette "#DDDDDD" "#000000" "#777777" "#6363CE"))
        (lispm-ui-looks nil))
    (lispm-ui-look-define 'test-flat :family 'test-flat)
    (cl-defmethod lispm-ui-scroll-layout (height &context (lispm-ui-look (lispm-ui-look-family test-flat)))
      (plist-put (cl-call-next-method) :track 16))
    (let ((lispm-ui-look 'test-flat))
      (should (= (plist-get (lispm-ui-scroll-layout 100) :track) 16)))
    (should (= (plist-get (lispm-ui-scroll-layout 100) :track) 0))
    (dolist (thumb '((40 . 30) nil))
      (dolist (active '(t nil))
        (let ((bar (lispm-ui-draw-scroll-bar palette 300 thumb active)))
          (should (equal (dom-attr bar 'width) lispm-ui-scroll-width))
          (should (equal (dom-attr bar 'height) 300)))))))

;;;; lispm-ui-window

(ert-deftest lispm-ui-test-title-layout ()
  "A short title sits in the middle of its bar, ridges either side, the
boxes spaced as Mac OS 9 spaced them; a title too wide is narrowed,
keeping ridges either side; a bar lacking boxes gives their room to the
title; margins stand the boxes nearest the ends that far in."
  (let* ((s lispm-ui-title-box-size)
         (margin lispm-ui-title-box-margin)
         (layout (lispm-ui-title-layout 600 100)))
    (should (= (plist-get layout :title) 100))
    (should (= (plist-get layout :label-x) 250))
    (should (= (plist-get layout :start) (+ 3 s)))
    (should (= (car (nth 0 (plist-get layout :ridges))) (+ 3 s 4)))
    (should (= (plist-get layout :end) (- 600 3 s 3 s)))
    (pcase-let* ((long (lispm-ui-title-layout 300 1000))
                 (`((,a . ,b) (,c . ,d)) (plist-get long :ridges)))
      (should (< (plist-get long :title) 300))
      (should (>= (- b a) 12))
      (should (>= (- d c) 12))
      (should (> (plist-get (lispm-ui-title-layout 300 1000 '(on nil nil)) :title)
                 (plist-get long :title))))
    (let ((dialog (lispm-ui-title-layout 448 100 '(nil nil nil))))
      (should (= (plist-get dialog :start) 0))
      (should (= (plist-get dialog :end) (- 448 margin)))
      (should (= (plist-get dialog :label-x) 174)))
    (let ((wide (lispm-ui-title-layout 600 100 nil '(12 . 15))))
      (should (equal (plist-get wide :margins) '(12 . 15)))
      (should (= (plist-get wide :start) (+ 12 s))))))

(ert-deftest lispm-ui-test-title-part ()
  "A stretch of title bar is as tall as a title bar; active, it is lit
along its top over the content's black line, and its label is drawn in
the titles' face; inactive, its label is greyed."
  (let* ((palette (lispm-ui-derive-palette "#DDDDDD" "#000000" "#777777" "#6363CE"))
         (active (lispm-ui-draw-title-part palette 200 :active t :ridges '((10 . 60)) :label "Choose"
                                           :label-x 80 :lead t :trail t))
         (inactive (lispm-ui-draw-title-part palette 200 :label "Choose" :label-x 80)))
    (should (equal (dom-attr active 'height) lispm-ui-title-bar-height))
    (should (equal (dom-attr (car (dom-by-tag active 'text)) 'fill) (plist-get palette :text)))
    (should (equal (dom-attr (car (dom-by-tag inactive 'text)) 'fill) (lispm-ui-grey palette #x77)))
    (should (< (length (dom-by-tag inactive 'rect)) (length (dom-by-tag active 'rect))))))

;;;; lispm-ui-modal

(ert-deftest lispm-ui-test-modal-key-roles ()
  "Every role of a key in a modal panel has a key the HHKB types: C-i for
Tab, C-m for Return, C-[ and C-g for Escape, DEL for Back, and C-f, C-b,
C-n and C-p for the arrows, which stay as well."
  (should (eq (lispm-ui-modal-role ?\C-i) 'next))
  (should (eq (lispm-ui-modal-role 'backtab) 'previous))
  (should (eq (lispm-ui-modal-role ?\C-m) 'confirm))
  (should (eq (lispm-ui-modal-role ?\C-\[) 'cancel))
  (should (eq (lispm-ui-modal-role ?\C-g) 'cancel))
  (should (eq (lispm-ui-modal-role ?\M-.) 'cancel))
  (should (eq (lispm-ui-modal-role ?\d) 'back))
  (should (eq (lispm-ui-modal-role ?\s) 'press))
  (should (equal (mapcar #'lispm-ui-modal-step '(?\C-f ?\C-n tab right ?\C-b ?\C-p backtab up ?a))
                 '(1 1 1 1 -1 -1 -1 -1 nil)))
  (should-not (lispm-ui-modal-role ?a)))

(ert-deftest lispm-ui-test-modal-cycle ()
  "A focus ring wraps around; from no control it goes to the first or the
last, and it passes over the controls it is told to."
  (should (= (lispm-ui-modal-cycle 2 1 3) 0))
  (should (= (lispm-ui-modal-cycle 0 -1 3) 2))
  (should (= (lispm-ui-modal-cycle nil 1 3) 0))
  (should (= (lispm-ui-modal-cycle nil -1 3) 2))
  (should (= (lispm-ui-modal-cycle 1 4 6) 5))
  (should (= (lispm-ui-modal-cycle 0 1 4 (lambda (index) (/= index 1))) 2))
  (should-not (lispm-ui-modal-cycle 0 1 4 #'ignore))
  (should-not (lispm-ui-modal-cycle nil 1 0)))

(ert-deftest lispm-ui-test-modal-hit ()
  "A click is on a control when it lies in its rectangle, the left and top
edges in, the right and bottom out."
  (let ((layout '((a 10 10 20 10) (b 30 10 20 10) (c 0 0 0 0))))
    (should (eq (lispm-ui-modal-hit layout 10 10) 'a))
    (should (eq (lispm-ui-modal-hit layout 29 19) 'a))
    (should (eq (lispm-ui-modal-hit layout 30 10) 'b))
    (should-not (lispm-ui-modal-hit layout 50 10))
    (should-not (lispm-ui-modal-hit layout 20 20))
    (should-not (lispm-ui-modal-hit layout 0 0))
    (should-not (lispm-ui-modal-inside-p nil 0 0))))

(ert-deftest lispm-ui-test-modal-flash ()
  "A control chosen blinks for its kind's time: a button eight ticks, a
menu item three, drawn back as it was when asked."
  (let ((log nil))
    (cl-letf (((symbol-function 'sit-for) (lambda (seconds) (push (list 'wait seconds) log)))
              ((symbol-function 'redisplay) #'ignore))
      (lispm-ui-modal-flash 'button (lambda () (push 'pressed log)))
      (should (equal (reverse log) '(pressed (wait 0.133))))
      (setq log nil)
      (lispm-ui-modal-flash 'menu-item (lambda () (push 'off log)) (lambda () (push 'on log)))
      (should (equal (reverse log) '(off (wait 0.05) on))))))

(ert-deftest lispm-ui-test-modal-hold ()
  "A control held with the mouse is drawn pushed in only while the mouse
is over it, and chosen only when the button rises over it; Escape gives
up."
  (let ((draws nil))
    (cl-flet ((hold (events)
                (setq draws nil)
                (let ((unread-command-events events))
                  (lispm-ui-modal-hold (lambda (event) (eq (cadr event) 'on))
                                       (lambda (in) (push in draws))))))
      (should (hold '((mouse-movement off) (mouse-movement on) (mouse-1 on))))
      (should (equal (reverse draws) '(t nil t nil)))
      (should-not (hold '((mouse-movement off) (mouse-1 off))))
      (should (equal (reverse draws) '(t nil)))
      (should-not (hold '(escape)))
      (should (equal (reverse draws) '(t nil))))))

(ert-deftest lispm-ui-test-modal-moved-p ()
  "A move of the mouse by more than `double-click-fuzz' is a drag; less is
a hand's jitter in a click.  In a text terminal's cells, of which Emacs
reads the fuzz as eighths, a cell is a drag."
  (let ((double-click-fuzz 3))
    (should-not (lispm-ui-modal-moved-p '(3 . -3)))
    (should (lispm-ui-modal-moved-p '(4 . 0)))
    (should (lispm-ui-modal-moved-p '(0 . -4)))
    (should-not (lispm-ui-modal-moved-p nil))
    (should (lispm-ui-modal-moved-p '(1 . 0) t))
    (should (lispm-ui-modal-moved-p '(0 . -1) t))
    (should-not (lispm-ui-modal-moved-p '(0 . 0) t))
    (should-not (lispm-ui-modal-moved-p nil t)))
  (let ((double-click-fuzz 16))
    (should-not (lispm-ui-modal-moved-p '(2 . 0) t))
    (should (lispm-ui-modal-moved-p '(3 . 0) t))))

;;;; lispm-ui-form

(defun lispm-ui-test--form-field (key)
  "Move to the start of the field KEY of the form in this buffer."
  (let ((widget (alist-get key lispm-ui-form--widgets)))
    (goto-char (or (widget-field-start widget) (widget-get widget :from)))))

(defun lispm-ui-test--form-type (key text)
  "Type TEXT into the field KEY of the form in this buffer."
  (lispm-ui-test--form-field key)
  (insert text))

(defun lispm-ui-test--form-press (key &optional nth)
  "Press the NTH (default first) widget of the field KEY, as Return would."
  (let ((widgets (seq-filter (lambda (widget) (eq (widget-get widget :lispm-form-key) key))
                             (lispm-ui-test--form-buttons))))
    (widget-apply-action (nth (or nth 0) widgets))))

(defun lispm-ui-test--form-buttons ()
  "The buttons of the form in this buffer, in order."
  (let (buttons)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when-let* ((widget (widget-at (point))))
          (unless (memq widget buttons) (push widget buttons)))
        (forward-char 1)))
    (nreverse buttons)))

(ert-deftest lispm-ui-test-form ()
  "A form written as data is drawn, filled in, checked and submitted."
  (let* ((submitted nil)
         (glyphs nil)
         (drawn 0)
         (lispm-ui-form-glyph-function (lambda (kind state label) (push (list kind state label) glyphs) nil))
         (lispm-ui-form-drawn-hook (list (lambda () (cl-incf drawn))))
         (buffer (lispm-ui-form
                  "Pet"
                  `((name :type text :label "Name" :required t)
                    (notes :type text-area :label "Notes" :lines 3 :help "Anything.")
                    (kind :type radio :label "Kind" :choices ((cat . "Cat") (dog . "Dog")) :value cat)
                    (barks :type check :text "Barks"
                           :when ,(lambda (values) (eq (alist-get 'kind values) 'dog)))
                    (size :type choice :label "Size" :choices ((small . "Small") (big . "Big")))
                    (age :type text :label "Age"
                         :validate ,(lambda (value) (and value (not (string-match-p "\\`[0-9]+\\'" value))
                                                         "a number"))))
                  :submit "Adopt"
                  :on-submit (lambda (values) (setq submitted values)))))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (should (eq major-mode 'lispm-ui-form-mode))
            (let ((text (buffer-string)))
              (should (string-prefix-p " Pet\n" text))
              (should (string-match-p "^ Name: " text))
              (should (string-match-p "^ Anything\\.$" text))
              (should (string-match-p "^ Kind: (\\*) Cat\n ( ) Dog$" text))
              (should (string-match-p "Choose… ▾" text))
              (should (string-match-p "\\[Cancel\\] \\[Adopt\\]" text))
              (should-not (string-match-p "Barks" text)))
            ;; Those who dress it are asked for pictures, and told when it is drawn.
            (should (member '(radio on nil) glyphs))
            (should (member '(button default "Adopt") glyphs))
            (should (member '(popup nil "Choose…") glyphs))
            ;; Each control says what it is, to be drawn otherwise, as focused.
            (should (equal (delq nil (mapcar (lambda (widget) (widget-get widget :lispm-form-control))
                                             (lispm-ui-test--form-buttons)))
                           '(radio radio (popup nil "Choose…") (button nil "Cancel") (button default "Adopt"))))
            (should (= drawn 1))
            ;; It starts in its first field.
            (should (eq (lispm-ui-form--key-at (point)) 'name))
            ;; A required field left empty is refused, and point goes there.
            (lispm-ui-test--form-field 'notes)
            (should-error (lispm-ui-form-submit) :type 'user-error)
            (should (eq (lispm-ui-form--key-at (point)) 'name))
            (lispm-ui-test--form-type 'name "Rex")
            (lispm-ui-test--form-type 'notes "Good boy.\nVery good.")
            (should (equal (lispm-ui-form-value 'name) "Rex"))
            (should (equal (lispm-ui-form-value 'notes) "Good boy.\nVery good."))
            ;; Choosing shows the fields that depend on it, keeping what was typed.
            (lispm-ui-test--form-press 'kind 1)
            (should (eq (lispm-ui-form-value 'kind) 'dog))
            (should (string-match-p "( ) Cat\n (\\*) Dog" (buffer-string)))
            (should (string-match-p "\\[ \\] Barks" (buffer-string)))
            (should (equal (lispm-ui-form-value 'name) "Rex"))
            (lispm-ui-test--form-press 'barks)
            (should (eq (lispm-ui-form-value 'barks) t))
            ;; Whatever shows the form shows a pop-up menu's choices.
            (let* ((asked nil)
                   (lispm-ui-form-choose-function
                    (lambda (label choices value _event) (setq asked (list label choices value)) '(big))))
              (lispm-ui-test--form-press 'size)
              (should (equal asked '("Size" ((small . "Small") (big . "Big")) nil))))
            (should (eq (lispm-ui-form-value 'size) 'big))
            (should (string-match-p "Big ▾" (buffer-string)))
            ;; Drawn again, as when the look changes, it keeps its values.
            (lispm-ui-form-redraw)
            (should (= drawn 5))
            (should (equal (lispm-ui-form-value 'name) "Rex"))
            (should (string-match-p "Big ▾" (buffer-string)))
            ;; A value its check refuses is refused.
            (lispm-ui-test--form-type 'age "two")
            (should (equal (cadr (should-error (lispm-ui-form-submit) :type 'user-error)) "Age: a number"))
            (lispm-ui-test--form-field 'age)
            (delete-region (point) (+ (point) 3))
            (insert "2")
            (lispm-ui-form-submit))
          (should (equal submitted '((name . "Rex") (notes . "Good boy.\nVery good.") (kind . dog)
                                     (barks . t) (size . big) (age . "2"))))
          (should-not (buffer-live-p buffer)))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest lispm-ui-test-form-buttons ()
  "A row of push buttons does what each says, in the form's buffer, and a
help that is a function says how things stand each time the form is drawn."
  (let* ((pressed nil)
         (stands "Not asked")
         (glyphs nil)
         (submitted nil)
         (lispm-ui-form-glyph-function (lambda (kind state label) (push (list kind state label) glyphs) nil))
         (buffer (lispm-ui-form
                  "Server"
                  `((address :type text :label "Address" :value "http://a:1"
                             :help ,(lambda (values) (format "%s: %s" (alist-get 'address values) stands)))
                    (actions :type buttons
                             :buttons (("Test" . ,(lambda () (push (cons 'test (lispm-ui-form-value 'address)) pressed)))
                                       ("Open" . ,(lambda () (push 'open pressed))))))
                  :on-submit (lambda (values) (setq submitted values)))))
    (unwind-protect
        (progn
          (should (memq 'buttons lispm-ui-form-types))
          (with-current-buffer buffer
            (let ((text (buffer-string)))
              (should (string-match-p "\\[Test\\] \\[Open\\]" text))
              (should (string-match-p "http://a:1: Not asked" text)))
            (should (member '(button nil "Test") glyphs))
            (should (member '(button nil "Open") glyphs))
            ;; Each button is the field's, as the keyboard and a focus ring find it.
            (lispm-ui-test--form-press 'actions 0)
            (lispm-ui-test--form-press 'actions 1)
            (should (equal pressed '(open (test . "http://a:1"))))
            ;; Drawn again, its help says how things stand now.
            (setq stands "Answers")
            (lispm-ui-form-redraw)
            (should (string-match-p "http://a:1: Answers" (buffer-string)))
            (should-not (string-match-p "Not asked" (buffer-string)))
            (lispm-ui-form-submit))
          ;; Buttons have no value of their own.
          (should (equal submitted '((address . "http://a:1")))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest lispm-ui-test-form-layout ()
  "A form is laid out as a dialog: its labels end in one column, its
controls start in the next, its fields reach one right edge; a heading's
fields are framed in a group box whose edges whatever shows the form
gives, a text area is a panel with a side on each of its lines, and the
title goes where the window shows it instead."
  (let* ((parts nil)
         (lispm-ui-form-group-function
          (lambda (part &rest args)
            (push (cons part args) parts)
            (pcase part ('top (format "[%s]" (cadr args))) ('bottom "[end]") ('side "|"))))
         (buffer (with-temp-buffer
                   (lispm-ui-form "Pet" '((who :type heading :label "Pet")
                                       (name :type text :label "Name")
                                       (notes :type text-area :label "Notes" :lines 2)
                                       (kind :type choice :label "Kind" :choices ((cat . "Cat"))))))))
    (unwind-protect
        (with-current-buffer buffer
          (let ((geometry lispm-ui-form--geometry)
                (text (buffer-substring-no-properties (point-min) (point-max))))
            (should (< (plist-get geometry :left) (plist-get geometry :label)
                       (plist-get geometry :control) (plist-get geometry :right)
                       (plist-get geometry :outer)))
            (should (= (plist-get geometry :right)
                       (+ (plist-get geometry :control) (* lispm-ui-form-width (plist-get geometry :em)))))
            ;; The group box, its sides on the rows of its fields.
            (should (string-match-p "^ \\[Pet\\]$" text))
            (should (string-match-p "^ | Name:  .*|$" text))
            (should (string-match-p "^ \\[end\\]$" text))
            (should (assq 'top parts))
            ;; Each label ends where the column does, as its space says.
            (goto-char (point-min))
            (search-forward "Name:")
            (let ((space (get-text-property (- (point) 6) 'display)))
              (should (equal (car space) 'space))
              (should (= (+ (car (plist-get (cdr space) :align-to)) (lispm-ui-form--measure (lispm-ui-form--label-text "Name")))
                         (plist-get geometry :label))))
            ;; A text field reaches the right edge, its padding one blank.
            (let ((field (alist-get 'name lispm-ui-form--widgets)))
              (should (equal (get-text-property (1- (widget-field-end field)) 'display)
                             `(space :align-to (,(plist-get geometry :right)))))
              (should (eq (get-text-property (widget-field-text-end field) 'invisible) t))
              (lispm-ui-test--form-type 'name "Rex")
              (should (equal (get-text-property (1- (widget-field-end field)) 'display)
                             `(space :align-to (,(plist-get geometry :right)))))
              (should-not (get-text-property (- (widget-field-text-end field) 1) 'display))
              (should-not (get-text-property (- (widget-field-text-end field) 1) 'invisible))
              (should (equal (lispm-ui-form-value 'name) "Rex")))
            ;; A text area: its later lines lined up, a side at each line's end.
            (let* ((area (alist-get 'notes lispm-ui-form--widgets))
                   (pads (widget-get area :lispm-form-pads)))
              (should (= (length pads) 2))
              (should (string-match-p "|\\'" (overlay-get (car pads) 'before-string)))
              (should (string-prefix-p " |" (overlay-get (widget-get area :field-overlay) 'line-prefix)))
              ;; Each of its lines starts at its side, the last one too.
              (save-excursion
                (goto-char (overlay-start (widget-get area :field-overlay)))
                (dotimes (_ 2)
                  (should (get-char-property (point) 'line-prefix))
                  (forward-line 1)))
              ;; Typed in, it is shaped again.
              (lispm-ui-test--form-type 'notes "a\nb")
              (should (= (length (widget-get area :lispm-form-pads)) 3)))
            ;; A check box's long text widens it, half as wide again at most.
            (dolist (case `((10 . 0) (,(+ lispm-ui-form-width 4) . 8) (,(* 2 lispm-ui-form-width) . 17)))
              (let ((buffer (lispm-ui-form "Pet" `((long :type check :text ,(make-string (car case) ?x))))))
                (unwind-protect
                    (with-current-buffer buffer
                      (should (= (plist-get lispm-ui-form--geometry :right)
                                 (+ (plist-get lispm-ui-form--geometry :control) lispm-ui-form-width
                                    (min (cdr case) (/ lispm-ui-form-width 2))))))
                  (kill-buffer buffer))))
            ;; Shown where its window is titled with it, it shows no title.
            (setq lispm-ui-form-show-header nil)
            (lispm-ui-form-redraw)
            (should-not (string-match-p "^ Pet\n" (buffer-string)))
            (should-not (string-match-p "TAB goes" (buffer-string)))))
      (kill-buffer buffer))))

;; Keys go through the command loop, which moves point after each
;; command out of what is displayed otherwise, as a field's padding.
(ert-deftest lispm-ui-test-form-typing ()
  "What is typed into a form, key by key, goes where point is: a text
field keeps what is typed together, its blanks too, and point does not
wander into its padding; a text area takes its lines."
  (let ((buffer (lispm-ui-form "Pet" '((name :type text :label "Name")
                                    (motto :type text :label "Motto")
                                    (notes :type text-area :label "Notes" :lines 2)))))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (execute-kbd-macro (kbd "R e x TAB G o o d SPC d o g C-f C-f ! TAB"))
          (execute-kbd-macro (kbd "S i t s . RET S t a y s ."))
          (should (equal (lispm-ui-form-value 'name) "Rex"))
          (should (equal (lispm-ui-form-value 'motto) "Good dog!"))
          (should (equal (lispm-ui-form-value 'notes) "Sits.\nStays."))
          ;; Typed into a text area, its text takes no face of its frame.
          (let ((area (widget-get (alist-get 'notes lispm-ui-form--widgets) :field-overlay)))
            (should-not (text-property-not-all (overlay-start area) (overlay-end area) 'face nil)))
          ;; What is shown of a field ends where its text does.
          (let ((field (alist-get 'motto lispm-ui-form--widgets)))
            (should (eq (get-text-property (widget-field-text-end field) 'invisible) t))
            (should-not (get-text-property (1- (widget-field-text-end field)) 'invisible))))
      (kill-buffer buffer))))

(defun lispm-ui-test--form-shown (key)
  "What the text field KEY of the form in this buffer shows of its text."
  (let ((field (alist-get key lispm-ui-form--widgets))
        (shown ""))
    (dotimes (i (- (widget-field-text-end field) (widget-field-start field)))
      (let ((position (+ (widget-field-start field) i)))
        (unless (get-text-property position 'invisible)
          (setq shown (concat shown (string (char-after position)))))))
    shown))

(ert-deftest lispm-ui-test-form-long-text ()
  "A text field is as wide as the form's fields, whatever is pasted into it:
it keeps to one line, shows what its width holds, scrolled to show
point while point is in it, and its start once point has left it."
  (let ((buffer (lispm-ui-form "Pet" '((name :type text :label "Name")
                                    (motto :type text :label "Motto"))))
        (text "abcdefghij0123456789ABCDEFGHIJ0123456789klmnopqrst")
        (room (1- lispm-ui-form-width)))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (lispm-ui-test--form-field 'name)
          (kill-new (concat text "\n"))
          (execute-kbd-macro (kbd "C-y"))
          ;; The line break pasted is taken out: the field stays one line.
          (should (equal (lispm-ui-form-value 'name) text))
          (let ((field (alist-get 'name lispm-ui-form--widgets)))
            (should-not (string-match-p "\n" (buffer-substring (widget-field-start field)
                                                               (widget-field-end field))))
            ;; Its end is shown, as much as fits, point after it.
            (should (= (point) (widget-field-text-end field)))
            (should (equal (lispm-ui-test--form-shown 'name) (substring text (- room))))
            ;; Back at its start, its start is shown.
            (execute-kbd-macro (kbd "C-a"))
            (should (= (point) (widget-field-start field)))
            (should (equal (lispm-ui-test--form-shown 'name) (substring text 0 room)))
            ;; Moved past what is shown, it scrolls by as much.
            (execute-kbd-macro (make-vector (1+ room) ?\C-f))
            (should (equal (lispm-ui-test--form-shown 'name) (substring text 1 (1+ room))))
            ;; Drawn again, it still shows point.
            (lispm-ui-form-redraw)
            (should (equal (lispm-ui-test--form-shown 'name) (substring text 1 (1+ room)))))
          ;; Left, it shows its start.
          (execute-kbd-macro (kbd "TAB"))
          (should (equal (lispm-ui-test--form-shown 'name) (substring text 0 room)))
          ;; Back at its end, text taken away brings back what was
          ;; scrolled out.
          (lispm-ui-test--form-field 'name)
          (execute-kbd-macro (kbd "C-e"))
          (should (equal (lispm-ui-test--form-shown 'name) (substring text (- room))))
          (execute-kbd-macro (make-vector 30 ?\d))
          (should (equal (lispm-ui-form-value 'name) (substring text 0 20)))
          (should (equal (lispm-ui-test--form-shown 'name) (substring text 0 20))))
      (kill-buffer buffer))))

(ert-deftest lispm-ui-test-form-accessors ()
  "What a form shows is read by its accessors, from its buffer or another."
  (let ((buffer (lispm-ui-form "Pet" '((name :type text :label "Name")))))
    (unwind-protect
        (progn
          (should (equal (lispm-ui-form-title buffer) "Pet"))
          (should (assq 'name (lispm-ui-form-widgets buffer)))
          (should (plist-get (lispm-ui-form-geometry buffer) :right))
          (with-current-buffer buffer
            (should (equal (lispm-ui-form-title) "Pet"))))
      (kill-buffer buffer))))

(defun lispm-ui-test--form-text (key)
  "The text the field KEY of the form in this buffer holds, as it is shown."
  (let ((field (alist-get key lispm-ui-form--widgets)))
    (buffer-substring-no-properties (widget-field-start field) (widget-field-text-end field))))

(ert-deftest lispm-ui-test-form-password ()
  "A password is typed into a field showing stars, never its text, drawn
again too; its value is what was typed, a star or a blank in it kept,
the line break of a paste taken out."
  (let* ((submitted nil)
         (buffer (lispm-ui-form "Account" '((user :type text :label "User")
                                            (password :type password :label "Password"))
                                :values '((password . "old"))
                                :on-submit (lambda (values) (setq submitted values)))))
    (unwind-protect
        (with-current-buffer buffer
          (should (memq 'password lispm-ui-form-types))
          (should (equal (lispm-ui-test--form-text 'password) "***"))
          ;; As wide as a text field, its padding one blank.
          (let ((field (alist-get 'password lispm-ui-form--widgets)))
            (should (equal (get-text-property (1- (widget-field-end field)) 'display)
                           `(space :align-to (,(plist-get lispm-ui-form--geometry :right))))))
          (should (string-match-p "Password: \\*\\*\\*" (buffer-string)))
          (should (equal (lispm-ui-form-value 'password) "old"))
          (let ((field (alist-get 'password lispm-ui-form--widgets)))
            (delete-region (widget-field-start field) (widget-field-text-end field)))
          (lispm-ui-test--form-type 'password " a*b\nc")
          (should (equal (lispm-ui-form-value 'password) " a*bc"))
          (should (equal (lispm-ui-test--form-text 'password) "*****"))
          (lispm-ui-form-redraw)
          (should (equal (lispm-ui-test--form-text 'password) "*****"))
          (should (equal (widget-value (alist-get 'password lispm-ui-form--widgets)) " a*bc"))
          (lispm-ui-test--form-type 'user "me")
          (lispm-ui-form-submit)
          (should (equal submitted '((user . "me") (password . " a*bc")))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest lispm-ui-test-form-on-change ()
  "A field's :on-change is called with its value each time it is changed,
and may change the others: the form is drawn again once a text's change
is done, at once after a choice; `lispm-ui-form-set' from elsewhere draws
it again at once."
  (let* ((changes nil)
         (drawn 0)
         (lispm-ui-form-drawn-hook (list (lambda () (cl-incf drawn))))
         (buffer (lispm-ui-form
                  "Server"
                  `((address :type text :label "Address"
                             :on-change ,(lambda (value)
                                           (push (cons 'address value) changes)
                                           (when (equal value "a@b")
                                             (lispm-ui-form-set 'host "mail.b"))))
                    (host :type text :label "Server")
                    (security :type choice :label "Security"
                              :choices ((tls . "SSL/TLS") (none . "None")) :value tls
                              :on-change ,(lambda (value)
                                            (push (cons 'security value) changes)
                                            (lispm-ui-form-set 'port (if (eq value 'tls) "993" "143"))))
                    (port :type text :label "Port" :value "993")
                    (keep :type check :text "Keep"
                          :on-change ,(lambda (value) (push (cons 'keep value) changes)))))))
    (unwind-protect
        (with-current-buffer buffer
          (should (= drawn 1))
          (lispm-ui-test--form-type 'address "a@")
          (should (equal changes '((address . "a@"))))
          (lispm-ui-test--form-type 'address "b")
          (should (equal (car changes) '(address . "ba@")))
          (let ((field (alist-get 'address lispm-ui-form--widgets)))
            (delete-region (widget-field-start field) (widget-field-text-end field)))
          (lispm-ui-test--form-type 'address "a@b")
          (should (equal (car changes) '(address . "a@b")))
          ;; Not drawn while the text is being changed: once it is done.
          (should (= drawn 1))
          (should lispm-ui-form--stale)
          (should (equal (lispm-ui-form-value 'host) "mail.b"))
          (should (lispm-ui-test--wait (lambda () (not lispm-ui-form--stale))))
          (should (= drawn 2))
          (should (equal (widget-value (alist-get 'host lispm-ui-form--widgets)) "mail.b"))
          (should (equal (lispm-ui-form-value 'address) "a@b"))
          ;; A choice is drawn again once, with what its :on-change set.
          (let ((lispm-ui-form-choose-function (lambda (&rest _) '(none))))
            (lispm-ui-test--form-press 'security))
          (should (equal (car changes) '(security . none)))
          (should (= drawn 3))
          (should (equal (widget-value (alist-get 'port lispm-ui-form--widgets)) "143"))
          (should-not lispm-ui-form--stale)
          (lispm-ui-test--form-press 'keep)
          (should (equal (car changes) '(keep . t)))
          ;; From elsewhere, as a reply come back, it is drawn again at
          ;; once, once for all it sets.
          (lispm-ui-form-set 'host "imap.b" 'port "995")
          (should (= drawn 5))
          (should (equal (widget-value (alist-get 'host lispm-ui-form--widgets)) "imap.b"))
          (should (equal (widget-value (alist-get 'port lispm-ui-form--widgets)) "995")))
      (kill-buffer buffer))
    (with-temp-buffer
      (should-error (lispm-ui-form-set 'host "x") :type 'user-error))))

(ert-deftest lispm-ui-test-form-keys-in-fields ()
  "The form's keys work in its text fields and text areas too, as C-c C-c
and those of a mode of its own, while what is typed there goes in."
  (let ((buffer (lispm-ui-form "Pet" '((name :type text :label "Name")
                                       (secret :type password :label "Secret")
                                       (notes :type text-area :label "Notes" :lines 2)
                                       (kind :type choice :label "Kind" :choices ((cat . "Cat")))))))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (dolist (key '(name secret notes kind))
            (lispm-ui-test--form-field key)
            (should (eq (key-binding (kbd "C-c C-c")) #'lispm-ui-form-submit))
            (should (eq (key-binding (kbd "C-c C-k")) #'lispm-ui-form-cancel))
            (should (eq (key-binding (kbd "TAB")) #'widget-forward)))
          (lispm-ui-test--form-field 'name)
          (should (eq (key-binding (kbd "C-k")) #'widget-kill-line))
          (should (eq (key-binding "a") #'self-insert-command))
          ;; The last line of a text area too.
          (let ((area (alist-get 'notes lispm-ui-form--widgets)))
            (goto-char (1- (widget-field-end area)))
            (should (eq (key-binding (kbd "C-c C-c")) #'lispm-ui-form-submit))))
      (kill-buffer buffer))))

(defvar-local lispm-ui-test--settings-note nil
  "What the settings of `lispm-ui-test-form-instant' say, set before they are drawn.")

(define-derived-mode lispm-ui-test-settings-mode lispm-ui-form-mode "Settings"
  "Settings, a form of a mode of its own, for `lispm-ui-test-form-instant'.")

(ert-deftest lispm-ui-test-form-instant ()
  "A form applying its changes at once has no Cancel button, and a button
closing it only when it names one; C-c C-c and C-c C-k close it.  It
may be put in a mode of its own, derived from the form's, and set up in
its buffer before it is drawn."
  (let* ((closed nil)
         (fields `((wrap :type check :text "Wrap lines"
                         :help ,(lambda (_) lispm-ui-test--settings-note))))
         (make (lambda (&rest more)
                 (apply #'lispm-ui-form "Settings" fields :instant t
                        :mode #'lispm-ui-test-settings-mode
                        :setup (lambda () (setq lispm-ui-test--settings-note "Long lines wrap."))
                        :on-submit (lambda (_) (push 'submit closed))
                        :on-cancel (lambda () (push 'cancel closed))
                        more))))
    (let ((buffer (funcall make)))
      (with-current-buffer buffer
        (should (eq major-mode 'lispm-ui-test-settings-mode))
        (should (string-match-p "Long lines wrap\\." (buffer-string)))
        (should (string-match-p "changes take effect at once · C-c C-c closes" (buffer-string)))
        (should-not (string-match-p "Cancel\\|OK" (buffer-string)))
        (should-not (seq-filter (lambda (widget) (eq (car-safe (widget-get widget :lispm-form-control)) 'button))
                                (lispm-ui-test--form-buttons)))
        (lispm-ui-form-cancel))
      (should-not (buffer-live-p buffer)))
    (let ((buffer (funcall make :submit "Done")))
      (with-current-buffer buffer
        (should (equal (delq nil (mapcar (lambda (widget) (widget-get widget :lispm-form-control))
                                         (lispm-ui-test--form-buttons)))
                       '(check (button default "Done"))))
        (lispm-ui-form-submit))
      (should-not (buffer-live-p buffer)))
    (should (equal closed '(submit cancel))))
  (should-error (lispm-ui-form "Settings" nil :mode #'fundamental-mode)))

(ert-deftest lispm-ui-test-form-rows ()
  "A list of things is drawn a row each: its columns lined up, the last
cut to the room its buttons leave, which stand at the right and do what
each says; a help under a row; what the field says when there are none.
It is asked again each time the form is drawn, and has no value."
  (let* ((pressed nil)
         (rows (list (list :text (list "Home" "me@home.example")
                           :buttons `(("Edit…" . ,(lambda () (push 'edit-home pressed)))
                                      ("Remove" . ,(lambda () (push 'remove-home pressed)))))
                     (list :text (list "Work account" (concat "me@work.example · default " (make-string 80 ?x)))
                           :help (propertize "Not answering" 'face 'error)
                           :buttons `(("Edit…" . ,(lambda () (push 'edit-work pressed)))
                                      ("Remove" . ,(lambda () (push 'remove-work pressed)))))))
         (submitted nil)
         (lispm-ui-form-width 60)
         (buffer (lispm-ui-form "Accounts"
                                `((accounts :type rows :rows ,(lambda (_values) rows) :empty "No account yet")
                                  (name :type text :label "Name")
                                  (more :type rows :label "More" :rows ((:text "Only one")))
                                  (sign-in :type rows :label "Sign in"
                                           :rows ((:buttons (("Sign In…" . ignore))) (:text "Signed in"))))
                                :on-submit (lambda (values) (setq submitted values)))))
    (unwind-protect
        (with-current-buffer buffer
          (should (memq 'rows lispm-ui-form-types))
          (let ((text (buffer-string))
                (geometry lispm-ui-form--geometry))
            (should (string-match-p "Home +me@home\\.example +\\[Edit…\\] \\[Remove\\]$" text))
            (should (string-match-p "Work account +me@work\\.example · default x+… +\\[Edit…\\] \\[Remove\\]$" text))
            ;; The help is in its own face beneath the help's.
            (goto-char (point-min))
            (search-forward "Not answering")
            (should (equal (get-text-property (1- (point)) 'face) '(error lispm-ui-form-help)))
            ;; Without a label, a list starts where the labels do; with
            ;; one, where the controls do; its columns are lined up.
            (cl-flet ((at (text)
                        (goto-char (point-min))
                        (search-forward text)
                        (plist-get (cdr (get-text-property (- (point) (length text) 1) 'display))
                                   :align-to)))
              (should (equal (at "Home") (list (plist-get geometry :start))))
              (should (equal (at "Work account") (list (plist-get geometry :start))))
              (should (equal (at "me@home") (at "me@work")))
              (should (equal (at "Only one") (list (plist-get geometry :control))))
              ;; A row of buttons alone has them where its text would be.
              (should (equal (at "[Sign In…]") (list (plist-get geometry :control))))
              (should (equal (at "Signed in") (list (plist-get geometry :control)))))
            ;; The form widened to hold its rows, half as wide again at most.
            (should (= (plist-get geometry :right)
                       (+ (plist-get geometry :control) (/ (* 3 60 (plist-get geometry :em)) 2))))
            ;; What is cut fits before the buttons.
            (goto-char (point-min))
            (search-forward "x…")
            (should (<= (lispm-ui-form--measure (buffer-substring (line-beginning-position) (point)))
                        (- (plist-get geometry :right)
                           (lispm-ui-form--buttons-width (plist-get (cadr rows) :buttons))
                           (plist-get geometry :em)))))
          ;; It starts at its first button, each a button of the field.
          (lispm-ui-form--first-field)
          (should (eq (lispm-ui-form--key-at (point)) 'accounts))
          (dotimes (n 4) (lispm-ui-test--form-press 'accounts n))
          (should (equal pressed '(remove-work edit-work remove-home edit-home)))
          ;; Asked again when drawn: none left, it says so.
          (setq rows nil)
          (lispm-ui-form-redraw)
          (should (string-match-p "^ No account yet$" (buffer-string)))
          (lispm-ui-form-submit)
          (should (equal submitted '((name)))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest lispm-ui-test-form-status ()
  "A status is said again where it stands, of one line or several, the
rest of the form left as it is: what is being typed, where point is,
its buttons, another status after it; those who dress it are told.
Not shown, it is kept for when it is."
  (let* ((drawn 0)
         (tested 0)
         (lispm-ui-form-drawn-hook (list (lambda () (cl-incf drawn))))
         (lispm-ui-form-group-function
          (lambda (part &rest args) (pcase part ('top (format "[%s]" (cadr args))) ('bottom "[end]") ('side "|"))))
         (buffer (lispm-ui-form
                  "Server"
                  `((address :type text :label "Address")
                    (connection :type heading :label "Connection")
                    (status :type status :label "Status" :value "Not tested yet." :help "Both\nservers.")
                    (also :type status :label "Also" :value "One")
                    (hidden :type status :label "Hidden" :when ,(lambda (_) nil))
                    (test :type buttons :buttons (("Test" . ,(lambda () (cl-incf tested)))))))))
    (unwind-protect
        (with-current-buffer buffer
          (should (memq 'status lispm-ui-form-types))
          (should (string-match-p "^ | Status: Not tested yet\\. *|\n | *Both *|\n | *servers\\. *|\n | Also: One *|$"
                                  (buffer-string)))
          (lispm-ui-test--form-type 'address "imap.exam")
          (let ((field (alist-get 'address lispm-ui-form--widgets))
                (here (point)))
            (lispm-ui-form-set 'status (concat (propertize "●" 'face 'success) " Incoming: logged in\n"
                                               "● Outgoing: refused"))
            (should (= drawn 2))
            ;; Not drawn again: its fields and point are as they were.
            (should (eq (alist-get 'address lispm-ui-form--widgets) field))
            (should (= (point) here))
            (should (string-match-p (concat "^ | Status: ● Incoming: logged in *|\n | *● Outgoing: refused *|\n"
                                            " | *Both *|\n | *servers\\. *|\n | Also: One *|$")
                                    (buffer-string)))
            (goto-char (point-min))
            (search-forward "●")
            (should (eq (get-text-property (1- (point)) 'face) 'success))
            (goto-char here)
            (insert "ple.com")
            (should (equal (lispm-ui-form-value 'address) "imap.example.com"))
            (lispm-ui-form-set 'also "Two")
            (lispm-ui-form-set 'status "Logging in…")
            (should (string-match-p "^ | Status: Logging in… *|\n | *Both *|\n | *servers\\. *|\n | Also: Two *|$"
                                    (buffer-string)))
            (should (equal (lispm-ui-form-value 'address) "imap.example.com"))
            (lispm-ui-test--form-press 'test)
            (should (= tested 1))
            ;; Not shown, it is kept.
            (lispm-ui-form-set 'hidden "Later")
            (should (equal (lispm-ui-form-value 'hidden) "Later"))
            (should-not (string-match-p "Later" (buffer-string)))
            ;; A status is said, not given.
            (should (equal (lispm-ui-form-values) '((address . "imap.example.com"))))
            ;; Wider than the form, it widens the form, drawn anew.
            (let ((right (plist-get lispm-ui-form--geometry :right)))
              (lispm-ui-form-set 'status (make-string (+ lispm-ui-form-width 10) ?x))
              (should (> (plist-get lispm-ui-form--geometry :right) right))
              (should (string-match-p "Status: x+ *|$" (buffer-string)))
              (should (equal (lispm-ui-form-value 'address) "imap.example.com")))))
      (kill-buffer buffer))))

(defconst lispm-ui-test--classic '("#DDDDDD" "#000000" "#777777" "#6363CE")
  "The colours a classic theme gives a palette.")

(defun lispm-ui-test--images-shown ()
  "The images the current buffer displays, in order."
  (let ((images nil))
    (dotimes (index (- (point-max) (point-min)))
      (let ((display (get-text-property (+ (point-min) index) 'display)))
        (when (and (eq (car-safe display) 'image) (not (eq display (car images))))
          (push display images))))
    (nreverse images)))

(ert-deftest lispm-ui-test-form-undressed ()
  "Until LispM UI has a palette, a form is drawn as text, as it was: no
pictures, no group boxes, its faces its own, nothing drawn focused."
  (let* ((lispm-ui-current-palette nil)
         (buffer (cl-letf (((symbol-function 'widget-image-find)
                            (lambda (image) (and (eq (car-safe image) 'image) image))))
                   (lispm-ui-form "Pet" '((looks :type heading :label "Looks")
                                          (name :type text :label "Name")
                                          (kind :type radio :label "Kind" :choices ((cat . "Cat")))
                                          (avatar :type picture :label "Picture"))))))
    (unwind-protect
        (with-current-buffer buffer
          (should-not (lispm-ui-form-glyph 'button nil "OK"))
          (should-not (lispm-ui-form-group 'side))
          (should-not (lispm-ui-test--images-shown))
          (goto-char (point-min))
          (search-forward "Looks")
          (should (eq (get-text-property (1- (point)) 'face) 'lispm-ui-form-title))
          (should-not face-remapping-alist)
          (should-not (memq #'lispm-ui-form--follow post-command-hook))
          (should (eq (overlay-get (widget-get (alist-get 'name lispm-ui-form--widgets) :field-overlay) 'face)
                      'widget-field)))
      (kill-buffer buffer))))

(ert-deftest lispm-ui-test-form-dressed ()
  "With a palette, a form is drawn in LispM UI's controls and group boxes,
on the chrome's ground, what the keyboard is on drawn focused as it
moves; a check box's or a radio button's picture leaves its centring to
Emacs's widgets.  On a text terminal its controls are text, its group
boxes lines, and its faces its own."
  (let ((lispm-ui-current-palette (apply #'lispm-ui-derive-palette lispm-ui-test--classic)))
    (let ((check (lispm-ui-form-glyph 'check 'on nil))
          (button (lispm-ui-form-glyph 'button 'default "OK")))
      (should (eq (car check) 'image))
      (should-not (plist-member (cdr check) :ascent))
      (should (eq (plist-get (cdr button) :ascent) 'center))
      (should (eq (car (lispm-ui-form-glyph 'picture nil nil)) 'image)))
    (cl-letf (((symbol-function 'widget-image-find)
               (lambda (image) (and (eq (car-safe image) 'image) image))))
      (let ((buffer (lispm-ui-form "Pet" '((looks :type heading :label "Looks")
                                           (name :type text :label "Name")
                                           (cat :type check :label "Cat")))))
        (unwind-protect
            (with-current-buffer buffer
              (should (equal (alist-get 'default face-remapping-alist)
                             `((lispm-ui-list (:background ,(lispm-ui-color :chrome))) default)))
              (should (memq #'lispm-ui-form--follow post-command-hook))
              ;; Its group box, its push buttons and its check box are drawn.
              (should (>= (length (lispm-ui-test--images-shown)) 5))
              ;; It starts in its first field, ringed in the focus colour
              ;; as the command showing it ends.
              (let ((name (widget-get (alist-get 'name lispm-ui-form--widgets) :field-overlay))
                    (cat (alist-get 'cat lispm-ui-form--widgets)))
                (run-hooks 'post-command-hook)
                (should (eq (overlay-get name 'face) 'lispm-ui-field-focused))
                ;; Gone on to the check box, it rings that instead.
                (widget-forward 1)
                (run-hooks 'post-command-hook)
                (should (eq (overlay-get name 'face) 'widget-field))
                (should (eq (widget-at (point)) cat))
                (should (eq (get-text-property (point) 'display) (lispm-ui-form--image 'check 'off nil t)))
                ;; Gone on again, it is drawn plain.
                (widget-forward 1)
                (run-hooks 'post-command-hook)
                (should (eq (get-text-property (widget-get cat :from) 'display)
                            (lispm-ui-form--image 'check 'off nil)))))
          (kill-buffer buffer))))
    (cl-letf (((symbol-function 'lispm-ui-tty-p) (lambda (&optional _) t)))
      (should-not (lispm-ui-form-glyph 'button nil "OK"))
      (should (equal (lispm-ui-form-group 'side) "│"))
      (should (equal (lispm-ui-form-group 'top 20 "Looks") "┌─ Looks ──────────┐"))
      (with-temp-buffer
        (lispm-ui-form--faces)
        (should-not face-remapping-alist)))))

(ert-deftest lispm-ui-test-form-dressed-elsewhere ()
  "A form an application draws with Emacs's widgets itself is dressed as
LispM UI's are once it calls `lispm-ui-form-dress', its controls found
by the properties `lispm-ui-form-control-properties' names, its text
areas white panels."
  (let ((lispm-ui-current-palette (apply #'lispm-ui-derive-palette lispm-ui-test--classic))
        (lispm-ui-form-control-properties '(:lispm-form-control :pet-control)))
    (cl-letf (((symbol-function 'widget-image-find)
               (lambda (image) (and (eq (car-safe image) 'image) image))))
      (with-temp-buffer
        (widget-insert "Notes:\n")
        (let ((notes (widget-create 'text :format "%v" :value "Sits.\n"))
              (check (progn (widget-insert "\n")
                            (widget-create 'toggle :format "%[%v%]" :on "[X]" :off "[ ]"
                                           :on-glyph (lispm-ui-form-glyph 'check 'on nil)
                                           :off-glyph (lispm-ui-form-glyph 'check 'off nil)
                                           :pet-control 'check :value t))))
          (widget-insert "\n")
          (widget-setup)
          (lispm-ui-form-dress)
          (should (assq 'default face-remapping-alist))
          (should (eq (overlay-get (widget-get notes :field-overlay) 'face) 'lispm-ui-form-text-area))
          (goto-char (widget-get check :from))
          (run-hooks 'post-command-hook)
          (should (eq (get-text-property (point) 'display) (lispm-ui-form--image 'check 'on nil t))))))))

(ert-deftest lispm-ui-test-form-redrawn ()
  "Every form is drawn again when the palette changes, once what shows
LispM UI has set its faces in it, its faces remapped to the new colours,
and when the language changes."
  (should (memq #'lispm-ui-form-redraw-all lispm-ui-language-changed-hook))
  (let* ((buffer (lispm-ui-form "Pet" '((name :type text :label "Name"))))
         (hook (default-toplevel-value 'lispm-ui-palette-changed-hook)))
    (unwind-protect
        (lispm-ui-test--with-theme '("#282a36" "#f8f8f2" "#6272a4" "#bd93f9")
          (let ((order nil))
            (setq lispm-ui-palette-changed-hook hook)
            (add-hook 'lispm-ui-palette-changed-hook (lambda () (push 'faces order)))
            (let ((lispm-ui-form-drawn-hook (cons (lambda () (push 'drawn order)) lispm-ui-form-drawn-hook)))
              (lispm-ui-palette-update))
            (should (equal (nreverse order) '(faces drawn)))
            (with-current-buffer buffer
              (should (equal (alist-get 'default face-remapping-alist)
                             '((lispm-ui-list (:background "#1e2028")) default))))))
      (kill-buffer buffer))))

(ert-deftest lispm-ui-test-form-former-names ()
  "A program written for LispM's forms before they moved here works by
their former names: the functions, the variables that dress them, its
buffer's own, the faces, and the mode, as `derived-mode-p' asks."
  (require 'lispm-form)
  (let* ((drawn 0)
         (lispm-form-drawn-hook (list (lambda () (setq drawn (1+ drawn)))))
         (buffer (lispm-form "Pet" '((name :type text :label "Name" :value "Rex")))))
    (unwind-protect
        (with-current-buffer buffer
          (should (eq major-mode 'lispm-ui-form-mode))
          (should (derived-mode-p 'lispm-form-mode))
          (should (= drawn 1))
          (should (equal lispm-form--title "Pet"))
          (should (equal (lispm-form-value 'name) "Rex"))
          (should (assq 'name lispm-form--widgets))
          (should (eq (widget-get (alist-get 'name lispm-form--widgets) :lispm-form-key) 'name))
          (should (equal (lispm-form-values) '((name . "Rex"))))
          (should (memq 'buttons lispm-form-types))
          (should (eq (get 'lispm-form-help 'face-alias) 'lispm-ui-form-help))
          (lispm-form-redraw)
          (should (= drawn 2)))
      (kill-buffer buffer))))

;;;; lispm-ui-ask

(ert-deftest lispm-ui-test-ask-in-minibuffer ()
  "Without an interface drawing them, an alert is a letter a button in the
minibuffer, quitting choosing Cancel; a question to agree to, `yes-or-no-p';
choices, completion; a file, `read-file-name', never in a dialog box."
  (let ((buttons '(("Don’t Save" discard nil) ("Cancel" cancel cancel) ("Save" save default)))
        (lispm-ui-alert-function nil)
        (lispm-ui-choose-function nil)
        (lispm-ui-choose-file-function nil)
        (asked nil))
    (should (equal (lispm-ui--alert-keys buttons) '((?d "Don’t Save" discard) (?c "Cancel" cancel)
                                                    (?s "Save" save))))
    (cl-letf (((symbol-function 'read-multiple-choice) (lambda (&rest _) '(?s "Save"))))
      (should (eq (lispm-ui-alert 'caution "Save?" buttons) 'save)))
    (cl-letf (((symbol-function 'read-multiple-choice) (lambda (&rest _) (signal 'quit nil))))
      (should (eq (lispm-ui-alert 'caution "Save?" buttons) 'cancel)))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (prompt) (push (list prompt use-dialog-box) asked) t)))
      (should (lispm-ui-confirm "Delete Ada?" "Her memory goes too." "Delete"))
      (should (equal asked '(("Delete Ada? Her memory goes too. " nil)))))
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "Big")))
      (should (equal (lispm-ui-choose '((small . "Small") (big . "Big")) 'small) '(big))))
    (cl-letf (((symbol-function 'read-file-name)
               (lambda (prompt directory &rest _) (push (list prompt directory use-dialog-box) asked)
                 (expand-file-name "lispm-ui.el" lispm-ui-directory))))
      (setq asked nil)
      (should (equal (lispm-ui-choose-file :directory lispm-ui-directory)
                     (expand-file-name "lispm-ui.el" lispm-ui-directory)))
      (should (equal asked `(("Choose a File: " ,lispm-ui-directory nil)))))))

(ert-deftest lispm-ui-test-ask-drawn ()
  "An interface drawing what is asked is called with what was asked, and
its answer is the answer; a question to agree to is an alert, Cancel
beside the default button its ACTION names."
  (let* ((called nil)
         (lispm-ui-alert-function
          (lambda (&rest args)
            (push (cons 'alert args) called)
            (nth 1 (seq-find (lambda (button) (eq (nth 2 button) 'default)) (nth 2 args)))))
         (lispm-ui-choose-function (lambda (&rest args) (push (cons 'choose args) called) '(big)))
         (lispm-ui-choose-file-function (lambda (&rest args) (push (cons 'file args) called) "/tmp/ada.png")))
    (should (eq (lispm-ui-alert 'note "Done." '(("OK" ok default))) 'ok))
    (should (lispm-ui-confirm "Delete Ada?" nil "Delete"))
    (should (equal (car called) '(alert caution "Delete Ada?" (("Cancel" cancel cancel) ("Delete" agree default))
                                        nil)))
    (should (equal (lispm-ui-choose '((big . "Big")) nil 1 "Size") '(big)))
    (should (equal (car called) '(choose ((big . "Big")) nil 1 "Size")))
    (should (equal (lispm-ui-choose-file :title "Choose a Picture" :directory "/tmp/" :preview t) "/tmp/ada.png"))
    (should (equal (car called) '(file :title "Choose a Picture" :file nil :directory "/tmp/"
                                       :predicate nil :preview t)))))

(ert-deftest lispm-ui-test-confirm-escape ()
  "Escape never agrees to a question: its Cancel is worth something, as an
alert taking a Cancel worth nil for none is dismissed by its default."
  (let* ((lispm-ui-alert-function
          (lambda (_icon _message buttons _detail)
            ;; Escape, as an alert with no Cancel is dismissed by its default.
            (let ((value (lambda (role) (nth 1 (seq-find (lambda (button) (eq (nth 2 button) role))
                                                         buttons)))))
              (or (funcall value 'cancel) (funcall value 'default))))))
    (should-not (lispm-ui-confirm "Throw this note away?" "Its text will be lost." "Throw Away"))
    (should-not (lispm-ui-confirm "Delete Ada?"))))

;;;; lispm-ui-i18n

(defun lispm-ui-test--messages ()
  "Return the messages LispM UI's code gives to translate; see
`lispm-kit-i18n-messages'."
  (lispm-kit-i18n-messages (lispm-ui-test--source-files)
                           :tr 'lispm-ui-tr :tr-n 'lispm-ui-tr-n :tr-in 'lispm-ui-tr-in
                           :tr-noop 'lispm-ui-tr-noop :format-time 'lispm-ui-format-time))

(ert-deftest lispm-ui-test-i18n-translates-every-message ()
  "Every language but English translates every message the code gives,
a plural in as many forms as the language has; no catalog holds a
message the code no longer gives, and each says its language's name."
  (let ((messages (lispm-ui-test--messages)))
    (should (> (hash-table-count messages) 10))
    (should-not (lispm-kit-i18n-problems 'lispm-ui messages '(nameless stale missing)))))

(ert-deftest lispm-ui-test-i18n-keeps-arguments ()
  "A translation takes the arguments its message takes, in any order; a
time format, its own."
  (should-not (lispm-kit-i18n-problems 'lispm-ui (lispm-ui-test--messages) '(arguments))))

(ert-deftest lispm-ui-test-i18n-english-spelling ()
  "The code's English spells as Britain does: American English changes
every message spelled so, and British English every one spelled the
American way; neither keeps the other's spelling."
  (should-not (lispm-kit-i18n-problems 'lispm-ui (lispm-ui-test--messages)
                                       '(spelled-as-written spelling))))

(ert-deftest lispm-ui-test-i18n-translates ()
  "LispM UI's words show in the language it speaks, from its own
catalogs, which LispM Kit reads as the domain `lispm-ui'; a message
missing from them shows as written."
  (let ((lispm-ui-language 'fr))
    (should (eq (lispm-ui-current-language) 'fr))
    (should (equal (lispm-ui-tr "Cancel") "Annuler"))
    (should (equal (lispm-ui-tr "%s: fill this in" "Nom") "Nom : à remplir"))
    (should (equal (lispm-ui-tr (lispm-ui-tr-noop "Cancel")) "Annuler"))
    (should (equal (lispm-ui-tr "Not in the catalog") "Not in the catalog"))
    (should (equal (lispm-ui-tr-n "%d item" "%d items" 2) "2 items"))
    (should (equal (lispm-ui-language-name 'fr) "Français")))
  (should (equal (lispm-ui-i18n-catalog-file 'de) (expand-file-name "i18n/de.eld" lispm-ui-directory))))

(ert-deftest lispm-ui-test-i18n-catalogs ()
  "Each language's catalog names it in itself."
  (should (equal (mapcar #'lispm-ui-language-name '(de fr ja zh-Hans zh-Hant))
                 '("Deutsch" "Fran\u00e7ais" "\u65e5\u672c\u8a9e" "\u7b80\u4f53\u4e2d\u6587"
                   "\u7e41\u9ad4\u4e2d\u6587"))))

(ert-deftest lispm-ui-test-i18n-language ()
  "The language spoken is `lispm-ui-language', else that of the program
showing LispM UI, a language it speaks, else the computer's."
  (cl-letf (((symbol-function 'lispm-kit-system-language) (lambda () 'zh-Hant)))
    (let ((lispm-ui-language nil)
          (lispm-ui-language-functions nil))
      (should (eq (lispm-ui-current-language) 'zh-Hant))
      (let ((lispm-ui-language-functions (list #'ignore (lambda () 'de))))
        (should (eq (lispm-ui-current-language) 'de))
        (let ((lispm-ui-language 'fr))
          (should (eq (lispm-ui-current-language) 'fr))))
      ;; A program's language LispM UI does not speak is not followed.
      (let ((lispm-ui-language-functions (list (lambda () 'en))))
        (should (eq (lispm-ui-current-language) 'zh-Hant))))))

(ert-deftest lispm-ui-test-i18n-language-changed ()
  "Setting `lispm-ui-language' runs `lispm-ui-language-changed-hook' once
the command ends, when the language spoken changes; so does a program
saying its language changed, unless `lispm-ui-language' chooses one."
  (let ((lispm-ui-language 'en-US)
        (lispm-ui-language-changed-hook nil)
        (drawn 0))
    (cl-letf (((symbol-function 'lispm-kit-system-language) (lambda () 'ja)))
      (add-hook 'lispm-ui-language-changed-hook (lambda () (setq drawn (1+ drawn))))
      (unwind-protect
          (progn
            (setq lispm-ui-language 'de)
            (should (lispm-ui-test--wait (lambda () (= drawn 1)) 1))
            (setq lispm-ui-language 'de)
            (accept-process-output nil 0.05)
            (should (= drawn 1))
            (lispm-ui-language-changed)
            (should (= drawn 1))
            (setq lispm-ui-language nil)
            (should (lispm-ui-test--wait (lambda () (= drawn 2)) 1))
            (lispm-ui-language-changed)
            (should (= drawn 3)))
        (setq lispm-ui-language 'en-US)))))

(defun lispm-ui-test-report-catalogs ()
  "Print, for each language, the messages its catalog lacks and those it
holds no longer.  Run with `make i18n'."
  (lispm-kit-i18n-report 'lispm-ui (lispm-ui-test--messages)))

(provide 'lispm-ui-test)
;;; lispm-ui-test.el ends here
