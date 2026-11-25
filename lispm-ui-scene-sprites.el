;;; lispm-ui-scene-sprites.el --- Pixel portraits, room interiors, and workers -*- lexical-binding: t; -*-

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

;; Low-level SVG sprites for spatially laid-out rooms, furniture, wall
;; art, 16x16 pixel portraits, custom avatar embedding, workstations,
;; and animated worker poses.  Contains zero runtime or session logic.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'dom)
(require 'svg)
(require 'xml)
(require 'lispm-ui-draw)
(require 'lispm-ui-palette)
(require 'lispm-ui-scene-layout)

;;;; Palette & session asset caches

(defvar *lispm-ui-scene-palette* nil
  "Palette dynamically bound during one scene drawing.")

(defvar lispm-ui-scene--avatars (make-hash-table :test #'equal)
  "Local avatar drawings cached until invalidated.")

(defvar lispm-ui-scene--portrait-assignments (make-hash-table :test #'equal)
  "Random portrait indices retained per key for this Emacs session.")

(defvar lispm-ui-scene--portrait-pool nil
  "Remaining indices in the shuffled portrait deck.")

(defvar lispm-ui-scene--room-assignments (make-hash-table :test #'equal)
  "Stable random interior indices retained per room for this session.")

(defvar lispm-ui-scene--room-pool nil
  "Remaining indices in the shuffled interior deck.")

;;;; Catalogs

(defconst lispm-ui-scene-portraits
  [(:id side-part :skin "#eab690" :hair "#5c3c30" :shirt "#3b6e91"
    :pixels ["....OOOOOOOO...." "...OHHhhhhHHO..." "..OHHhhhhhhHHO.." "..OHHHHHHHHHHO.."
             "..OHHHhSSSHHHO.." "..OHHSSSSSSsHO.." "..OSSSSSSSSssO.." ".OSSSSSSSSSssSO."
             ".OSSEWSSSSEWsSO." ".OSSESSSSSESsSO." "..OCSSSSSSSSCO.." "..OSSSSssSSssO.."
             "...OSsssssssO..." "....OOOOOOOO...." ".....OssssO....." "....OAAAAAAO...."])
   (:id teal-bob :skin "#f2c8a4" :hair "#295861" :shirt "#b86452"
    :pixels ["....OOOOOOOO...." "...OHHhhhhHHO..." "..OHHhhhhhhHHO.." ".OHHHHHHHHHHHHO."
             ".OHHHHHHHHHHHHO." ".OHHHhSSSShHHHO." ".OHHSSSSSSSsHHO." ".OHSSSSSSSSssHO."
             ".OHSEWSSSSEWsHO." ".OHSESSSSSESsHO." ".OHCSSSSSSSSCHO." ".OHSSSSssSSssHO."
             "..OHSsssssssHO.." "..OOOOOOOOOOOO.." ".....OssssO....." "....OAAAAAAO...."])
   (:id curls :skin "#9e654d" :hair "#2c2633" :shirt "#c49442"
    :pixels ["...OO.OOOO.OO..." "..OHHOHhhHOHHO.." ".OHhHHhHHhHHhHO." ".OHHHhHHHHhHHHO."
             ".OHhHHHHHHHHhHO." "..OHHHSSSHHHHO.." "..OHSSSSSSSSsO.." ".OSSSSSSSSSssSO."
             ".OSSEWSSSSEWsSO." ".OSSESSSSSESsSO." "..OCSSSSSSSSCO.." "..OSSSSssSSssO.."
             "...OSsssssssO..." "....OOOOOOOO...." ".....OssssO....." "....OAAAAAAO...."])
   (:id ponytail :skin "#f3c2a1" :hair "#c79440" :shirt "#6a74ad"
    :pixels ["....OOOOOOOO...." "...OHHhhhhHHO..." "..OHHhhhhhhHHO.." "..OHHHHHHHHHHO.."
             "..OHHHHHHHHHHOA." "..OHHHSSSSHHHAaO" "..OHSSSSSSssHHOO" ".OSSSSSSSSSssSHO"
             ".OSSEWSSSSEWsSHO" ".OSSESSSSSESsSHO" "..OCSSSSSSSSCOHO" "..OSSSSssSSssOHO"
             "...OSsssssssOOO." "....OOOOOOOO...." ".....OssssO....." "....OAAAAAAO...."])
   (:id auburn-waves :skin "#e9ae89" :hair "#8c4238" :shirt "#43838c"
    :pixels ["....OOOOOOOO...." "...OHHhhhhHHO..." "..OHHhhhhhhHHO.." ".OHHHHHHHHHHHHO."
             ".OHHHHHHHHHHHHO." ".OHHHHSSSSHHHHO." ".OHHHSSSSSSsHHO." ".OHHSSSSSSSssHO."
             ".OHSEWSSSSEWsHO." ".OHSESSSSSESsHO." ".OHCSSSSSSSSCHO." ".OHHSSSssSSsHHO."
             ".OHHSsssssssHHO." "..OOOOOOOOOOOO.." ".OHH.OssssO.HHO." "..OO.OAAAAO.OO.."])
   (:id silver-cut :skin "#bc805d" :hair "#aeb9ca" :shirt "#966684"
    :pixels ["....OOOOOOOO...." "...OHhhhhhhHHO.." "..OHHhhhhhhhHO.." "..OHHHHHHHHHHO.."
             "..OHHHHHHHHHO..." "..OHHHSSSSSsO..." "..OHSSSSSSSssO.." ".OSSSSSSSSSssSO."
             ".OSSEWSSSSEWsSO." ".OSSESSSSSESsSO." "..OCSSSSSSSSCO.." "..OSSSSssSSssO.."
             "...OSsssssssO..." "....OOOOOOOO...." ".....OssssO....." "....OAAAAAAO...."])
   (:id blue-cap :skin "#d99c72" :hair "#42393a" :shirt "#4670a8"
    :pixels ["....OOOOOOOO...." "...OAAAAAAAAO..." "..OAAaaaaaaAAO.." "..OAAaaaaAAAAO.."
             "..OAAAAWWAAAAO.." ".OAAAAAAAAAAAAO." ".OOOOOOOOOOOOOO." ".OSSSSSSSSSssSO."
             ".OSSEWSSSSEWsSO." ".OSSESSSSSESsSO." "..OCSSSSSSSSCO.." "..OSSSSssSSssO.."
             "...OSsssssssO..." "....OOOOOOOO...." ".....OssssO....." "....OAAAAAAO...."])
   (:id round-glasses :skin "#aa7053" :hair "#3d2f35" :shirt "#67885c"
    :pixels ["....OOOOOOOO...." "...OHHhhhhHHO..." "..OHHhhhhhhHHO.." "..OHHHHHHHHHHO.."
             "..OHHHhSSSHHHO.." "..OHHSSSSSSsHO.." "..OSSSSSSSSssO.." ".OSSSSSSSSSssSO."
             ".OOOEEEOOEEEOOO." ".OSOEWEssEWEOSO." "..OOEEEssEEEOO.." "..OSSSSssSSssO.."
             "...OSsssssssO..." "....OOOOOOOO...." ".....OssssO....." "....OAAAAAAO...."])
   (:id violet-buns :skin "#eebaa0" :hair "#6b4b87" :shirt "#c28088"
    :pixels ["..OOO.OOOO.OOO.." ".OHhHOhhhhOHhHO." ".OHHOHHHHHHOHHO." "..OOOHHHHHHOOO.."
             "..OAAHSSSSHHAAO." "..OHHSSSSSSsHHO." "..OHSSSSSSSSssO." ".OSSSSSSSSSssSO."
             ".OSSEWSSSSEWsSO." ".OSSESSSSSESsSO." "..OCSSSSSSSSCO.." "..OSSSSssSSssO.."
             "...OSsssssssO..." "....OOOOOOOO...." ".....OssssO....." "....OAAAAAAO...."])
   (:id top-bun :skin "#855541" :hair "#2c2835" :shirt "#bd8248"
    :pixels [".....OOOOOO....." "....OHHhhHHO...." "....OHHHHHHO...." "...OOAAAAAAOO..."
             "..OHHHHHHHHHHO.." "..OHHHSSSSHHHO.." "..OHSSSSSSSSsO.." ".OSSSSSSSSSssSO."
             ".OSSEWSSSSEWsSO." ".OSSESSSSSESsSO." "..OCSSSSSSSSCO.." "..OSSSSssSSssO.."
             "...OSsssssssO..." "....OOOOOOOO...." ".....OssssO....." "....OAAAAAAO...."])
   (:id ginger-beard :skin "#e8b691" :hair "#a85331" :shirt "#4f7b65"
    :pixels ["....OOOOOOOO...." "...OHHhhhhHHO..." "..OHHhhhhhhHHO.." "..OHHHHHHHHHHO.."
             "..OHHHhSSSHHHO.." "..OHHSSSSSSsHO.." "..OSSSSSSSSssO.." ".OSSSSSSSSSssSO."
             ".OSSEWSSSSEWsSO." ".OSSESSSSSESsSO." "..OHSSSSSSSSHO.." "..OHHhHssHhHHO.."
             "...OHHhHHhHHO..." "....OOHHHHOO...." ".....OOOOOO....." "....OAAAAAAO...."])
   (:id rose-crop :skin "#bf8261" :hair "#ba6b82" :shirt "#546c99"
    :pixels ["....OOOOOOOO...." "...OHHhhhhHHO..." "..OHhhhhhhhHHO.." "..OHhhhHHHHHHO.."
             "..OHHhSSSHHHHO.." "..OHHSSSSSSsHO.." "..OSSSSSSSSssO.." ".OSSSSSSSSSssSO."
             ".OSSEWSSSSEWsSO." ".OSSESSSSSESsSO." "..OCSSSSSSSSCO.." "..OSSSSssSSssO.."
             "...OSsssssssO..." "....OOOOOOOO...." ".....OssssO....." "....OAAAAAAO...."])
   (:id cream-wrap :skin "#9b6149" :hair "#49373e" :shirt "#d2ad75"
    :pixels ["....OOOOOOOO...." "...OAAAAAAAAO..." "..OAAWaaaaaaAO.." ".OAAAWaaaaaaaAO."
             ".OAAAAWaaaaaaAO." ".OAAASSSSSSAAAO." ".OASSSSSSSSSsAO." ".OASSEWSSSSEWAO."
             ".OASSESSSSSESAO." "..OACSSSSSSSCAO." "..OASSSssSSsAAO." "...OASsssssAAO.."
             "...OAAOOOOAAAO.." "....OAAAAAAAO..." ".....OssssO....." "....OAAAAAAO...."])
   (:id headset :skin "#dda783" :hair "#443c52" :shirt "#5b899c"
    :pixels ["....OOOOOOOO...." "...OAAAAAAAAO..." "..OAHHhhhhHHAO.." ".OAOHHHHHHHHOAO."
             ".OAOHHhSSSHHOAO." ".OAOHSSSSSSsOAO." ".OAASSSSSSSSAAO." "OOAASEWSSSSEWAAO"
             "OOAASESSSSSESAAO" ".OAACSSSSSSSCAOO" "..OSSSSssSSsAAO." "...OSsssssAAAO.."
             "....OOOOOOOO...." ".....OssssO....." "....OAAAAAAO...." "....OaaaaaaO...."])
   (:id green-beanie :skin "#e9bc9a" :hair "#5d4739" :shirt "#688661"
    :pixels [".....OOOOOO....." "....OAAAAAAO...." "...OAAaaaaAAO..." "..OAaAaAaAaAAO.."
             "..OAaAaAaAaAAO.." ".OAAAAAAAAAAAAO." ".OAAWaaaaaaaaAO." ".OOOOOOOOOOOOOO."
             ".OSSEWSSSSEWsSO." ".OSSESSSSSESsSO." "..OCSSSSSSSSCO.." "..OSSSSssSSssO.."
             "...OSsssssssO..." "....OOOOOOOO...." ".....OssssO....." "....OAAAAAAO...."])
   (:id white-spikes :skin "#c88e69" :hair "#c4cad6" :shirt "#88667d"
    :pixels ["...OO..OO..OO..." "..OHOOOHOOOHHO.." ".OHhHHhHHhHHhHO." "..OHhhhHHhhhHO.."
             "..OHHHHHHHHHHO.." "..OHHSSSSSSHHO.." "..OHSSSSSSSSsO.." ".OSSSSSSSSSssSO."
             ".OSSEWSSSSEWsSO." ".OSSESSSSSESsSO." "..OCSSSSSSSSCO.." "..OSSSSssSSssO.."
             "...OSsssssssO..." "....OOOOOOOO...." ".....OssssO....." "....OAAAAAAO...."])]
  "Sixteen hand-drawn 16 by 16 pixel portraits.
Dots are transparent; O outlines, H/h hair, S/s skin, E eyes, W glints,
A/a accessories, and C cheeks.")

(defconst lispm-ui-scene-room-styles
  [(:id oak :floor "#c6a479" :grain "#ad875d" :wall "#f0e4ca" :trim "#756856" :pattern planks)
   (:id mint :floor "#d6d5bc" :grain "#b6c0a4" :wall "#e0ead5" :trim "#6e7e69" :pattern tiles)
   (:id rose :floor "#b78d7a" :grain "#967462" :wall "#eedcd6" :trim "#7e635f" :pattern parquet)
   (:id tatami :floor "#d8c58e" :grain "#b2a26f" :wall "#efe8d5" :trim "#707058" :pattern tatami)
   (:id library :floor "#a98e72" :grain "#886f57" :wall "#e6d9bd" :trim "#665c49" :pattern planks)
   (:id apricot :floor "#e0bf95" :grain "#cca57b" :wall "#f1e0c8" :trim "#997760" :pattern parquet)
   (:id sage :floor "#a8ae9b" :grain "#929a86" :wall "#e5e7da" :trim "#647165" :pattern carpet)
   (:id cream :floor "#dad3be" :grain "#beb6a3" :wall "#f3edde" :trim "#867e6c" :pattern tiles)]
  "Eight pixel room interiors independent of the interface accent color.")

;;;; Palette, cache, and shuffled assignment deck

(defun lispm-ui-scene--theme-color (attribute fallback)
  "Read default-face ATTRIBUTE color, else FALLBACK."
  (let ((value (face-attribute 'default attribute nil t)))
    (if (and (stringp value) (color-defined-p value)) value fallback)))

(defun lispm-ui-scene-palette ()
  "Return the active palette or derive one from the current theme."
  (or *lispm-ui-scene-palette*
      lispm-ui-current-palette
      (lispm-ui-derive-palette
       (lispm-ui-scene--theme-color :background "white")
       (lispm-ui-scene--theme-color :foreground "black")
       (lispm-ui-scene--theme-color :foreground "black")
       (let ((accent (face-background 'highlight nil t)))
         (if (and (stringp accent) (color-defined-p accent)) accent "gray")))))

(defun lispm-ui-scene-palette-color (name)
  "Return palette entry NAME from the active scene palette."
  (plist-get (lispm-ui-scene-palette) name))

(defun lispm-ui-scene-clear-avatar-cache ()
  "Forget cached avatar images after a palette or character change."
  (clrhash lispm-ui-scene--avatars))

(defun lispm-ui-scene-reset-assignments (&optional kind)
  "Reset shuffled session assignments for KIND (`portrait', `room', or nil).
When KIND is nil, both portrait and room assignment tables are cleared."
  (when (memq kind '(nil portrait))
    (clrhash lispm-ui-scene--portrait-assignments)
    (setq lispm-ui-scene--portrait-pool nil))
  (when (memq kind '(nil room))
    (clrhash lispm-ui-scene--room-assignments)
    (setq lispm-ui-scene--room-pool nil)))

(defun lispm-ui-scene--deck-assign (key count assignments pool-symbol)
  "Return KEY's stable index in [0, COUNT), drawing from shuffled POOL-SYMBOL."
  (or (gethash key assignments)
      (progn
        (unless (symbol-value pool-symbol)
          (let ((deck (vconcat (number-sequence 0 (1- count)))))
            (cl-loop for i downfrom (1- count) above 0 do
                     (let* ((j (random (1+ i)))
                            (tmp (aref deck i)))
                       (aset deck i (aref deck j))
                       (aset deck j tmp)))
            (set pool-symbol (append deck nil))))
        (let ((index (pop (symbol-value pool-symbol))))
          (puthash (copy-tree key) index assignments)
          index))))

(defun lispm-ui-scene-portrait-index (node)
  "Return NODE's stable shuffled portrait index."
  (lispm-ui-scene--deck-assign
   (plist-get node :key) (length lispm-ui-scene-portraits)
   lispm-ui-scene--portrait-assignments 'lispm-ui-scene--portrait-pool))

(defun lispm-ui-scene-portrait-for-node (node)
  "Return the portrait plist assigned to NODE."
  (aref lispm-ui-scene-portraits (lispm-ui-scene-portrait-index node)))

(defun lispm-ui-scene-room-style (room)
  "Return ROOM's stable shuffled interior style."
  (let* ((key (list (plist-get room :root) (plist-get room :id)))
         (index (lispm-ui-scene--deck-assign
                 key (length lispm-ui-scene-room-styles)
                 lispm-ui-scene--room-assignments 'lispm-ui-scene--room-pool)))
    (aref lispm-ui-scene-room-styles index)))

;;;; SVG primitives

(defun lispm-ui-scene-sanitize-label (value &optional width)
  "Format VALUE as a single safe line, optionally truncated at WIDTH columns."
  (let ((text (replace-regexp-in-string "[[:cntrl:]]" " "
                                        (format "%s" (or value "")))))
    (if width
        (truncate-string-to-width text (max 1 width) nil nil "…")
      text)))

(defun lispm-ui-scene-svg-group (svg class)
  "Append an SVG `<g>' element with CLASS to SVG and return it."
  (let ((group (list 'g (list (cons 'class class)))))
    (dom-append-child svg group)
    group))

(defun lispm-ui-scene-svg-rects (group specs)
  "Draw rectangle SPECS as (X Y W H FILL &optional OPACITY) on GROUP."
  (dolist (spec specs)
    (if (nth 5 spec)
        (svg-rectangle group (nth 0 spec) (nth 1 spec)
                       (nth 2 spec) (nth 3 spec)
                       :fill (nth 4 spec) :opacity (nth 5 spec))
      (svg-rectangle group (nth 0 spec) (nth 1 spec)
                     (nth 2 spec) (nth 3 spec)
                     :fill (nth 4 spec)))))

(defun lispm-ui-scene-svg-text
    (svg text x y size color &optional anchor width weight)
  "Draw TEXT on SVG at X/Y with SIZE, COLOR, and optional ANCHOR, WIDTH, WEIGHT."
  (lispm-ui-svg-label
   svg (lispm-ui-scene-sanitize-label text width) x y color
   (list :family "monospace" :size size :weight (or weight "normal"))
   (or anchor "start")))

;;;; Avatars & pixel portraits

(defun lispm-ui-scene-read-avatar (avatar)
  "Read local AVATAR file as an 18×18 SVG image DOM, or return nil."
  (when (and (not (file-remote-p avatar))
             (file-regular-p avatar)
             (file-readable-p avatar)
             (< (file-attribute-size (file-attributes avatar)) (* 2 1024 1024)))
    (condition-case nil
        (let ((svg (svg-create 18 18))
              (ext (downcase (or (file-name-extension avatar) ""))))
          (cond
           ((equal ext "svg")
            (when-let* ((dom (lispm-ui-svg-dom avatar)))
              (lispm-ui-svg-embed svg dom 0 0 18 18)
              svg))
           ((member ext '("png" "jpg" "jpeg" "gif"))
            (svg-embed svg avatar
                       (if (member ext '("jpg" "jpeg")) "image/jpeg"
                         (concat "image/" ext))
                       nil :x 0 :y 0 :width 18 :height 18)
            svg)))
      ((file-error wrong-type-argument args-out-of-range) nil))))

(defun lispm-ui-scene-draw-avatar (svg avatar x y &optional size)
  "Draw cached local AVATAR on SVG at X/Y, returning non-nil on success.
SIZE defaults to 18 pixels."
  (when (stringp avatar)
    (let ((cached (gethash avatar lispm-ui-scene--avatars 'unread)))
      (when (eq cached 'unread)
        (when (> (hash-table-count lispm-ui-scene--avatars) 128)
          (clrhash lispm-ui-scene--avatars))
        (setq cached (lispm-ui-scene-read-avatar avatar))
        (puthash avatar cached lispm-ui-scene--avatars))
      (when cached
        (lispm-ui-svg-embed svg cached x y (or size 18) (or size 18))
        t))))

(defun lispm-ui-scene-draw-pixel-portrait (svg portrait x y &optional pixel-size)
  "Draw PORTRAIT on SVG at X/Y in a crisp pixel grid with PIXEL-SIZE (default 2)."
  (let* ((group (lispm-ui-scene-svg-group svg "office-pixel-portrait"))
         (size (or pixel-size 2))
         (skin (plist-get portrait :skin))
         (hair (plist-get portrait :hair))
         (shirt (plist-get portrait :shirt))
         (colors (list (cons ?O "#2a2533")
                       (cons ?E "#24202c")
                       (cons ?H hair)
                       (cons ?h (lispm-ui-blend hair "#fff4d8" 0.28))
                       (cons ?S skin)
                       (cons ?s (lispm-ui-blend skin "#6e3b40" 0.24))
                       (cons ?C (lispm-ui-blend skin "#d46a64" 0.36))
                       (cons ?W "#fffaf0")
                       (cons ?A shirt)
                       (cons ?a (lispm-ui-blend shirt "#fff4d8" 0.25)))))
    (dom-set-attribute group 'data-portrait (symbol-name (plist-get portrait :id)))
    (dom-set-attribute group 'shape-rendering "crispEdges")
    (cl-loop for row across (plist-get portrait :pixels) for py from 0 do
             (let ((px 0))
               (while (< px (length row))
                 (let* ((pixel (aref row px))
                        (end (1+ px))
                        (color (cdr (assq pixel colors))))
                   (while (and (< end (length row)) (= pixel (aref row end)))
                     (setq end (1+ end)))
                   (when color
                     (svg-rectangle group (+ x (* px size)) (+ y (* py size))
                                    (* (- end px) size) size :fill color))
                   (setq px end)))))
    group))

;;;; Room interiors, fixtures, and wall decor

(defun lispm-ui-scene--interior-patterns (svg room style)
  "Return `(FLOOR . WALL)' fill colors from STYLE for ROOM on SVG."
  (ignore svg room)
  (cons (plist-get style :floor) (plist-get style :wall)))

(defun lispm-ui-scene--draw-bookshelf (group x y w h trim)
  "Draw a bookcase fixture on GROUP at X, Y with dimensions W, H and TRIM."
  (svg-rectangle group x y w h :fill trim)
  (svg-rectangle group (+ x 2) (+ y 2) (- w 4) (- h 4) :fill "#cbb390")
  (dotimes (shelf 3)
    (let ((sy (+ y 4 (* shelf (/ (- h 8) 3)))))
      (svg-rectangle group (+ x 4) sy (- w 8) 2 :fill trim)
      (dotimes (book (max 0 (floor (/ (- w 10) 6))))
        (svg-rectangle group (+ x 5 (* book 6)) (+ sy 3) 4 6
                       :fill (nth (% (+ shelf book) 4)
                                  '("#7e967b" "#be7f68" "#9a95ae" "#dcc284")))))))

(defun lispm-ui-scene--draw-fixture (svg fixture style)
  "Draw pixel FIXTURE on SVG inside layout bounds using STYLE."
  (let* ((x (plist-get fixture :x))
         (y (plist-get fixture :y))
         (w (plist-get fixture :width))
         (h (plist-get fixture :height))
         (trim (plist-get style :trim))
         (group (lispm-ui-scene-svg-group svg "office-fixture")))
    (dom-set-attribute group 'data-kind (format "%s" (plist-get fixture :kind)))
    (pcase (or (plist-get fixture :kind) (plist-get fixture :type))
      ('plant
       (lispm-ui-scene-svg-rects
        group `((,(+ x (* w 0.22)) ,(+ y (* h 0.85)) ,(* w 0.56) ,(* h 0.14) "#4a4238" 0.2)
                (,(+ x (* w 0.25)) ,(+ y (* h 0.58)) ,(* w 0.5) ,(* h 0.36) "#a86848")
                (,(+ x (* w 0.22)) ,(+ y (* h 0.55)) ,(* w 0.56) ,(* h 0.1) "#c27d5b")
                (,(+ x (* w 0.43)) ,(+ y (* h 0.24)) ,(* w 0.14) ,(* h 0.38) "#596b45")
                (,x ,(+ y (* h 0.16)) ,(* w 0.52) ,(* h 0.26) "#678c56")
                (,(+ x (* w 0.42)) ,y ,(* w 0.56) ,(* h 0.32) "#82a668")
                (,(+ x (* w 0.18)) ,(+ y (* h 0.04)) ,(* w 0.46) ,(* h 0.24) "#97b87a")
                (,(+ x (* w 0.22)) ,(+ y (* h 0.34)) ,(* w 0.62) ,(* h 0.22) "#547547"))))
      ((or 'coffee 'coffee-bar)
       (let* ((cw (min w 92)) (ch (min h 28)) (cx (+ x (/ (- w cw) 2))))
         (lispm-ui-scene-svg-rects
          group `((,cx ,(+ y 6) ,cw ,(- ch 6) ,trim)
                  (,(+ cx 2) ,(+ y 8) ,(- cw 4) ,(- ch 10) "#c29e75")
                  (,cx ,(+ y 4) ,cw 5 "#efe6d4") (,cx ,(+ y 8) ,cw 2 "#cbbfa9")
                  (,(+ cx 5) ,y 18 12 "#494440") (,(+ cx 7) ,(+ y 2) 10 4 "#c8c8b0")
                  (,(+ cx 19) ,(+ y 3) 2 2 "#d95b43") (,(+ cx 9) ,(+ y 8) 6 4 "#f6eedb")
                  (,(+ cx 30) ,(+ y 2) 14 5 "#d7b377")
                  (,(+ cx cw -22) ,(+ y 2) 6 6 "#f4e8d2")
                  (,(+ cx cw -13) ,(+ y 2) 6 6 "#e6d8be")))
         (when (>= h 76)
           (lispm-ui-scene--draw-fixture
            group (list :kind 'sofa :x (+ cx 6) :y (+ y 52)
                        :width (- cw 12) :height 24)
            style))))
      ((or 'sofa 'lounge)
       (lispm-ui-scene-svg-rects
        group `((,x ,(+ y 2) ,w ,(- h 2) "#4a4238" 0.18) (,x ,y ,w ,(- h 2) ,trim)
                (,(+ x 3) ,(+ y 3) ,(- w 6) ,(- h 7) "#8e9e90")
                (,(+ x 5) ,(+ y 8) ,(- w 10) ,(- h 13) "#b7c4b5")
                (,(+ x 5) ,(+ y 8) ,(- w 10) 3 "#cdd8cb")
                (,(+ x (/ w 2)) ,(+ y 6) 2 ,(- h 11) "#7c8d81"))))
      (_ (lispm-ui-scene--draw-bookshelf group x y w h trim)))))

(defun lispm-ui-scene--draw-wall-object (svg kind x y width height style)
  "Hang a pixel KIND on SVG inside X, Y, WIDTH, HEIGHT of wall using STYLE."
  (let ((group (lispm-ui-scene-svg-group svg "office-wall-object"))
        (trim (plist-get style :trim))
        (paper (plist-get style :wall)))
    (dolist (entry `((data-kind . ,(symbol-name kind)) (data-x . ,x) (data-y . ,y)
                     (data-width . ,width) (data-height . ,height)))
      (dom-set-attribute group (car entry) (cdr entry)))
    (dom-set-attribute group 'transform
                       (format "translate(%s %s) scale(%s %s)"
                               x y (/ width 64.0) (/ height 40.0)))
    (pcase kind
      ('window
       (lispm-ui-scene-svg-rects
        group `((3 3 60 37 "#51483e" 0.22) (0 0 62 36 ,trim) (2 2 58 32 "#f5ebd8")
                (5 5 52 25 "#a9cfd6") (5 5 52 7 "#95c2cc") (44 8 6 5 "#fae29c")
                (10 9 14 3 "#f4f7e8") (14 7 7 2 "#f4f7e8") (5 22 52 8 "#86a67e")
                (5 18 16 12 "#9ab892") (42 16 15 14 "#72987b") (30 4 3 28 "#f5ebd8")
                (4 17 54 2 "#f5ebd8") (0 0 5 32 "#c49676") (57 0 5 32 "#c49676")
                (0 34 64 3 "#f0e3c8") (2 37 60 3 ,trim))))
      ('clock
       (lispm-ui-scene-svg-rects
        group `((8 3 48 34 ,trim) (4 7 56 26 ,trim) (12 0 40 40 ,trim)
                (12 6 40 28 "#f6edd5") (8 10 48 20 "#f6edd5") (16 4 32 32 "#f6edd5")
                (30 6 4 3 ,trim) (30 31 4 3 ,trim) (11 18 4 3 ,trim) (49 18 4 3 ,trim)
                (30 11 3 11 "#4a423a") (30 20 13 3 "#4a423a") (30 19 4 4 "#b85d48"))))
      ('shelf
       (lispm-ui-scene-svg-rects
        group `((2 35 60 4 "#544b42" 0.22) (7 31 4 9 ,trim) (52 31 4 9 ,trim)))
       (dotimes (i 7)
         (let ((bh (- 19 (% (* i 3) 7))))
           (svg-rectangle group (+ 4 (* i 5)) (- 30 bh) 4 bh
                          :fill (nth (% i 4) '("#738f76" "#c27d63" "#9389a3" "#d6b86e")))))
       (lispm-ui-scene-svg-rects
        group `((44 21 12 9 "#b87d5d") (48 10 3 14 "#5c754a")
                (39 12 13 5 "#7e9c64") (49 6 12 7 "#6c8c56")
                (0 30 64 4 "#d6b88d") (0 34 64 2 ,trim))))
      ('noticeboard
       (lispm-ui-scene-svg-rects
        group `((3 3 61 37 "#544b42" 0.22) (0 0 61 37 ,trim) (3 3 55 31 "#b99c72")))
       (cl-loop for (nx ny nw nh color) in
                '((6 6 18 21 "#f7ecc4") (28 6 24 12 "#dde6cc") (31 21 20 10 "#ecc3a7"))
                do (lispm-ui-scene-svg-rects
                    group `((,nx ,ny ,nw ,nh ,color)
                            (,(+ nx 3) ,(+ ny 5) ,(- nw 6) 1 "#9e9278")
                            (,(+ nx 3) ,(+ ny 8) ,(- nw 8) 1 "#b5a98e")
                            (,(+ nx (/ nw 2)) ,(1+ ny) 2 2 "#b85648")))))
      (_
       (lispm-ui-scene-svg-rects
        group `((3 3 61 37 "#544b42" 0.22) (0 0 61 37 ,trim) (3 3 55 31 ,paper)
                (6 6 49 25 "#a8bec2") (42 9 7 7 "#eed28a") (6 24 49 7 "#5f7d6e")
                (6 19 20 12 "#769482") (11 15 11 5 "#86a391") (28 21 27 10 "#8ca088")
                (35 17 13 5 "#9cb098")))))
    group))

(defun lispm-ui-scene--draw-wall-decor (svg room style)
  "Place bounded wall decorations in ROOM without consuming floor space."
  (let* ((x (plist-get room :x))
         (y (plist-get room :y))
         (width (plist-get room :width))
         (kind (pcase (plist-get style :id)
                 ((or 'library 'tatami) 'shelf)
                 ((or 'mint 'sage) 'noticeboard)
                 (_ 'painting)))
         (window-width (min 66 (max 32 (* width 0.23))))
         (picture-width (min 60 (max 28 (* width 0.20)))))
    (lispm-ui-scene--draw-wall-object
     svg 'window (+ x 24) (+ y 33) window-width 40 style)
    (lispm-ui-scene--draw-wall-object
     svg kind (- (+ x width) picture-width 24) (+ y 36) picture-width 36 style)
    (when (>= width 250)
      (lispm-ui-scene--draw-wall-object
       svg 'clock (- (+ x (/ width 2)) 13) (+ y 42) 26 26 style))))

(defun lispm-ui-scene-draw-room
    (svg room _selected level scale &optional forced-style)
  "Draw ROOM on SVG using LEVEL, SCALE, and optional FORCED-STYLE."
  (let* ((style (or forced-style (lispm-ui-scene-room-style room)))
         (patterns (lispm-ui-scene--interior-patterns svg room style))
         (group (lispm-ui-scene-svg-group svg "office-room"))
         (x (plist-get room :x))
         (y (plist-get room :y))
         (w (plist-get room :width))
         (h (plist-get room :height))
         (wall-h lispm-ui-scene-wall-height)
         (floor-y (+ y wall-h))
         (trim (plist-get style :trim)))
    (dom-set-attribute group 'data-interior (symbol-name (plist-get style :id)))
    (dom-set-attribute group 'shape-rendering "crispEdges")
    (svg-rectangle group x y w h :fill trim)
    (svg-rectangle group (+ x 12) floor-y (- w 24) (- h wall-h 10)
                   :fill (car patterns) :class "office-floor-surface")
    (svg-rectangle group (+ x 8) (+ y 8) (- w 16) (- wall-h 8)
                   :fill (cdr patterns) :class "office-back-wall")
    (lispm-ui-scene-svg-rects
     group `((,(+ x 8) ,(+ y 8) ,(- w 16) 3 "#fff5df")
             (,(+ x 8) ,(+ y 11) ,(- w 16) 2 ,trim 0.22)
             (,(+ x 8) ,(- floor-y 16) ,(- w 16) 11 ,trim 0.18)
             (,(+ x 8) ,(- floor-y 17) ,(- w 16) 2 "#fff1d6")
             (,(+ x 8) ,(- floor-y 5) ,(- w 16) 5 ,trim)
             (,(+ x 8) ,(- floor-y 5) ,(- w 16) 1 "#eadbc0")
             (,x ,y ,w 6 "#e9dfca") (,x ,y 7 ,h "#e9dfca")
             (,(+ x 7) ,(+ y 6) 5 ,(- h 6) ,trim)
             (,(+ x w -12) ,(+ y 6) 6 ,(- h 6) ,trim)
             (,(+ x w -6) ,y 6 ,h "#625b50")
             (,x ,(+ y h -9) ,w 5 "#e9dfca") (,x ,(+ y h -4) ,w 4 "#625b50")
             (,(+ x 12) ,floor-y ,(- w 24) 5 "#51483e" 0.2)
             (,(+ x 12) ,(+ floor-y 5) ,(- w 24) 4 "#51483e" 0.08)
             (,(+ x 12) ,floor-y 5 ,(- h wall-h 10) "#51483e" 0.13)))
    (when-let* ((door (plist-get room :door-anchor)))
      (if (< (car door) (+ x 20))
          (lispm-ui-scene-svg-rects
           group `((,x ,(- (cdr door) 14) 12 28 ,(plist-get style :floor))
                   (,(+ x 9) ,(- (cdr door) 14) 3 28 "#e7d6b6")
                   (,x ,(- (cdr door) 16) 12 3 ,trim)
                   (,x ,(+ (cdr door) 14) 12 3 ,trim)))
        (svg-rectangle group (- (car door) 16) (+ y h -9) 32 9
                       :fill (plist-get style :floor))))
    (unless (eq level 'map)
      (lispm-ui-scene--draw-wall-decor group room style)
      (dolist (fixture (plist-get room :fixtures))
        (lispm-ui-scene--draw-fixture group fixture style)))
    (let* ((font (if (eq level 'map) (min 24 (/ 11 scale)) 10))
           (label (lispm-ui-scene-sanitize-label
                   (plist-get room :label) (max 1 (floor (/ (- w 48) (* font 0.65))))))
           (label-w (+ 18 (* (string-width label) font 0.62)))
           (label-x (- (+ x (/ w 2)) (/ label-w 2))))
      (svg-rectangle group label-x (+ y 13) label-w 17 :fill trim :opacity 0.28)
      (svg-rectangle group (+ label-x 1) (+ y 14) (- label-w 2) 15
                     :fill (plist-get style :wall))
      (svg-rectangle group (+ label-x 2) (+ y 15) (- label-w 4) 1 :fill "#fff9eb")
      (lispm-ui-scene-svg-text group label (+ x (/ w 2)) (+ y 25)
                               font "#4a433b" "middle" nil "bold"))
    (when (eq level 'map)
      (lispm-ui-scene-svg-text group (format "%d" (length (plist-get room :nodes)))
                               (+ x (/ w 2)) (+ floor-y (/ (- h wall-h) 2))
                               (min 90 (/ 24 scale)) trim "middle"))))

;;;; Workers, stations, and people

(defun lispm-ui-scene--draw-help-arm (group node skin shirt)
  "Draw raised help arm and bubble on GROUP for NODE using SKIN and SHIRT."
  (let ((kind (plist-get (plist-get node :status) :kind))
        (err-p (eq (plist-get (plist-get node :status) :state) 'error)))
    (lispm-ui-scene-svg-rects
     group `((60 20 8 19 ,shirt) (63 10 6 13 ,skin) (61 7 10 7 ,skin)
             (71 1 16 15 "#2a2533")
             (72 2 14 13 ,(if err-p "#e05a52" "#f5ca5c"))
             (73 3 12 2 ,(if err-p "#f48b84" "#fae39c"))
             (69 12 3 3 "#2a2533")))
    (if (eq kind 'question)
        (lispm-ui-scene-svg-rects
         group '((77 4 5 2 "#3a2b1c") (80 6 2 3 "#3a2b1c")
                 (78 8 2 2 "#3a2b1c") (78 12 2 2 "#3a2b1c")))
      (lispm-ui-scene-svg-rects
       group `((78 4 2 6 ,(if err-p "#ffffff" "#3a2b1c"))
               (78 12 2 2 ,(if err-p "#ffffff" "#3a2b1c")))))))

(defun lispm-ui-scene-draw-person (svg node level phase pose)
  "Draw NODE's pixel figure on SVG in POSE using LEVEL and PHASE."
  (let* ((group (lispm-ui-scene-svg-group svg "office-person"))
         (portrait (lispm-ui-scene-portrait-for-node node))
         (skin (plist-get portrait :skin))
         (shirt (plist-get portrait :shirt))
         (light (lispm-ui-blend shirt "#fff2d6" 0.25))
         (dark (lispm-ui-blend shirt "#282332" 0.32))
         (cycle (% phase 6))
         (stride (if (eq pose 'walking) (aref [-4 -2 1 4 2 -1] cycle) 0))
         (bob (cond ((eq pose 'walking) (aref [0 -1 -2 0 -1 -2] cycle))
                    ((eq pose 'typing) (aref [0 0 -1 0 0 1] cycle))
                    (t 0))))
    (dom-set-attribute group 'data-pose (symbol-name pose))
    (dom-set-attribute group 'shape-rendering "crispEdges")
    (lispm-ui-scene-svg-rects
     group `((31 ,(+ 33 bob) 28 19 ,dark) (29 ,(+ 35 bob) 32 13 ,shirt)
             (33 ,(+ 33 bob) 24 4 ,light) (39 ,(+ 33 bob) 12 2 "#f4ead8")
             (40 ,(+ 29 bob) 10 5 ,skin) (25 ,(+ 37 bob) 8 11 ,shirt)
             (57 ,(+ 37 bob) 8 11 ,shirt) (25 ,(+ 37 bob) 6 2 ,light)
             (59 ,(+ 37 bob) 6 2 ,light)))
    (when (memq pose '(walking talking))
      (lispm-ui-scene-svg-rects
       group `((34 50 8 ,(+ 9 (max -3 stride)) "#454050")
               (48 50 8 ,(- 9 (max -3 stride)) "#454050")
               (32 ,(+ 58 (max -3 stride)) 10 4 "#2a2533")
               (48 ,(- 58 (max -3 stride)) 10 4 "#2a2533"))))
    (unless (and (eq level 'desk)
                 (lispm-ui-scene-draw-avatar
                  group (plist-get node :avatar) 30 (+ 3 bob) 30))
      (lispm-ui-scene-draw-pixel-portrait group portrait 29 (+ 1 bob)))
    (pcase pose
      ('coffee
       (lispm-ui-scene-svg-rects
        group `((56 30 8 12 ,shirt) (53 25 6 11 ,skin) (43 23 11 11 "#2a2533")
                (44 24 9 9 "#fbf2e0") (45 24 7 2 "#7b5338") (53 26 4 6 "#fbf2e0")
                (46 18 2 3 "#fffaf0" 0.75) (49 15 2 4 "#fffaf0" 0.65)
                (45 13 2 2 "#fffaf0" 0.45))))
      ('help (lispm-ui-scene--draw-help-arm group node skin shirt))
      ('talking
       (lispm-ui-scene-svg-rects
        group `((60 30 10 6 ,skin) (64 5 24 17 "#3d3730")
                (66 7 20 13 "#fff7e2") (65 20 5 5 "#3d3730")))
       (dolist (x '(69 75 81))
         (svg-rectangle group x 12 3 3 :fill "#635a4e")))
      (_
       (svg-rectangle group 24 (+ 47 (if (eq pose 'walking) stride 0)) 8 4 :fill skin)
       (svg-rectangle group 58 (- 47 (if (eq pose 'walking) stride 0)) 8 4 :fill skin)))
    group))

(defun lispm-ui-scene-draw-station (svg node phase working)
  "Draw NODE's stationary desk, chair, and monitor on SVG at PHASE when WORKING."
  (let* ((wood "#d8be9c") (edge "#876f59") (dark-wood "#695442")
         (frame "#424046") (bezel "#5b5961")
         (cycle (% phase 6))
         (group (lispm-ui-scene-svg-group svg "office-station")))
    (dom-set-attribute group 'data-key
                       (xml-escape-string (prin1-to-string (plist-get node :key))))
    (lispm-ui-scene-svg-rects
     group `((10 53 70 10 ,edge) (12 53 66 6 ,wood) (12 53 66 2 "#f2e1c4")
             (13 59 64 3 ,dark-wood) (13 62 5 5 ,dark-wood) (62 59 14 8 ,edge)
             (63 60 12 6 "#9e846b") (67 62 4 2 "#e8d6bc") (16 55 10 4 "#b5a692")
             (16 55 10 2 "#f7ebd6") (66 47 11 7 "#9e907d") (66 47 11 2 "#faf0dc")
             (68 45 9 2 "#749483") (26 57 38 4 ,edge) (28 57 34 2 "#d9d2c5")
             (38 57 14 2 "#f7ead4") (42 54 6 5 ,frame) (27 36 36 22 "#2c2a30")
             (28 37 34 20 ,frame) (28 37 34 2 ,bezel)
             (30 39 30 15 ,(if working "#1f2b33" "#5a635f"))
             (30 39 30 2 ,(if working "#38505e" "#7c8580"))
             (58 55 2 2 ,(if working "#4cd98c" "#9e988e"))))
    (when working
      (let ((c1 (aref [10 14 18 12 16 20] cycle))
            (c2 (aref [16 10 14 18 12 8] cycle))
            (c3 (aref [8 12 10 16 14 12] cycle))
            (sy (aref [32 30 27 31 28 25] cycle))
            (sx (aref [33 54 44 36 52 41] cycle)))
        (lispm-ui-scene-svg-rects
         group `((32 42 3 2 "#f0c674") (36 42 ,c1 2 "#b5e8b0")
                 (32 45 2 2 "#81a2be") (35 45 ,c2 2 "#8abeb7")
                 (32 48 4 2 "#b294bb") (37 48 ,c3 2 "#de935f")))
        (when (zerop (% cycle 2))
          (svg-rectangle group (+ 38 c3) 48 3 2 :fill "#fff6d6"))
        (svg-rectangle group sx sy 3 3 :fill "#ffe89e")
        (svg-rectangle group (+ sx 1) (- sy 3) 2 2 :fill "#8ce0b8")))))

(defun lispm-ui-scene-draw-worker (svg node level phase &optional away)
  "Draw NODE's workstation and pose on SVG at LEVEL and PHASE; AWAY empties chair."
  (let* ((status (plist-get node :status))
         (state (or (plist-get status :state) 'unknown))
         (working (and (eq state 'working) (not away)))
         (pose (cond ((memq state '(blocked error)) 'help)
                     ((eq state 'working) 'typing)
                     (t 'coffee)))
         (portrait (lispm-ui-scene-portrait-for-node node))
         (cycle (% phase 6)))
    (dom-set-attribute svg 'shape-rendering "crispEdges")
    (dom-set-attribute svg 'data-pose (if away "away" (symbol-name pose)))
    (lispm-ui-scene-svg-rects
     svg '((14 64 62 3 "#4a433b" 0.18) (31 25 28 30 "#5c5852")
           (33 27 24 24 "#959c90") (35 28 20 4 "#aeb5a8")
           (28 38 4 10 "#5c5852") (58 38 4 10 "#5c5852")))
    (unless away
      (lispm-ui-scene-draw-person svg node level phase pose))
    (lispm-ui-scene-draw-station svg node phase working)
    (when working
      (let ((skin (plist-get portrait :skin))
            (lh (aref [0 2 1 -1 2 0] cycle))
            (rh (aref [2 0 -1 2 0 1] cycle)))
        (lispm-ui-scene-svg-rects
         svg `((32 ,(+ 54 lh) 8 4 ,skin) (50 ,(+ 54 rh) 8 4 ,skin)
               (,(+ 30 (% cycle 3)) ,(- 51 (abs lh)) 2 2 "#fff0b0")
               (,(- 59 (% cycle 3)) ,(- 51 (abs rh)) 2 2 "#9be8c8")))))))

(provide 'lispm-ui-scene-sprites)
;;; lispm-ui-scene-sprites.el ends here
