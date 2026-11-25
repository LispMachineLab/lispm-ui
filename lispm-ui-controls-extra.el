;;; lispm-ui-controls-extra.el --- Modern semantic buttons, switch, slider, stepper, segmented -*- lexical-binding: t; -*-

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

;; Extended modern interactive controls built atop `lispm-ui-tokens',
;; `lispm-ui-icons', `lispm-ui-draw', and `lispm-ui-controls':
;;
;; - Semantic button variants (`primary', `secondary', `ghost',
;;   `destructive', `icon', plus `:loading' and `:disabled').
;; - `lispm-ui-switch' / `lispm-ui-insert-switch'.
;; - `lispm-ui-slider' / `lispm-ui-insert-slider'.
;; - `lispm-ui-stepper' / `lispm-ui-insert-stepper'.
;; - `lispm-ui-segmented' / `lispm-ui-insert-segmented'.
;; - `lispm-ui-checkbox-tristate' / `lispm-ui-insert-checkbox-tristate'.
;; - `lispm-ui-insert-bevel-button'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'svg)
(require 'button)
(require 'lispm-ui-base)
(require 'lispm-ui-palette)
(require 'lispm-ui-tokens)
(require 'lispm-ui-look)
(require 'lispm-ui-text)
(require 'lispm-ui-draw)
(require 'lispm-ui-icons)
(require 'lispm-ui-controls)

;;;; Semantic Button Variants

(defun lispm-ui-controls--button-colors (variant disabled pressed)
  "Return plist `(:bg :fg :border)' for button VARIANT, DISABLED, PRESSED."
  (if disabled
      (list :bg (or (lispm-ui-token :disabled-bg) "#e5e5e5")
            :fg (or (lispm-ui-token :disabled-fg) "#888888")
            :border (or (lispm-ui-token :disabled-border) "#cccccc"))
    (pcase variant
      ('primary
       (let ((bg (if pressed
                     (or (lispm-ui-color :highlight) "#1d4ed8")
                   (or (lispm-ui-color :accent) "#2563eb"))))
         (list :bg bg
               :fg (lispm-ui-wcag-legible bg "#ffffff" "#000000")
               :border (or (lispm-ui-color :outline) bg))))
      ('destructive
       (let ((bg (if pressed
                     (or (lispm-ui-color-hex (lispm-ui-blend "#dc2626" "#000000" 0.25)) "#b91c1c")
                   (or (lispm-ui-token :danger) "#dc2626"))))
         (list :bg bg
               :fg (lispm-ui-wcag-legible bg "#ffffff" "#000000")
               :border bg)))
      ('ghost
       (list :bg (when pressed (or (lispm-ui-token :hover) "#eeeeee"))
             :fg (or (lispm-ui-color :text) "#000000")
             :border nil))
      (_
       (list :bg (if pressed
                     (or (lispm-ui-token :active) "#d4d4d8")
                   (or (lispm-ui-token :surface-2) "#eeeeee"))
             :fg (or (lispm-ui-color :text) "#000000")
             :border (or (lispm-ui-token :border-strong) "#888888"))))))

(defun lispm-ui-controls--draw-button-svg (width height text colors focused)
  "Return SVG of a semantic button WIDTH x HEIGHT reading TEXT in COLORS."
  (let* ((svg (svg-create width height))
         (bg (plist-get colors :bg))
         (fg (plist-get colors :fg))
         (border (plist-get colors :border)))
    (when bg
      (svg-rectangle svg 1 1 (- width 2) (- height 2) :rx 4 :fill bg))
    (when border
      (svg-rectangle svg 1 1 (- width 2) (- height 2)
                     :rx 4 :fill "none" :stroke border :stroke-width 1))
    (when focused
      (svg-rectangle svg 0 0 width height :rx 5 :fill "none"
                     :stroke (or (lispm-ui-token :focus-ring) "#2563eb")
                     :stroke-width 1.5))
    (when (and text (not (string-empty-p text)))
      (let* ((tw (lispm-ui-text-width text 'lispm-ui-title))
             (tx (max 6 (/ (- width tw) 2))))
        (lispm-ui-svg-content svg text tx 1 (- height 2) fg 'lispm-ui-title)))
    svg))

(cl-defun lispm-ui-button (label &key (variant 'secondary) icon loading disabled
                                 focused pressed width)
  "Return a semantic button string reading LABEL.
VARIANT may be `primary', `secondary', `ghost', `destructive', or `icon'.
ICON names an icon from `lispm-ui-icon'; LOADING adds a waiting indicator."
  (let* ((text (cond
                ((eq variant 'icon) (or (and loading "…") (if icon "◈" label) ""))
                (loading (concat "… " (or label "")))
                (t (or label ""))))
         (colors (lispm-ui-controls--button-colors variant (or disabled loading) pressed)))
    (if (lispm-ui-tty-p)
        (let* ((icon-str (when (and icon (not loading))
                           (concat (lispm-ui-icon icon :tint (plist-get colors :fg)) " ")))
               (inner (string-trim (concat (or icon-str "") text)))
               (face `(:foreground ,(plist-get colors :fg)
                       ,@(when (plist-get colors :bg) (list :background (plist-get colors :bg)))
                       ,@(when (memq variant '(primary destructive)) (list :weight 'bold)))))
          (propertize (if (eq variant 'ghost) (concat " " inner " ") (concat "[" inner "]"))
                      'face face
                      'lispm-ui-label label
                      'lispm-ui-button-variant variant))
      (let* ((h 22)
             (pad (if (eq variant 'icon) 12 20))
             (w (max (or width 0) (+ pad (lispm-ui-text-width text 'lispm-ui-title))))
             (img (lispm-ui-look-image
                   (list 'btn2 variant text (or disabled loading) focused pressed w)
                   (lambda ()
                     (lispm-ui-controls--draw-button-svg w h text colors focused)))))
        (propertize " " 'display img 'lispm-ui-label label
                    'lispm-ui-button-variant variant)))))

(cl-defun lispm-ui-insert-button (label action &key (variant 'secondary) icon loading
                                        disabled help width)
  "Insert an interactive button reading LABEL calling ACTION when activated."
  (let ((inactive (or disabled loading)))
    (if inactive
        (insert (propertize
                 (lispm-ui-button label :variant variant :icon icon :loading loading
                                  :disabled disabled :width width)
                 'help-echo (or help label)))
      (let* ((plain (lispm-ui-button label :variant variant :icon icon :width width))
             (focused (lispm-ui-button label :variant variant :icon icon :width width :focused t))
             (pressed (lispm-ui-button label :variant variant :icon icon :width width :pressed t))
             (start (point))
             (act (lambda (&rest _)
                    (let ((lispm-ui-in-gui-action t))
                      (funcall action)))))
        (insert-text-button
         (if (lispm-ui-tty-p)
             plain
           (propertize " "
                       'display (get-text-property 0 'display plain)
                       'lispm-ui-plain (get-text-property 0 'display plain)
                       'lispm-ui-focused (get-text-property 0 'display focused)
                       'lispm-ui-pressed (get-text-property 0 'display pressed)))
         'action act
         'lispm-ui-label label
         'lispm-ui-focusable t
         'keymap lispm-ui-push-button-map
         'follow-link t
         'help-echo (or help label))
        (put-text-property start (point) 'lispm-ui-focusable t)))))

;;;; Switch / Toggle

(defun lispm-ui-controls--draw-switch-svg (on disabled focused)
  "Return 30x16 pill switch SVG for ON, DISABLED, FOCUSED."
  (let* ((svg (svg-create 30 16))
         (track (cond (disabled (or (lispm-ui-token :disabled-bg) "#cccccc"))
                      (on (or (lispm-ui-color :accent) "#2563eb"))
                      (t (or (lispm-ui-token :surface-3) "#b0b0b8"))))
         (knob (if disabled (or (lispm-ui-token :disabled-fg) "#999999") "#ffffff"))
         (cx (if on 21 9)))
    (svg-rectangle svg 1 1 28 14 :rx 7 :fill track
                   :stroke (or (lispm-ui-token :border-strong) "#666666")
                   :stroke-width 0.75)
    (svg-circle svg cx 8 5 :fill knob)
    (when focused
      (svg-rectangle svg 0 0 30 16 :rx 8 :fill "none"
                     :stroke (or (lispm-ui-token :focus-ring) "#2563eb")
                     :stroke-width 1.5))
    svg))

(cl-defun lispm-ui-switch (on &key disabled focused)
  "Return a toggle switch string representing ON."
  (if (lispm-ui-tty-p)
      (propertize (if on "[● ]" "[ ○]")
                  'face (cond (disabled 'shadow)
                              (on 'lispm-ui-success)
                              (t 'lispm-ui-muted)))
    (propertize " " 'display
                (lispm-ui-look-image
                 (list 'switch (and on t) (and disabled t) (and focused t))
                 (lambda () (lispm-ui-controls--draw-switch-svg on disabled focused))))))

(cl-defun lispm-ui-insert-switch (label on on-toggle &key disabled help)
  "Insert an interactive switch labeled LABEL in state ON calling ON-TOGGLE."
  (let ((act (lambda ()
               (when on-toggle
                 (funcall on-toggle (not on))))))
    (if disabled
        (insert (propertize (concat (lispm-ui-switch on :disabled t)
                                    (when label (concat " " label)))
                            'face 'shadow 'help-echo (or help label)))
      (let* ((plain (lispm-ui-switch on))
             (focused (lispm-ui-switch on :focused t))
             (start (point)))
        (insert-text-button
         (concat (if (lispm-ui-tty-p)
                     plain
                   (propertize " "
                               'display (get-text-property 0 'display plain)
                               'lispm-ui-plain (get-text-property 0 'display plain)
                               'lispm-ui-focused (get-text-property 0 'display focused)
                               'lispm-ui-pressed (get-text-property 0 'display focused)))
                 (when label (concat " " label)))
         'action (lambda (_) (let ((lispm-ui-in-gui-action t)) (funcall act)))
         'keymap lispm-ui-control-button-map
         'follow-link t
         'help-echo (or help label))
        (put-text-property start (point) 'lispm-ui-focusable t)))))

;;;; Slider

(defun lispm-ui-controls--ratio (value min max)
  "Return normalized ratio in [0.0, 1.0] for VALUE between MIN and MAX."
  (let* ((lo (float (or min 0)))
         (hi (float (or max 100)))
         (v (float (or value lo))))
    (if (<= hi lo)
        0.0
      (max 0.0 (min 1.0 (/ (- v lo) (- hi lo)))))))

(defun lispm-ui-controls--draw-slider-svg (ratio width disabled focused)
  "Return SVG slider bar WIDTH pixels wide filled to RATIO."
  (let* ((w (max 40 width))
         (h 16)
         (svg (svg-create w h))
         (usable (- w 12))
         (tx (+ 6 (round (* usable ratio))))
         (track-bg (or (lispm-ui-token :surface-3) "#d4d4d8"))
         (fill-bg (if disabled
                      (or (lispm-ui-token :disabled-fg) "#999999")
                    (or (lispm-ui-color :accent) "#2563eb"))))
    (svg-rectangle svg 6 6 usable 4 :rx 2 :fill track-bg)
    (svg-rectangle svg 6 6 (max 2 (- tx 6)) 4 :rx 2 :fill fill-bg)
    (svg-circle svg tx 8 5 :fill "#ffffff" :stroke fill-bg :stroke-width 2)
    (when focused
      (svg-rectangle svg 0 0 w h :rx 4 :fill "none"
                     :stroke (or (lispm-ui-token :focus-ring) "#2563eb")
                     :stroke-width 1.5))
    svg))

(cl-defun lispm-ui-slider (value &key (min 0) (max 100) (width 140) disabled focused)
  "Return a slider control string for VALUE in [MIN, MAX]."
  (let ((ratio (lispm-ui-controls--ratio value min max)))
    (if (lispm-ui-tty-p)
        (let* ((cells (max 6 (if (<= width 40) width (lispm-ui-px width))))
               (pos (round (* (1- cells) ratio)))
               (left (make-string pos ?━))
               (right (make-string (max 0 (- cells 1 pos)) ?─)))
          (propertize (concat left "●" right)
                      'face (if disabled 'shadow 'lispm-ui-body)))
      (let ((q (round (* 100 ratio))))
        (propertize " " 'display
                    (lispm-ui-look-image
                     (list 'slider q width (and disabled t) (and focused t))
                     (lambda () (lispm-ui-controls--draw-slider-svg ratio width disabled focused))))))))

(cl-defun lispm-ui-insert-slider (value on-change &key (min 0) (max 100) (step 1)
                                        (width 140) disabled label)
  "Insert an interactive slider initialized to VALUE invoking ON-CHANGE."
  (let* ((cur (max min (min max (or value min))))
         (emit (lambda (next)
                 (let ((clamped (max min (min max next))))
                   (when on-change (funcall on-change clamped))
                   clamped)))
         (map (unless disabled
                (let ((m (make-sparse-keymap)))
                  (keymap-set m "<right>" (lambda () (interactive) (funcall emit (+ cur step))))
                  (keymap-set m "<left>" (lambda () (interactive) (funcall emit (- cur step))))
                  (keymap-set m "+" (lambda () (interactive) (funcall emit (+ cur step))))
                  (keymap-set m "-" (lambda () (interactive) (funcall emit (- cur step))))
                  (keymap-set m "<home>" (lambda () (interactive) (funcall emit min)))
                  (keymap-set m "<end>" (lambda () (interactive) (funcall emit max)))
                  m))))
    (insert (propertize
             (concat (when label (concat label " "))
                     (lispm-ui-slider cur :min min :max max :width width :disabled disabled))
             'lispm-ui-focusable (not disabled)
             'keymap map
             'pointer (unless disabled 'hand)
             'help-echo (format "%s / %s" cur max)))))

;;;; Stepper & Segmented Control

(cl-defun lispm-ui-stepper (value &key (min most-negative-fixnum)
                                  (max most-positive-fixnum) disabled)
  "Return a compact numeric stepper display string for VALUE."
  (ignore min max)
  (propertize (format "[−] %s [+]" value)
              'face (if disabled 'shadow 'lispm-ui-tabular)))

(cl-defun lispm-ui-insert-stepper (value on-change &key (min 0) (max 100) (step 1)
                                         disabled label)
  "Insert an interactive numeric stepper holding VALUE calling ON-CHANGE."
  (let* ((cur (max min (min max (or value min))))
         (dec (lambda () (when on-change (funcall on-change (max min (- cur step))))))
         (inc (lambda () (when on-change (funcall on-change (min max (+ cur step))))))
         (map (unless disabled
                (let ((m (make-sparse-keymap)))
                  (keymap-set m "-" (lambda () (interactive) (funcall dec)))
                  (keymap-set m "<left>" (lambda () (interactive) (funcall dec)))
                  (keymap-set m "+" (lambda () (interactive) (funcall inc)))
                  (keymap-set m "<right>" (lambda () (interactive) (funcall inc)))
                  m))))
    (when label (insert label " "))
    (lispm-ui-insert-button "−" dec :variant 'secondary :disabled (or disabled (<= cur min)))
    (insert " ")
    (let ((val-start (point)))
      (insert (propertize (format "%s" cur) 'face 'lispm-ui-tabular))
      (when map
        (add-text-properties val-start (point)
                             (list 'keymap map 'lispm-ui-focusable t))))
    (insert " ")
    (lispm-ui-insert-button "+" inc :variant 'secondary :disabled (or disabled (>= cur max)))))

(cl-defun lispm-ui-segmented (options selected &key disabled)
  "Return a segmented control string rendering OPTIONS with SELECTED active."
  (mapconcat
   (pcase-lambda (`(,val . ,label))
     (let ((active (equal val selected)))
       (propertize (concat " " label " ")
                   'face (cond
                          (disabled 'shadow)
                          (active `(:background ,(or (lispm-ui-color :accent) "#2563eb")
                                    :foreground ,(lispm-ui-wcag-legible
                                                  (or (lispm-ui-color :accent) "#2563eb")
                                                  "#ffffff" "#000000")
                                    :weight bold))
                          (t `(:background ,(or (lispm-ui-token :surface-2) "#e4e4e7")
                               :foreground ,(or (lispm-ui-color :text) "#000000")))))))
   options
   (propertize " " 'face 'default)))

(cl-defun lispm-ui-insert-segmented (options selected on-change &key disabled)
  "Insert an interactive segmented control over OPTIONS invoking ON-CHANGE."
  (let ((keys (mapcar #'car options)))
    (cl-loop for (val . label) in options for idx from 0 do
             (when (> idx 0) (insert " "))
             (let* ((active (equal val selected))
                    (map (unless disabled
                           (let ((m (make-sparse-keymap))
                                 (prev (nth (mod (1- idx) (length keys)) keys))
                                 (next (nth (mod (1+ idx) (length keys)) keys)))
                             (keymap-set m "<left>" (lambda () (interactive)
                                                      (when on-change (funcall on-change prev))))
                             (keymap-set m "<right>" (lambda () (interactive)
                                                       (when on-change (funcall on-change next))))
                             (keymap-set m "RET" (lambda () (interactive)
                                                   (when on-change (funcall on-change val))))
                             (keymap-set m "SPC" (lambda () (interactive)
                                                   (when on-change (funcall on-change val))))
                             (keymap-set m "<down-mouse-1>"
                                         (lambda (_e) (interactive "e")
                                           (when on-change (funcall on-change val))))
                             (keymap-set m "<mouse-1>" #'ignore)
                             m))))
               (insert (propertize
                        (if (lispm-ui-tty-p)
                            (if active (format "[%s]" label) (format " %s " label))
                          (concat " " label " "))
                        'face (cond
                               (disabled 'shadow)
                               (active 'lispm-ui-button-pressed)
                               (t 'lispm-ui-button))
                        'lispm-ui-focusable (not disabled)
                        'pointer (unless disabled 'hand)
                        'keymap map))))))

;;;; Tri-state Check Box & Bevel Button wrapper

(cl-defun lispm-ui-checkbox-tristate (state &key disabled focused)
  "Return a check box string for STATE (`nil', `t', or `indeterminate')."
  (cond
   ((not (eq state 'indeterminate))
    (lispm-ui-check-box (and state t) (and disabled 'disabled) focused 2))
   ((lispm-ui-tty-p)
    (propertize "[-]" 'face (if disabled 'shadow 'lispm-ui-body)))
   (t
    (propertize
     " " 'display
     (lispm-ui-look-image
      (list 'checkbox-indeterminate (and disabled t) (and focused t))
      (lambda ()
        (let* ((svg (svg-create 16 16))
               (bg (if disabled
                       (or (lispm-ui-token :disabled-bg) "#dddddd")
                     (or (lispm-ui-color :content) "#ffffff")))
               (fg (if disabled
                       (or (lispm-ui-token :disabled-fg) "#888888")
                     (or (lispm-ui-color :text) "#000000"))))
          (svg-rectangle svg 2 2 12 12 :fill bg :stroke fg :stroke-width 1)
          (svg-rectangle svg 5 7 6 2 :fill fg)
          (when focused
            (svg-rectangle svg 0 0 16 16 :rx 2 :fill "none"
                           :stroke (or (lispm-ui-token :focus-ring) "#2563eb")
                           :stroke-width 1.5))
          svg)))))))

(cl-defun lispm-ui-insert-checkbox-tristate (label state on-change &key disabled help)
  "Insert an interactive tri-state check box for STATE calling ON-CHANGE."
  (let ((next (if (eq state t) nil t)))
    (if disabled
        (insert (propertize (concat (lispm-ui-checkbox-tristate state :disabled t)
                                    (when label (concat " " label)))
                            'face 'shadow 'help-echo (or help label)))
      (let* ((plain (lispm-ui-checkbox-tristate state))
             (focused (lispm-ui-checkbox-tristate state :focused t))
             (start (point)))
        (insert-text-button
         (concat (if (lispm-ui-tty-p)
                     plain
                   (propertize " "
                               'display (get-text-property 0 'display plain)
                               'lispm-ui-plain (get-text-property 0 'display plain)
                               'lispm-ui-focused (get-text-property 0 'display focused)
                               'lispm-ui-pressed (get-text-property 0 'display focused)))
                 (when label (concat " " label)))
         'action (lambda (_)
                   (when on-change
                     (let ((lispm-ui-in-gui-action t))
                       (funcall on-change next))))
         'keymap lispm-ui-control-button-map
         'follow-link t
         'help-echo (or help label))
        (put-text-property start (point) 'lispm-ui-focusable t)))))

(cl-defun lispm-ui-insert-bevel-button (label action &key active disabled help)
  "Insert an interactive bevel button reading LABEL calling ACTION."
  (let ((state (cond (disabled 'disabled) (active 'pressed) (t nil))))
    (if disabled
        (insert (propertize (lispm-ui-bevel-button label state) 'help-echo (or help label)))
      (let ((plain (lispm-ui-bevel-button label state))
            (pressed (lispm-ui-bevel-button label 'pressed))
            (start (point)))
        (insert-text-button
         (if (lispm-ui-tty-p)
             plain
           (propertize " "
                       'display (get-text-property 0 'display plain)
                       'lispm-ui-plain (get-text-property 0 'display plain)
                       'lispm-ui-focused (get-text-property 0 'display plain)
                       'lispm-ui-pressed (get-text-property 0 'display pressed)))
         'action (lambda (_)
                   (let ((lispm-ui-in-gui-action t))
                     (funcall action)))
         'lispm-ui-label label
         'keymap lispm-ui-push-button-map
         'follow-link t
         'help-echo (or help label))
        (put-text-property start (point) 'lispm-ui-focusable t)))))

(provide 'lispm-ui-controls-extra)
;;; lispm-ui-controls-extra.el ends here
