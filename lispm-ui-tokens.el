;;; lispm-ui-tokens.el --- Design tokens 2.0: semantics, WCAG contrast, scales -*- lexical-binding: t; -*-

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

;; Design Token 2.0 layer extending `lispm-ui-palette' without altering
;; classic 33-slot snapshot outputs:
;;
;; - WCAG 2.1 sRGB linearized relative luminance (`lispm-ui-relative-luminance'),
;;   contrast ratio (`lispm-ui-contrast-ratio'), and contrast enforcement
;;   (`lispm-ui-ensure-contrast').
;; - Semantic status roles (`:success', `:warning', `:info', `:danger' with
;;   `-fg', `-bg', `-on-bg', `-border'), interactive states (`:hover',
;;   `:active', `:selected-bg'/`-fg', `:disabled-bg'/`-fg'/`-border',
;;   `:placeholder', `:focus-ring', `:scrim'), and dark-mode surface elevation
;;   (`:surface-0' through `:surface-3').
;; - Spacing scale (`lispm-ui-space'), radius scale (`lispm-ui-radius'),
;;   and typographic hierarchy faces (`lispm-ui-h1'..`h4', `lispm-ui-body',
;;   `lispm-ui-caption', `lispm-ui-muted', `lispm-ui-code', `lispm-ui-tabular').

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'lispm-ui-base)
(require 'lispm-ui-frp)
(require 'lispm-ui-palette)
(require 'lispm-ui-look)

;;;; WCAG 2.1 color science

(defun lispm-ui--srgb-linear (channel)
  "Return gamma-linearized sRGB value for normalized CHANNEL in [0.0, 1.0]."
  (let ((c (max 0.0 (min 1.0 (float channel)))))
    (if (<= c 0.04045)
        (/ c 12.92)
      (expt (/ (+ c 0.055) 1.055) 2.4))))

(defun lispm-ui-relative-luminance (color)
  "Return WCAG 2.1 linearized relative luminance of COLOR in [0.0, 1.0]."
  (pcase-let ((`(,r ,g ,b) (or (lispm-ui-rgb color) '(0.0 0.0 0.0))))
    (+ (* 0.2126 (lispm-ui--srgb-linear r))
       (* 0.7152 (lispm-ui--srgb-linear g))
       (* 0.0722 (lispm-ui--srgb-linear b)))))

(defun lispm-ui-contrast-ratio (fg bg)
  "Return WCAG 2.1 contrast ratio between FG and BG in [1.0, 21.0]."
  (let* ((l1 (lispm-ui-relative-luminance fg))
         (l2 (lispm-ui-relative-luminance bg))
         (bright (max l1 l2))
         (dark (min l1 l2)))
    (/ (+ bright 0.05) (+ dark 0.05))))

(defun lispm-ui-wcag-legible (background &rest colors)
  "Return whichever of COLORS achieves highest WCAG contrast on BACKGROUND."
  (car (seq-sort-by (lambda (color)
                      (lispm-ui-contrast-ratio color background))
                    #'> colors)))

(defun lispm-ui-ensure-contrast (fg bg &optional min-ratio)
  "Return hex color from FG adjusted toward black/white to reach MIN-RATIO on BG.
MIN-RATIO defaults to 4.5 (WCAG 2.1 AA normal text)."
  (let* ((target (or min-ratio 4.5))
         (hex (or (lispm-ui-color-hex fg) "#000000"))
         (bg-hex (or (lispm-ui-color-hex bg) "#ffffff")))
    (if (>= (lispm-ui-contrast-ratio hex bg-hex) target)
        hex
      (let ((pole (lispm-ui-wcag-legible bg-hex "#000000" "#ffffff")))
        (if (< (lispm-ui-contrast-ratio pole bg-hex) target)
            pole
          (let ((low 0.0)
                (high 1.0)
                (best pole))
            (dotimes (_ 12)
              (let* ((mid (/ (+ low high) 2.0))
                     (cand (lispm-ui-blend hex pole mid)))
                (if (and cand (>= (lispm-ui-contrast-ratio cand bg-hex) target))
                    (setq best cand high mid)
                  (setq low mid))))
            best))))))

;;;; Semantic token derivation

(defconst lispm-ui-token--semantic-seeds
  '((:success . "#16a34a")
    (:warning . "#d97706")
    (:info    . "#2563eb")
    (:danger  . "#dc2626"))
  "Default seed colors for semantic status roles.")

(defun lispm-ui-dark-p (&optional palette)
  "Return non-nil when PALETTE (default `lispm-ui-current-palette') is dark."
  (let* ((pal (or palette lispm-ui-current-palette))
         (content (or (plist-get pal :content) "#ffffff")))
    (< (lispm-ui-relative-luminance content) 0.2)))

(defun lispm-ui--derive-role-tokens (role base content dark-p)
  "Return plist of derived `:ROLE*' tokens from BASE on CONTENT."
  (let* ((name (substring (symbol-name role) 1))
         (seed (or (lispm-ui-color-hex base) base))
         (fg (lispm-ui-ensure-contrast seed content 4.5))
         (bg (lispm-ui-blend content seed (if dark-p 0.20 0.12)))
         (on-bg (lispm-ui-ensure-contrast seed bg 4.5))
         (border (lispm-ui-blend bg seed 0.45)))
    (list role seed
          (intern (concat ":" name "-fg")) fg
          (intern (concat ":" name "-bg")) bg
          (intern (concat ":" name "-on-bg")) on-bg
          (intern (concat ":" name "-border")) border)))

(defun lispm-ui-derive-tokens (palette)
  "Derive Design Token 2.0 plist from base PALETTE without mutating it."
  (let* ((pal (or palette '(:content "#ffffff" :text "#000000"
                            :dim "#555555" :accent "#333399"
                            :chrome "#dddddd" :muted "#444444"
                            :outline "#000000" :highlight "#222266"
                            :focus "#333399")))
         (content (or (lispm-ui-color-hex (plist-get pal :content)) "#ffffff"))
         (text (or (lispm-ui-color-hex (plist-get pal :text)) "#000000"))
         (chrome (or (lispm-ui-color-hex (plist-get pal :chrome)) "#dddddd"))
         (muted (or (lispm-ui-color-hex (plist-get pal :muted)) "#555555"))
         (accent (or (lispm-ui-color-hex (plist-get pal :accent)) "#2563eb"))
         (outline (or (lispm-ui-color-hex (plist-get pal :outline)) "#000000"))
         (dark-p (< (lispm-ui-relative-luminance content) 0.2))
         ;; Dark mode steps surfaces progressively lighter; light mode steps subtly.
         (s0 content)
         (s1 (if dark-p
                 (lispm-ui-blend content "#ffffff" 0.06)
               (lispm-ui-blend content chrome 0.22)))
         (s2 (if dark-p
                 (lispm-ui-blend content "#ffffff" 0.12)
               (lispm-ui-blend content chrome 0.45)))
         (s3 (if dark-p
                 (lispm-ui-blend content "#ffffff" 0.18)
               (lispm-ui-blend content chrome 0.70)))
         (sel-bg (or (lispm-ui-color-hex (plist-get pal :highlight)) accent))
         (sel-fg (lispm-ui-wcag-legible sel-bg text content "#ffffff" "#000000"))
         (base-tokens
          (list :surface-0 s0
                :surface-1 s1
                :surface-2 s2
                :surface-3 s3
                :hover (lispm-ui-blend s1 text (if dark-p 0.08 0.05))
                :active (lispm-ui-blend s1 accent (if dark-p 0.22 0.14))
                :selected-bg sel-bg
                :selected-fg sel-fg
                :disabled-bg (lispm-ui-blend s1 content 0.5)
                :disabled-fg (lispm-ui-blend content muted 0.55)
                :disabled-border (lispm-ui-blend content chrome 0.45)
                :placeholder (lispm-ui-blend content muted 0.72)
                :border-subtle (lispm-ui-blend content outline (if dark-p 0.24 0.16))
                :border-strong (lispm-ui-blend content outline (if dark-p 0.48 0.36))
                :focus-ring (or (lispm-ui-color-hex (plist-get pal :focus)) accent)
                :scrim (if dark-p "#000000" "#111111"))))
    (dolist (seed lispm-ui-token--semantic-seeds base-tokens)
      (setq base-tokens
            (append base-tokens
                    (lispm-ui--derive-role-tokens
                     (car seed)
                     (or (plist-get pal (car seed)) (cdr seed))
                     content dark-p))))))

(defvar lispm-ui-current-tokens nil
  "Current derived Design Token 2.0 plist synced with `lispm-ui-current-palette'.")

(defvar lispm-ui-tokens-signal (lispm-ui-signal nil #'equal)
  "Reactive signal holding `lispm-ui-current-tokens'.")

(defun lispm-ui-token (name &optional palette)
  "Return color token NAME from PALETTE (or `lispm-ui-current-palette').
Checks derived Design Token 2.0 roles first, then base PALETTE slots."
  (let* ((pal (or palette lispm-ui-current-palette))
         (tokens (if palette
                     (lispm-ui-derive-tokens pal)
                   (or lispm-ui-current-tokens
                       (and pal (lispm-ui-derive-tokens pal))))))
    (or (plist-get tokens name)
        (plist-get pal name))))

;;;; Spacing & radius scales

(defconst lispm-ui-space-scale
  '((none . 0) (xs . 4) (sm . 8) (md . 12) (lg . 16) (xl . 24) (2xl . 32))
  "Standard pixel spacing scale for LispM UI components.")

(defconst lispm-ui-radius-scale
  '((none . 0) (sm . 2) (md . 4) (lg . 8) (pill . 9999))
  "Standard corner radius scale in pixels.")

(defun lispm-ui-space (size)
  "Return spacing in pixels for token SIZE (`none' `xs' `sm' `md' `lg' `xl' `2xl').
When SIZE is already a non-negative number, return it directly."
  (cond
   ((integerp size) (max 0 size))
   ((numberp size) (max 0 (round size)))
   (t (or (alist-get size lispm-ui-space-scale) 0))))

(defun lispm-ui-radius (size)
  "Return corner radius in pixels for token SIZE (`none' `sm' `md' `lg' `pill').
When SIZE is already a non-negative number, return it directly."
  (cond
   ((integerp size) (max 0 size))
   ((numberp size) (max 0 (round size)))
   (t (or (alist-get size lispm-ui-radius-scale) 0))))

;;;; Typographic & semantic faces

(defface lispm-ui-h1 '((t :inherit lispm-ui-title :height 1.5))
  "Primary view title heading."
  :group 'lispm-ui)

(defface lispm-ui-h2 '((t :inherit lispm-ui-title :height 1.25))
  "Section heading."
  :group 'lispm-ui)

(defface lispm-ui-h3 '((t :inherit lispm-ui-title :height 1.1))
  "Subsection or card heading."
  :group 'lispm-ui)

(defface lispm-ui-h4 '((t :inherit lispm-ui-title :height 1.0))
  "Compact group heading."
  :group 'lispm-ui)

(defface lispm-ui-body '((t :inherit lispm-ui-list))
  "Standard body text in LispM UI views."
  :group 'lispm-ui)

(defface lispm-ui-caption '((t :inherit lispm-ui-small))
  "Secondary caption text."
  :group 'lispm-ui)

(defface lispm-ui-muted '((t :inherit shadow))
  "Subdued helper or placeholder text."
  :group 'lispm-ui)

(defface lispm-ui-code '((t :inherit fixed-pitch))
  "Monospace inline code face."
  :group 'lispm-ui)

(defface lispm-ui-tabular '((t :inherit fixed-pitch))
  "Aligned numeric column face."
  :group 'lispm-ui)

(defface lispm-ui-success '((t :weight bold))
  "Semantic success status face."
  :group 'lispm-ui)

(defface lispm-ui-warning '((t :weight bold))
  "Semantic warning status face."
  :group 'lispm-ui)

(defface lispm-ui-info '((t :weight bold))
  "Semantic informational status face."
  :group 'lispm-ui)

(defface lispm-ui-danger '((t :weight bold))
  "Semantic destructive or error status face."
  :group 'lispm-ui)

(defun lispm-ui-tokens--sync ()
  "Synchronize Design Token 2.0 values and faces from `lispm-ui-current-palette'."
  (when lispm-ui-current-palette
    (let* ((tokens (lispm-ui-derive-tokens lispm-ui-current-palette))
           (text (plist-get lispm-ui-current-palette :text))
           (muted (plist-get lispm-ui-current-palette :muted)))
      (setq lispm-ui-current-tokens tokens)
      (set-face-attribute 'lispm-ui-h1 nil :foreground text)
      (set-face-attribute 'lispm-ui-h2 nil :foreground text)
      (set-face-attribute 'lispm-ui-h3 nil :foreground text)
      (set-face-attribute 'lispm-ui-h4 nil :foreground text)
      (set-face-attribute 'lispm-ui-body nil :foreground text)
      (set-face-attribute 'lispm-ui-caption nil :foreground muted)
      (set-face-attribute 'lispm-ui-muted nil :foreground muted :inherit 'unspecified)
      (set-face-attribute 'lispm-ui-success nil :foreground (plist-get tokens :success-fg))
      (set-face-attribute 'lispm-ui-warning nil :foreground (plist-get tokens :warning-fg))
      (set-face-attribute 'lispm-ui-info nil :foreground (plist-get tokens :info-fg))
      (set-face-attribute 'lispm-ui-danger nil :foreground (plist-get tokens :danger-fg))
      (lispm-ui-signal-set lispm-ui-tokens-signal tokens)
      tokens)))

(add-hook 'lispm-ui-palette-changed-hook #'lispm-ui-tokens--sync 20)
(lispm-ui-tokens--sync)

(provide 'lispm-ui-tokens)
;;; lispm-ui-tokens.el ends here
