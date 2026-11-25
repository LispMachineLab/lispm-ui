;;; lispm-ui-rich.el --- Pagination, Breadcrumb, Stepper, Markdown & Diff blocks -*- lexical-binding: t; -*-

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

;; Navigation chrome and rich read-only content formatters:
;;
;; - `lispm-ui-pagination-pages' & `lispm-ui-pagination'.
;; - `lispm-ui-breadcrumb'.
;; - `lispm-ui-wizard-stepper'.
;; - `lispm-ui-markdown' (headings, emphasis, inline code, fences, lists, quotes).
;; - `lispm-ui-diff' (unified diff hunk & +/- line highlighting).

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-ui-base)
(require 'lispm-ui-palette)
(require 'lispm-ui-tokens)
(require 'lispm-ui-icons)
(require 'lispm-ui-layout)
(require 'lispm-ui-controls-extra)
(require 'lispm-ui-feedback)

;;;; Pagination & Breadcrumb

(defun lispm-ui-pagination-pages (current total &optional siblings)
  "Return ordered list of page integers and `ellipsis' symbols for CURRENT/TOTAL."
  (let* ((tot (max 1 total))
         (cur (max 1 (min tot current)))
         (sib (max 0 (or siblings 1))))
    (if (<= tot (+ 5 (* 2 sib)))
        (number-sequence 1 tot)
      (let* ((left (max 2 (- cur sib)))
             (right (min (1- tot) (+ cur sib)))
             (out (list 1)))
        (when (> left 2)
          (setq out (append out '(ellipsis))))
        (setq out (append out (number-sequence left right)))
        (when (< right (1- tot))
          (setq out (append out '(ellipsis))))
        (append out (list tot))))))

(cl-defun lispm-ui-pagination (current total on-page &key (siblings 1))
  "Return interactive pagination control string for CURRENT of TOTAL pages."
  (let* ((tot (max 1 total))
         (cur (max 1 (min tot current)))
         (tokens (lispm-ui-pagination-pages cur tot siblings))
         (make-btn
          (lambda (lbl target active disabled)
            (let ((m (unless (or disabled (not target))
                       (let ((km (make-sparse-keymap)))
                         (keymap-set km "RET" (lambda () (interactive) (funcall on-page target)))
                         (keymap-set km "<down-mouse-1>"
                                     (lambda (_e) (interactive "e") (funcall on-page target)))
                         (keymap-set km "<mouse-1>" #'ignore)
                         km))))
              (propertize (if active (format "[%s]" lbl) (format " %s " lbl))
                          'face (cond (disabled 'shadow)
                                      (active 'lispm-ui-button-pressed)
                                      (t 'lispm-ui-button))
                          'lispm-ui-focusable (not (null m))
                          'keymap m
                          'pointer (when m 'hand))))))
    (string-join
     (append
      (list (funcall make-btn "‹" (1- cur) nil (<= cur 1)))
      (mapcar (lambda (tok)
                (if (eq tok 'ellipsis)
                    (propertize "…" 'face 'lispm-ui-muted)
                  (funcall make-btn (number-to-string tok) tok (= tok cur) nil)))
              tokens)
      (list (funcall make-btn "›" (1+ cur) nil (>= cur tot))))
     " ")))

(cl-defun lispm-ui-breadcrumb (items &key (max-items 6) separator)
  "Render clickable Breadcrumb trail from ITEMS `((:label L :action FN) ...)'."
  (let* ((sep (concat " " (or separator "›") " "))
         (collapsed (if (and max-items (> (length items) max-items) (>= max-items 3))
                        (append (seq-take items 1)
                                (list (list :label "…" :disabled t))
                                (last items (- max-items 2)))
                      items)))
    (string-join
     (cl-loop for item in collapsed for idx from 0
              collect
              (let* ((last-p (= idx (1- (length collapsed))))
                     (label (or (plist-get item :label) (format "%s" item)))
                     (act (unless (or last-p (plist-get item :disabled))
                            (plist-get item :action)))
                     (map (when act
                            (let ((m (make-sparse-keymap)))
                              (keymap-set m "RET" (lambda () (interactive) (funcall act)))
                              (keymap-set m "<down-mouse-1>"
                                          (lambda (_e) (interactive "e") (funcall act)))
                              (keymap-set m "<mouse-1>" #'ignore)
                              m))))
                (propertize label
                            'face (cond (last-p 'lispm-ui-h4)
                                        (act 'lispm-ui-body)
                                        (t 'lispm-ui-muted))
                            'lispm-ui-focusable (not (null map))
                            'keymap map
                            'pointer (when map 'hand))))
     sep)))

;;;; Wizard Stepper

(cl-defun lispm-ui-wizard-stepper (steps &key (orientation 'horizontal) width)
  "Render multi-step Wizard progress indicator from STEPS plists.
Each step uses `:label' and `:status' (`completed' `current' `error' `pending')."
  (ignore width)
  (let ((parts
         (cl-loop for step in steps for idx from 1
                  collect
                  (let* ((status (or (plist-get step :status) 'pending))
                         (label (or (plist-get step :label) (format "Step %d" idx)))
                         (glyph (pcase status
                                  ('completed "✓")
                                  ('current "●")
                                  ('error "×")
                                  (_ "○")))
                         (face (pcase status
                                 ('completed 'lispm-ui-success)
                                 ('current `(:foreground ,(or (lispm-ui-color :accent) "#2563eb")
                                             :weight bold))
                                 ('error 'lispm-ui-danger)
                                 (_ 'lispm-ui-muted))))
                    (concat (propertize (format "%s %d." glyph idx) 'face face)
                            " "
                            (propertize label 'face (if (eq status 'current) 'lispm-ui-h4 face)))))))
    (if (eq orientation 'vertical)
        (lispm-ui-vstack parts :gap 'xs)
      (string-join parts (propertize " ── " 'face 'lispm-ui-muted)))))

;;;; Lightweight Markdown & Unified Diff

(defun lispm-ui-rich--inline-markdown (line)
  "Apply lightweight bold and inline code styling to LINE."
  (let ((s (copy-sequence line)))
    (setq s (replace-regexp-in-string
             "`\\([^`\n]+\\)`"
             (lambda (m) (propertize (match-string 1 m) 'face 'lispm-ui-code))
             s t t))
    (replace-regexp-in-string
     "\\*\\*\\([^*]+\\)\\*\\*"
     (lambda (m) (propertize (match-string 1 m) 'face 'bold))
     s t t)))

(cl-defun lispm-ui-markdown (text &key width)
  "Format lightweight Markdown TEXT for read-only UI buffers."
  (let ((in-code nil)
        (out nil)
        (rule-w (or width 48)))
    (dolist (line (split-string (or text "") "\n"))
      (cond
       ((string-prefix-p "```" line)
        (setq in-code (not in-code)))
       (in-code
        (push (propertize (concat "  " line) 'face 'lispm-ui-code) out))
       ((string-match "\\`\\(#\\{1,3\\}\\) +\\(.*\\)\\'" line)
        (let ((face (pcase (length (match-string 1 line))
                      (1 'lispm-ui-h1) (2 'lispm-ui-h2) (_ 'lispm-ui-h3))))
          (push (propertize (match-string 2 line) 'face face) out)))
       ((string-match-p "\\`---+$" (string-trim line))
        (push (lispm-ui-divider :width rule-w) out))
       ((string-prefix-p "> " line)
        (push (concat (propertize "│ " 'face 'lispm-ui-muted)
                      (propertize (lispm-ui-rich--inline-markdown (substring line 2))
                                  'face 'italic))
              out))
       ((string-match "\\`[-*] +\\(.*\\)\\'" line)
        (push (concat "  • " (lispm-ui-rich--inline-markdown (match-string 1 line))) out))
       (t
        (push (lispm-ui-rich--inline-markdown line) out))))
    (string-join (nreverse out) "\n")))

(defun lispm-ui-diff (text)
  "Render unified diff TEXT with semantic hunk, addition, and deletion styling."
  (let ((add-bg (or (lispm-ui-token :success-bg) "#dcfce7"))
        (add-fg (or (lispm-ui-token :success-on-bg) "#15803d"))
        (del-bg (or (lispm-ui-token :danger-bg) "#fee2e2"))
        (del-fg (or (lispm-ui-token :danger-on-bg) "#b91c1c"))
        (info-fg (or (lispm-ui-token :info-fg) "#2563eb")))
    (mapconcat
     (lambda (line)
       (cond
        ((or (string-prefix-p "+++" line) (string-prefix-p "---" line))
         (propertize line 'face 'lispm-ui-h4))
        ((string-prefix-p "@@" line)
         (propertize line 'face `(:inherit lispm-ui-code :foreground ,info-fg :weight bold)))
        ((string-prefix-p "+" line)
         (propertize line 'face `(:inherit lispm-ui-code :background ,add-bg :foreground ,add-fg :extend t)))
        ((string-prefix-p "-" line)
         (propertize line 'face `(:inherit lispm-ui-code :background ,del-bg :foreground ,del-fg :extend t)))
        (t
         (propertize line 'face 'lispm-ui-code))))
     (split-string (or text "") "\n")
     "\n")))

(provide 'lispm-ui-rich)
;;; lispm-ui-rich.el ends here
