;;; lispm-ui-data.el --- Interactive ListView, DataTable, TreeView, Accordion & KV -*- lexical-binding: t; -*-

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

;; Interactive data display components for GUI and TTY buffers:
;;
;; - `lispm-ui-list-view': virtual-sliced list with single/multi-selection,
;;   keyboard navigation, prefix typeahead, empty state, and loading skeleton.
;; - `lispm-ui-table': adaptive sortable DataTable with custom cell formatting,
;;   alignment, selection, and Look/palette styling.
;; - `lispm-ui-tree': collapsible hierarchical TreeView using `lispm-ui-triangle'.
;; - `lispm-ui-accordion': expandable/exclusive accordion sections.
;; - `lispm-ui-kv': aligned property sheet with copyable and secret fields.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-ui-base)
(require 'lispm-ui-frp)
(require 'lispm-ui-palette)
(require 'lispm-ui-tokens)
(require 'lispm-ui-list)
(require 'lispm-ui-icons)
(require 'lispm-ui-layout)
(require 'lispm-ui-controls-extra)
(require 'lispm-ui-feedback)

;;;; Interactive ListView

(cl-defstruct (lispm-ui-list-view
               (:constructor lispm-ui-list-view--create)
               (:copier nil)
               (:predicate lispm-ui-list-view-p))
  "Stateful virtual-sliced list view supporting selection and typeahead."
  items
  key-fn
  render-item
  (multi-select nil :type boolean)
  (selected-keys nil :type list)
  (cursor 0 :type integer)
  (offset 0 :type integer)
  (page-size 50 :type integer)
  (loading nil :type boolean)
  empty-title
  empty-description
  on-select
  on-activate
  typed-pair)

(cl-defun lispm-ui-list-view-create (&key items key-fn render-item multi-select
                                          (page-size 50) loading empty-title
                                          empty-description on-select on-activate)
  "Create a `lispm-ui-list-view' over ITEMS."
  (lispm-ui-list-view--create
   :items (copy-sequence items)
   :key-fn (or key-fn #'identity)
   :render-item render-item
   :multi-select (and multi-select t)
   :page-size (max 1 page-size)
   :loading (and loading t)
   :empty-title (or empty-title "No items")
   :empty-description empty-description
   :on-select on-select
   :on-activate on-activate))

(defun lispm-ui-list-view-selected (view)
  "Return selected key list in VIEW (or single selected key when single-select)."
  (if (lispm-ui-list-view-multi-select view)
      (copy-sequence (lispm-ui-list-view-selected-keys view))
    (car (lispm-ui-list-view-selected-keys view))))

(defun lispm-ui-list-view-set-items (view items)
  "Replace ITEMS in VIEW cleanly and clamp cursor/offset."
  (setf (lispm-ui-list-view-items view) (copy-sequence items)
        (lispm-ui-list-view-cursor view)
        (min (lispm-ui-list-view-cursor view) (max 0 (1- (length items))))
        (lispm-ui-list-view-offset view)
        (min (lispm-ui-list-view-offset view) (max 0 (1- (length items)))))
  view)

(defun lispm-ui-list-view-set-loading (view flag)
  "Set loading state FLAG on VIEW."
  (setf (lispm-ui-list-view-loading view) (and flag t)))

(defun lispm-ui-list-view-toggle-select (view key &optional append)
  "Select or toggle KEY in VIEW, notifying `:on-select'."
  (let* ((multi (lispm-ui-list-view-multi-select view))
         (cur (lispm-ui-list-view-selected-keys view))
         (next (cond
                ((and multi (or append t) (member key cur))
                 (delete key (copy-sequence cur)))
                (multi
                 (append cur (list key)))
                (t (list key)))))
    (setf (lispm-ui-list-view-selected-keys view) next)
    (when (lispm-ui-list-view-on-select view)
      (funcall (lispm-ui-list-view-on-select view) (lispm-ui-list-view-selected view)))
    next))

(defun lispm-ui-list-view-move (view delta)
  "Move cursor in VIEW by DELTA and adjust virtual slice window."
  (let ((count (length (lispm-ui-list-view-items view))))
    (when (> count 0)
      (let* ((next (max 0 (min (1- count) (+ (lispm-ui-list-view-cursor view) delta))))
             (ps (lispm-ui-list-view-page-size view))
             (off (lispm-ui-list-view-offset view)))
        (setf (lispm-ui-list-view-cursor view) next)
        (cond ((< next off) (setf (lispm-ui-list-view-offset view) next))
              ((>= next (+ off ps)) (setf (lispm-ui-list-view-offset view) (- next ps -1))))
        (let ((item (nth next (lispm-ui-list-view-items view))))
          (unless (lispm-ui-list-view-multi-select view)
            (lispm-ui-list-view-toggle-select
             view (funcall (lispm-ui-list-view-key-fn view) item))))
        next))))

(defun lispm-ui-list-view-typeahead (view char &optional now)
  "Move cursor in VIEW to first item matching incremental prefix CHAR."
  (let* ((pair (lispm-ui-frp-type-select-step
                (lispm-ui-list-view-typed-pair view) (downcase char) 1.0 now))
         (prefix (car pair))
         (kfn (lispm-ui-list-view-key-fn view))
         (idx (seq-position (lispm-ui-list-view-items view) prefix
                            (lambda (item p)
                              (string-prefix-p p (downcase (format "%s" (funcall kfn item))))))))
    (setf (lispm-ui-list-view-typed-pair view) pair)
    (when idx
      (setf (lispm-ui-list-view-cursor view) idx)
      (unless (lispm-ui-list-view-multi-select view)
        (lispm-ui-list-view-toggle-select
         view (funcall kfn (nth idx (lispm-ui-list-view-items view))))))
    idx))

(defun lispm-ui-list-view-render (view)
  "Render visible virtual slice of VIEW (or skeleton/empty-state)."
  (let ((items (lispm-ui-list-view-items view)))
    (cond
     ((lispm-ui-list-view-loading view)
      (lispm-ui-skeleton :lines 4 :width 40))
     ((null items)
      (lispm-ui-empty-state (lispm-ui-list-view-empty-title view)
                            :description (lispm-ui-list-view-empty-description view)
                            :icon 'search))
     (t
      (let* ((off (lispm-ui-list-view-offset view))
             (ps (lispm-ui-list-view-page-size view))
             (slice (seq-take (nthcdr off items) ps))
             (kfn (lispm-ui-list-view-key-fn view))
             (rfn (lispm-ui-list-view-render-item view))
             (sel-list (lispm-ui-list-view-selected-keys view))
             (rows nil))
        (cl-loop for item in slice for idx from off do
                 (let* ((key (funcall kfn item))
                        (sel (and (member key sel-list) t))
                        (cursor-p (= idx (lispm-ui-list-view-cursor view)))
                        (text (cond (rfn (funcall rfn item sel cursor-p))
                                    ((stringp item) item)
                                    (t (format "%s" item))))
                        (map (let ((m (make-sparse-keymap)))
                               (keymap-set m "RET"
                                           (lambda () (interactive)
                                             (when (lispm-ui-list-view-on-activate view)
                                               (funcall (lispm-ui-list-view-on-activate view) item))))
                               (keymap-set m "SPC"
                                           (lambda () (interactive)
                                             (lispm-ui-list-view-toggle-select view key)))
                               (keymap-set m "<down-mouse-1>"
                                           (lambda (ev) (interactive "e")
                                             (if (memq 'double (event-modifiers ev))
                                                 (when (lispm-ui-list-view-on-activate view)
                                                   (funcall (lispm-ui-list-view-on-activate view) item))
                                               (lispm-ui-list-view-toggle-select view key))))
                               (keymap-set m "<mouse-1>" #'ignore)
                               m)))
                   (push (propertize (concat (if cursor-p "▸ " "  ") text)
                                     'lispm-ui-item-key key
                                     'lispm-ui-focusable t
                                     'keymap map
                                     'pointer 'hand
                                     'face (cond
                                            (sel `(:background ,(or (lispm-ui-token :selected-bg) "#2563eb")
                                                   :foreground ,(or (lispm-ui-token :selected-fg) "#ffffff")
                                                   :extend t))
                                            (cursor-p `(:background ,(or (lispm-ui-token :hover) "#eeeeee")
                                                        :extend t))))
                         rows)))
        (lispm-ui-vstack (nreverse rows) :gap 'none))))))

;;;; Interactive DataTable

(defun lispm-ui-table-column-widths (columns total-width)
  "Compute pixel/cell widths for COLUMNS plist list within TOTAL-WIDTH."
  (let* ((gaps (max 0 (1- (length columns))))
         (rem (max (length columns) (- (or total-width 80) gaps)))
         (fixed 0)
         (flex 0))
    (dolist (col columns)
      (if-let* ((w (plist-get col :width)))
          (setq fixed (+ fixed w))
        (setq flex (1+ flex))))
    (let* ((avail (max 0 (- rem fixed)))
           (per (if (> flex 0) (max 4 (/ avail flex)) 0)))
      (mapcar (lambda (col) (or (plist-get col :width) per)) columns))))

(cl-defstruct (lispm-ui-table
               (:constructor lispm-ui-table--create)
               (:copier nil)
               (:predicate lispm-ui-table-p))
  columns
  rows
  key-fn
  sort-key
  (descending nil :type boolean)
  (multi-select nil :type boolean)
  selected-keys
  on-select
  on-activate
  on-sort)

(cl-defun lispm-ui-table-create (columns &key rows key-fn sort-key descending
                                         multi-select on-select on-activate on-sort)
  "Create a sortable `lispm-ui-table' with COLUMNS specification."
  (lispm-ui-table--create
   :columns columns
   :rows (copy-sequence rows)
   :key-fn (or key-fn (lambda (row) (or (plist-get row :id) (car-safe row) row)))
   :sort-key sort-key
   :descending (and descending t)
   :multi-select (and multi-select t)
   :on-select on-select
   :on-activate on-activate
   :on-sort on-sort))

(defun lispm-ui-table--cell-val (row key)
  "Extract column KEY value from alist or plist ROW."
  (cond
   ((and (listp row) (keywordp (car row)))
    (or (plist-get row key)
        (and (symbolp key) (not (keywordp key))
             (plist-get row (intern (concat ":" (symbol-name key)))))))
   ((consp row)
    (or (cdr (assoc key row))
        (and (keywordp key)
             (cdr (assoc (intern (substring (symbol-name key) 1)) row)))))
   (t row)))

(defun lispm-ui-table-sort (table col-key)
  "Sort TABLE by COL-KEY (toggling direction on repeat) without mutating source."
  (let* ((col (seq-find (lambda (c) (equal (plist-get c :key) col-key))
                        (lispm-ui-table-columns table)))
         (desc (if (equal (lispm-ui-table-sort-key table) col-key)
                   (not (lispm-ui-table-descending table))
                 nil))
         (cmp (or (plist-get col :comparator)
                  (lambda (a b)
                    (let ((va (lispm-ui-table--cell-val a col-key))
                          (vb (lispm-ui-table--cell-val b col-key)))
                      (if (and (numberp va) (numberp vb))
                          (< va vb)
                        (string-lessp (format "%s" (or va "")) (format "%s" (or vb "")))))))))
    (setf (lispm-ui-table-sort-key table) col-key
          (lispm-ui-table-descending table) desc)
    (let ((sorted (sort (copy-sequence (lispm-ui-table-rows table)) cmp)))
      (when desc (setq sorted (nreverse sorted)))
      (setf (lispm-ui-table-rows table) sorted))
    (when (lispm-ui-table-on-sort table)
      (funcall (lispm-ui-table-on-sort table) col-key desc))
    (lispm-ui-table-rows table)))

(defun lispm-ui-table--format-cell (text width align)
  "Format TEXT to WIDTH columns honoring ALIGN (`left', `right', `center')."
  (let* ((fit (truncate-string-to-width (or text "") width nil nil "…"))
         (pad (max 0 (- width (string-width fit)))))
    (pcase align
      ('right (concat (make-string pad ?\s) fit))
      ('center (let ((l (/ pad 2)))
                 (concat (make-string l ?\s) fit (make-string (- pad l) ?\s))))
      (_ (concat fit (make-string pad ?\s))))))

(defun lispm-ui-table--header-row (table widths)
  "Render clickable sortable header row for TABLE using WIDTHS."
  (let ((cols (lispm-ui-table-columns table))
        (skey (lispm-ui-table-sort-key table))
        (desc (lispm-ui-table-descending table)))
    (string-join
     (cl-mapcar
      (lambda (col w)
        (let* ((key (plist-get col :key))
               (title (or (plist-get col :title) (format "%s" key)))
               (ind (when (equal key skey) (if desc " ↓" " ↑")))
               (cell (lispm-ui-table--format-cell (concat title (or ind ""))
                                                  w (plist-get col :align)))
               (sortable (or (not (plist-member col :sortable))
                             (plist-get col :sortable)))
               (map (when sortable
                      (let ((m (make-sparse-keymap)))
                        (keymap-set m "RET" (lambda () (interactive) (lispm-ui-table-sort table key)))
                        (keymap-set m "<down-mouse-1>"
                                    (lambda (_e) (interactive "e") (lispm-ui-table-sort table key)))
                        (keymap-set m "<mouse-1>" #'ignore)
                        m))))
          (propertize cell
                      'face 'lispm-ui-h4
                      'keymap map
                      'lispm-ui-table-header key
                      'pointer (when sortable 'hand))))
      cols widths)
     " ")))

(defun lispm-ui-table-render (table &optional width)
  "Render TABLE headers and rows cleanly within WIDTH columns."
  (let* ((w (or width 72))
         (cols (lispm-ui-table-columns table))
         (widths (lispm-ui-table-column-widths cols w))
         (header (lispm-ui-table--header-row table widths))
         (rule (lispm-ui-divider :width w))
         (kfn (lispm-ui-table-key-fn table))
         (sel-keys (lispm-ui-table-selected-keys table))
         (body
          (if (null (lispm-ui-table-rows table))
              (list (propertize "  (empty)" 'face 'lispm-ui-muted))
            (mapcar
             (lambda (row)
               (let* ((rkey (funcall kfn row))
                      (sel (and (member rkey sel-keys) t))
                      (line
                       (string-join
                        (cl-mapcar
                         (lambda (col cw)
                           (let* ((ck (plist-get col :key))
                                  (raw (lispm-ui-table--cell-val row ck))
                                  (rfn (plist-get col :render))
                                  (txt (if rfn (funcall rfn raw row) (format "%s" (or raw "")))))
                             (lispm-ui-table--format-cell txt cw (plist-get col :align))))
                         cols widths)
                        " "))
                      (map (let ((m (make-sparse-keymap)))
                             (keymap-set m "RET"
                                         (lambda () (interactive)
                                           (when (lispm-ui-table-on-activate table)
                                             (funcall (lispm-ui-table-on-activate table) row))))
                             (keymap-set m "<down-mouse-1>"
                                         (lambda (_e) (interactive "e")
                                           (setf (lispm-ui-table-selected-keys table) (list rkey))
                                           (when (lispm-ui-table-on-select table)
                                             (funcall (lispm-ui-table-on-select table) row))))
                             (keymap-set m "<mouse-1>" #'ignore)
                             m)))
                 (propertize line
                             'lispm-ui-item-key rkey
                             'lispm-ui-focusable t
                             'keymap map
                             'face (when sel
                                     `(:background ,(or (lispm-ui-token :selected-bg) "#2563eb")
                                       :foreground ,(or (lispm-ui-token :selected-fg) "#ffffff")
                                       :extend t)))))
             (lispm-ui-table-rows table)))))
    (lispm-ui-vstack (cons header (cons rule body)) :gap 'none)))

;;;; TreeView, Accordion & PropertySheet (KV)

(defun lispm-ui-tree-flatten (nodes expanded-keys &optional depth)
  "Flatten nested tree NODES filtering collapsed subtrees by EXPANDED-KEYS."
  (let ((d (or depth 0))
        (out nil))
    (dolist (node nodes)
      (let* ((key (plist-get node :key))
             (children (plist-get node :children))
             (has-ch (and children t))
             (exp (and has-ch (member key expanded-keys) t)))
        (push (list :node node :key key :depth d :has-children has-ch :expanded exp) out)
        (when exp
          (dolist (child-row (lispm-ui-tree-flatten children expanded-keys (1+ d)))
            (push child-row out)))))
    (nreverse out)))

(defun lispm-ui-tree-toggle (expanded-keys key)
  "Return updated EXPANDED-KEYS list toggling KEY non-destructively."
  (if (member key expanded-keys)
      (delete key (copy-sequence expanded-keys))
    (cons key (copy-sequence expanded-keys))))

(defun lispm-ui-tree--node-keymap (key has-ch on-toggle on-select)
  "Return keymap binding toggle and select actions for tree node KEY."
  (let ((m (make-sparse-keymap)))
    (keymap-set m "TAB" (lambda () (interactive) (when (and has-ch on-toggle) (funcall on-toggle key))))
    (keymap-set m "RET" (lambda () (interactive) (when on-select (funcall on-select key))))
    (keymap-set m "<down-mouse-1>"
                (lambda (_e) (interactive "e")
                  (when (and has-ch on-toggle) (funcall on-toggle key))
                  (when on-select (funcall on-select key))))
    (keymap-set m "<mouse-1>" #'ignore)
    m))

(defun lispm-ui-data--disclosure (open)
  "Return disclosure triangle or TTY fallback glyph for OPEN state."
  (if (and (not (lispm-ui-tty-p)) lispm-ui-current-palette)
      (lispm-ui-triangle open)
    (if open "▾" "▸")))

(cl-defun lispm-ui-tree-render (nodes expanded-keys &key selected-key on-toggle on-select)
  "Render hierarchical NODES expanded at EXPANDED-KEYS."
  (let ((flat (lispm-ui-tree-flatten nodes expanded-keys 0)))
    (lispm-ui-vstack
     (mapcar
      (lambda (row)
        (let* ((node (plist-get row :node))
               (key (plist-get row :key))
               (depth (plist-get row :depth))
               (has-ch (plist-get row :has-children))
               (exp (plist-get row :expanded))
               (sel (and selected-key (equal key selected-key)))
               (indent (make-string (* 2 depth) ?\s))
               (tri (if has-ch (concat (lispm-ui-data--disclosure exp) " ") "  "))
               (ic (when-let* ((icon (plist-get node :icon)))
                     (concat (lispm-ui-icon icon) " ")))
               (label (or (plist-get node :label) (format "%s" key))))
          (propertize (concat indent tri (or ic "") label)
                      'lispm-ui-item-key key
                      'lispm-ui-focusable t
                      'keymap (lispm-ui-tree--node-keymap key has-ch on-toggle on-select)
                      'pointer 'hand
                      'face (when sel
                              `(:background ,(or (lispm-ui-token :selected-bg) "#2563eb")
                                :foreground ,(or (lispm-ui-token :selected-fg) "#ffffff")
                                :extend t)))))
      flat)
     :gap 'none)))

(defun lispm-ui-accordion-toggle (open-keys key &optional exclusive)
  "Return updated OPEN-KEYS list toggling KEY (single section when EXCLUSIVE)."
  (cond
   ((member key open-keys)
    (delete key (copy-sequence open-keys)))
   (exclusive (list key))
   (t (cons key (copy-sequence open-keys)))))

(cl-defun lispm-ui-accordion-render (sections open-keys &key exclusive on-toggle width)
  "Render collapsible accordion SECTIONS (`(:key K :title T :body B)')."
  (ignore exclusive)
  (lispm-ui-vstack
   (mapcar
    (lambda (sec)
      (let* ((key (plist-get sec :key))
             (title (or (plist-get sec :title) ""))
             (body (plist-get sec :body))
             (open (and (member key open-keys) t))
             (map (when on-toggle
                    (let ((m (make-sparse-keymap)))
                      (keymap-set m "RET" (lambda () (interactive) (funcall on-toggle key)))
                      (keymap-set m "<down-mouse-1>"
                                  (lambda (_e) (interactive "e") (funcall on-toggle key)))
                      (keymap-set m "<mouse-1>" #'ignore)
                      m)))
             (head (propertize (concat (lispm-ui-data--disclosure open) " " title)
                               'face 'lispm-ui-h4
                               'lispm-ui-item-key key
                               'lispm-ui-focusable t
                               'pointer (when on-toggle 'hand)
                               'keymap map)))
        (if open
            (lispm-ui-card (if (functionp body) (funcall body) (or body ""))
                           :title head :width (or width 56))
          head)))
    sections)
   :gap 'xs))

(cl-defun lispm-ui-kv-render (entries &key (label-width 18) width revealed-keys
                                      on-toggle-secret on-copy)
  "Render ENTRIES property sheet with copyable and secret masking controls.
Each entry is `(:key K :label L :value V :copyable B :secret B)'."
  (ignore width)
  (lispm-ui-vstack
   (mapcar
    (lambda (entry)
      (let* ((key (plist-get entry :key))
             (label (or (plist-get entry :label) (format "%s" key)))
             (raw (format "%s" (or (plist-get entry :value) "")))
             (secret (plist-get entry :secret))
             (shown (or (not secret) (member key revealed-keys)))
             (display-val (if shown raw "••••••••"))
             (pad-label (lispm-ui-table--format-cell label label-width 'right))
             (eye-btn (when (and secret on-toggle-secret)
                        (let ((m (make-sparse-keymap)))
                          (keymap-set m "<down-mouse-1>"
                                      (lambda (_e) (interactive "e") (funcall on-toggle-secret key)))
                          (keymap-set m "<mouse-1>" #'ignore)
                          (propertize (concat " " (lispm-ui-icon (if shown 'eye-off 'eye)))
                                      'keymap m 'pointer 'hand))))
             (copy-btn (when (plist-get entry :copyable)
                         (let ((m (make-sparse-keymap)))
                           (keymap-set m "<down-mouse-1>"
                                       (lambda (_e) (interactive "e")
                                         (kill-new raw)
                                         (when on-copy (funcall on-copy raw))))
                           (keymap-set m "<mouse-1>" #'ignore)
                           (propertize (concat " " (lispm-ui-icon 'copy))
                                       'keymap m 'pointer 'hand)))))
        (concat (propertize pad-label 'face 'lispm-ui-muted)
                ": "
                (propertize display-val 'face (if secret 'lispm-ui-code 'lispm-ui-body))
                (or eye-btn "")
                (or copy-btn ""))))
    entries)
   :gap 'xs))

(provide 'lispm-ui-data)
;;; lispm-ui-data.el ends here
