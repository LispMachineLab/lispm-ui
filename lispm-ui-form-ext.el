;;; lispm-ui-form-ext.el --- Extensible Form 2.0 field types and state -*- lexical-binding: t; -*-

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

;; Open Form 2.0 runtime layered cleanly atop `lispm-ui-form.el':
;; - Extensible field-type registry via `lispm-ui-form-define-type'
;; - Nine built-in modern field types: `switch', `slider', `number',
;;   `file', `combobox', `tags', `color', `date', and `repeater'
;; - Inline per-field errors, cross-field `:validate-form', async
;;   `:validate-async' with stale-token cancellation, dirty/touched
;;   tracking, `lispm-ui-form-reset', and collapsible group headings.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'widget)
(require 'wid-edit)
(require 'lispm-ui-base)
(require 'lispm-ui-i18n)
(require 'lispm-ui-frp)
(require 'lispm-ui-tokens)
(require 'lispm-ui-controls-extra)
(require 'lispm-ui-input)

(declare-function lispm-ui-form--at "lispm-ui-form" (position &optional width face))
(declare-function lispm-ui-form--remember "lispm-ui-form" (key widget))
(declare-function lispm-ui-form--changed "lispm-ui-form" (key value &optional redraw))
(declare-function lispm-ui-form--all-values "lispm-ui-form" ())
(declare-function lispm-ui-form--shown-p "lispm-ui-form" (field))
(declare-function lispm-ui-form--open-group "lispm-ui-form" (title))
(declare-function lispm-ui-form-value "lispm-ui-form" (key))
(declare-function lispm-ui-form-redraw "lispm-ui-form" ())

(defvar lispm-ui-form--fields)
(defvar lispm-ui-form--values)
(defvar lispm-ui-form--signals)
(defvar lispm-ui-form--props)
(defvar lispm-ui-form--widgets)
(defvar lispm-ui-form-choose-file-function)

;;;; Open Field-Type Registry

(defvar lispm-ui-form--type-registry (make-hash-table :test #'eq)
  "Registry mapping custom form field type symbols to handler plists.")

;;;###autoload
(cl-defun lispm-ui-form-define-type (type &key render normalize validate default)
  "Register Form 2.0 field TYPE with RENDER, NORMALIZE, VALIDATE, and DEFAULT.
RENDER is called with `(KEY FIELD PROPS VALUE LABEL-FN ROW-FN)'.
NORMALIZE is called with `(RAW PROPS)' -> normalized value.
VALIDATE is called with `(VALUE PROPS)' -> error string or nil."
  (declare (indent 1))
  (puthash type (list :render render :normalize normalize
                      :validate validate :default default)
           lispm-ui-form--type-registry)
  type)

(defun lispm-ui-form-type-spec (type)
  "Return the registered handler plist for form field TYPE, or nil."
  (gethash type lispm-ui-form--type-registry))

;;;; Form 2.0 Buffer State

(defvar-local lispm-ui-form--initial-values nil
  "Deep-copied initial `(KEY . VALUE)' alist captured for dirty/reset tracking.")

(defvar-local lispm-ui-form--touched nil
  "List of field keys modified since form creation or last reset.")

(defvar-local lispm-ui-form--errors nil
  "Alist `(KEY . MSG)' of current validation errors (KEY nil = form-level).")

(defvar-local lispm-ui-form--async-pending nil
  "List of field keys currently running `:validate-async'.")

(defvar-local lispm-ui-form--async-seq nil
  "Alist `(KEY . SEQ)' preventing stale async validation callbacks.")

(defvar-local lispm-ui-form--collapsed-groups nil
  "List of collapsible heading keys currently collapsed.")

(defvar-local lispm-ui-form--ext-handles nil
  "Embedded UI handles (`lispm-ui-input', etc.) owned by the current draw.")

(defun lispm-ui-form--ext-clear-handles ()
  "Dispose embedded UI handles from the previous draw of this form."
  (dolist (h lispm-ui-form--ext-handles)
    (cond
     ((lispm-ui-combobox-p h) (lispm-ui-combobox-dispose h))
     ((lispm-ui-tags-p h) (lispm-ui-tags-dispose h))
     ((lispm-ui-input-p h) (lispm-ui-input-dispose h))))
  (setq lispm-ui-form--ext-handles nil))

(defun lispm-ui-form--ext-init (fields values)
  "Initialize Form 2.0 tracking state for FIELDS and initial VALUES."
  (lispm-ui-form--ext-clear-handles)
  (add-hook 'kill-buffer-hook #'lispm-ui-form--ext-clear-handles nil t)
  (setq lispm-ui-form--initial-values (copy-tree values)
        lispm-ui-form--touched nil
        lispm-ui-form--errors nil
        lispm-ui-form--async-pending nil
        lispm-ui-form--async-seq nil
        lispm-ui-form--collapsed-groups
        (cl-loop for f in fields
                 when (and (eq (plist-get (cdr f) :type) 'heading)
                           (plist-get (cdr f) :collapsed))
                 collect (car f))))

(defun lispm-ui-form--ext-default (props)
  "Resolve default value from field PROPS or its registered type."
  (cond
   ((plist-member props :value) (plist-get props :value))
   ((when-let* ((spec (lispm-ui-form-type-spec (plist-get props :type)))
                (def (plist-get spec :default)))
      (if (functionp def) (funcall def props) def)))))

(defun lispm-ui-form--ext-normalize (type props raw)
  "Normalize RAW via registered handler for TYPE and PROPS, or `no-handler'."
  (if-let* ((spec (lispm-ui-form-type-spec type))
            (fn (plist-get spec :normalize)))
      (funcall fn raw props)
    'no-handler))

;;;; Dirty / Touched / Errors / Reset / Sections

(defun lispm-ui-form-dirty-p (&optional key buffer)
  "Return non-nil if field KEY (or any field) in BUFFER differs from initial."
  (with-current-buffer (or buffer (current-buffer))
    (if key
        (not (equal (alist-get key lispm-ui-form--values)
                    (alist-get key lispm-ui-form--initial-values)))
      (cl-some (lambda (pair)
                 (not (equal (alist-get (car pair) lispm-ui-form--values)
                             (cdr pair))))
               lispm-ui-form--initial-values))))

(defun lispm-ui-form-touched-p (key &optional buffer)
  "Return non-nil when field KEY in BUFFER has been modified."
  (with-current-buffer (or buffer (current-buffer))
    (and (memq key lispm-ui-form--touched) t)))

(defun lispm-ui-form-errors (&optional buffer)
  "Return current validation errors alist `(KEY . MSG)' in BUFFER."
  (buffer-local-value 'lispm-ui-form--errors (or buffer (current-buffer))))

(defun lispm-ui-form-toggle-section (section-key &optional buffer)
  "Toggle collapsed state of collapsible heading SECTION-KEY in BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (if (memq section-key lispm-ui-form--collapsed-groups)
        (setq lispm-ui-form--collapsed-groups
              (delq section-key lispm-ui-form--collapsed-groups))
      (push section-key lispm-ui-form--collapsed-groups))
    (lispm-ui-form-redraw)))

(defun lispm-ui-form-section-collapsed-p (section-key &optional buffer)
  "Return non-nil when heading SECTION-KEY is collapsed in BUFFER."
  (and (memq section-key
             (buffer-local-value 'lispm-ui-form--collapsed-groups
                                 (or buffer (current-buffer))))
       t))

(defun lispm-ui-form-reset (&optional buffer)
  "Reset all form values in BUFFER to their initial values and redraw."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (setq lispm-ui-form--values (copy-tree lispm-ui-form--initial-values)
          lispm-ui-form--touched nil
          lispm-ui-form--errors nil
          lispm-ui-form--async-pending nil
          lispm-ui-form--async-seq
          (mapcar (pcase-lambda (`(,k . ,n)) (cons k (1+ n)))
                  lispm-ui-form--async-seq))
    (lispm-ui-frp-batch
      (dolist (pair lispm-ui-form--signals)
        (lispm-ui-signal-set (cdr pair)
                             (alist-get (car pair) lispm-ui-form--values))))
    (lispm-ui-form-redraw)))

;;;; Validation (sync + cross-field + async)

(defun lispm-ui-form--ext-field-error (field)
  "Validate single FIELD via `:required', type `:validate', and `:validate'."
  (let* ((key (car field))
         (props (cdr field))
         (val (lispm-ui-form-value key))
         (spec (lispm-ui-form-type-spec (plist-get props :type)))
         (type-val (plist-get spec :validate))
         (field-val (plist-get props :validate))
         (lbl (or (plist-get props :label) key)))
    (cond
     ((and (plist-get props :required) (null val))
      (lispm-ui-tr "%s: fill this in" lbl))
     ((when-let* ((msg (and type-val (funcall type-val val props))))
        (format "%s: %s" lbl msg)))
     ((when-let* ((msg (and field-val (funcall field-val val))))
        (format "%s: %s" lbl msg))))))

(defun lispm-ui-form--ext-apply-form-validator (errors)
  "Run `:validate-form' callback and append any returned errors to ERRORS."
  (when-let* ((fn (plist-get lispm-ui-form--props :validate-form))
              (res (funcall fn (lispm-ui-form--all-values))))
    (cond
     ((stringp res)
      (unless (string-empty-p res)
        (setq errors (append errors (list (cons nil res))))))
     ((listp res)
      (dolist (entry res)
        (when (and (consp entry) (stringp (cdr entry)))
          (setq errors (append errors (list entry))))))))
  errors)

(defun lispm-ui-form-validate-all (&optional buffer)
  "Validate all shown fields and `:validate-form' in BUFFER; return t if valid."
  (with-current-buffer (or buffer (current-buffer))
    (let ((errs nil))
      (dolist (field (seq-filter #'lispm-ui-form--shown-p lispm-ui-form--fields))
        (when-let* ((msg (lispm-ui-form--ext-field-error field)))
          (push (cons (car field) msg) errs)))
      (setq errs (lispm-ui-form--ext-apply-form-validator (nreverse errs))
            lispm-ui-form--errors errs)
      (null errs))))

(defun lispm-ui-form-validate-async (key &optional buffer)
  "Invoke `:validate-async' for field KEY in BUFFER with stale guard."
  (with-current-buffer (or buffer (current-buffer))
    (when-let* ((props (cdr (assq key lispm-ui-form--fields)))
                (async-fn (plist-get props :validate-async)))
      (let* ((buf (current-buffer))
             (seq (1+ (or (alist-get key lispm-ui-form--async-seq) 0)))
             (val (lispm-ui-form-value key)))
        (setf (alist-get key lispm-ui-form--async-seq) seq)
        (cl-pushnew key lispm-ui-form--async-pending)
        (funcall async-fn val
                 (lambda (msg)
                   (when (buffer-live-p buf)
                     (with-current-buffer buf
                       (when (eql seq (alist-get key lispm-ui-form--async-seq))
                         (setq lispm-ui-form--async-pending
                               (delq key lispm-ui-form--async-pending)
                               lispm-ui-form--errors
                               (assq-delete-all key lispm-ui-form--errors))
                         (when (and (stringp msg) (not (string-empty-p msg)))
                           (push (cons key msg) lispm-ui-form--errors))
                         (lispm-ui-form-redraw))))))))))

(defun lispm-ui-form--sync-error-overlays ()
  "Display current `lispm-ui-form--errors' via inline overlays without redrawing."
  (remove-overlays (point-min) (point-max) 'lispm-ui-form-error t)
  (dolist (pair lispm-ui-form--errors)
    (let ((key (car pair))
          (msg (cdr pair)))
      (if key
          (when-let* ((widget (alist-get key lispm-ui-form--widgets))
                      (pos (or (widget-get widget :to)
                               (widget-field-end widget)
                               (widget-get widget :from))))
            (save-excursion
              (goto-char (min (point-max) pos))
              (let ((ov (make-overlay (line-end-position) (line-end-position))))
                (overlay-put ov 'lispm-ui-form-error t)
                (overlay-put ov 'after-string
                             (concat "\n  " (propertize msg 'face 'lispm-ui-danger))))))
        (save-excursion
          (goto-char (point-max))
          (forward-line -1)
          (let ((ov (make-overlay (line-beginning-position) (line-beginning-position))))
            (overlay-put ov 'lispm-ui-form-error t)
            (overlay-put ov 'before-string
                         (concat " " (propertize msg 'face 'lispm-ui-danger) "\n"))))))))

(defun lispm-ui-form--ext-on-change (key)
  "Record touched state for KEY, clear prior error, and trigger async check."
  (cl-pushnew key lispm-ui-form--touched)
  (when (assq key lispm-ui-form--errors)
    (setq lispm-ui-form--errors (assq-delete-all key lispm-ui-form--errors))
    (lispm-ui-form--sync-error-overlays))
  (lispm-ui-form-validate-async key))

;;;; Inline Error & Text Decoration Rendering

(defun lispm-ui-form--insert-inline-error (key start row-fn)
  "Insert pending indicator or inline error for KEY at column START via ROW-FN."
  (let ((pending (memq key lispm-ui-form--async-pending))
        (err (alist-get key lispm-ui-form--errors)))
    (cond
     (pending
      (funcall row-fn
               (lambda ()
                 (insert (lispm-ui-form--at start)
                         (propertize "Validating…" 'face 'lispm-ui-muted)))))
     (err
      (funcall row-fn
               (lambda ()
                 (insert (lispm-ui-form--at start)
                         (propertize err 'face 'lispm-ui-danger))))))))

(defun lispm-ui-form--insert-form-errors ()
  "Insert form-level validation errors before the bottom button row."
  (dolist (pair lispm-ui-form--errors)
    (when (null (car pair))
      (insert (lispm-ui-form--at :left)
              (propertize (cdr pair) 'face 'lispm-ui-danger)
              "\n"))))

(defun lispm-ui-form--apply-text-props (widget props value)
  "Apply optional Form 2.0 `:placeholder', `:disabled', `:read-only' to WIDGET."
  (let ((ph (plist-get props :placeholder))
        (ro (or (plist-get props :read-only) (plist-get props :disabled))))
    (when (or ph ro)
      (let* ((start (widget-field-start widget))
             (end (widget-field-end widget))
             (ov (widget-get widget :placeholder-overlay)))
        (when (and start end)
          (if (and ph (string-empty-p (or value "")))
              (progn
                (unless (overlayp ov)
                  (setq ov (make-overlay start start nil nil t))
                  (widget-put widget :placeholder-overlay ov))
                (move-overlay ov start start)
                (overlay-put ov 'before-string
                             (propertize ph 'face 'lispm-ui-muted 'cursor t)))
            (when (overlayp ov)
              (overlay-put ov 'before-string nil)))
          (when ro
            (widget-put widget :lispm-read-only t)
            (add-text-properties start end
                                 `(read-only t front-sticky t
                                             ,@(when (plist-get props :disabled)
                                                 '(face shadow))))))))))

(defun lispm-ui-form--open-collapsible-group (key title collapsed)
  "Render clickable collapsible section header TITLE for KEY in COLLAPSED state."
  (let ((map (let ((m (make-sparse-keymap)))
               (keymap-set m "RET" (lambda () (interactive) (lispm-ui-form-toggle-section key)))
               (keymap-set m "<down-mouse-1>"
                           (lambda (_e) (interactive "e") (lispm-ui-form-toggle-section key)))
               (keymap-set m "<mouse-1>" #'ignore)
               m)))
    (if collapsed
        (insert "\n" (lispm-ui-form--at :left)
                (propertize (concat "▸ " (or title ""))
                            'face 'lispm-ui-form-title 'keymap map 'pointer 'hand
                            'lispm-ui-form-heading title 'lispm-ui-form-section key)
                "\n")
      (let ((start (point)))
        (lispm-ui-form--open-group (concat "▾ " (or title "")))
        (add-text-properties start (point)
                             (list 'keymap map 'pointer 'hand
                                   'lispm-ui-form-section key))))))

;;;; Built-in Form 2.0 Field Types

(defun lispm-ui-form--anchor-widget (key value)
  "Insert an invisible anchor item widget for non-text field KEY with VALUE."
  (lispm-ui-form--remember
   key (widget-create 'item :format "" :value value)))

(defun lispm-ui-form--simple-field (key label-fn row-fn value inserter)
  "Render a Form 2.0 field KEY row via INSERTER inside ROW-FN."
  (funcall
   row-fn
   (lambda ()
     (funcall label-fn)
     (insert (lispm-ui-form--at :control))
     (lispm-ui-form--anchor-widget key value)
     (funcall inserter))))

(defun lispm-ui-form--valid-date-p (str)
  "Return non-nil when STR is a valid `YYYY-MM-DD' calendar date."
  (when (string-match "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)\\'" str)
    (let ((y (string-to-number (match-string 1 str)))
          (m (string-to-number (match-string 2 str)))
          (d (string-to-number (match-string 3 str))))
      (and (<= 1 m 12)
           (<= 1 d (pcase m
                     (2 (if (date-leap-year-p y) 29 28))
                     ((or 4 6 9 11) 30)
                     (_ 31)))))))

(lispm-ui-form-define-type 'switch
  :render
  (lambda (key _field props value label-fn row-fn)
    (let ((ro (or (plist-get props :disabled) (plist-get props :read-only))))
      (lispm-ui-form--simple-field
       key label-fn row-fn value
       (lambda ()
         (lispm-ui-insert-switch
          (or (plist-get props :text) "")
          value
          (lambda (new-v) (lispm-ui-form--changed key new-v t))
          :disabled ro
          :help (plist-get props :help)))))))

(lispm-ui-form-define-type 'slider
  :render
  (lambda (key _field props value label-fn row-fn)
    (let ((min (or (plist-get props :min) 0))
          (max (or (plist-get props :max) 100))
          (step (or (plist-get props :step) 1))
          (ro (or (plist-get props :disabled) (plist-get props :read-only))))
      (lispm-ui-form--simple-field
       key label-fn row-fn value
       (lambda ()
         (lispm-ui-insert-slider
          (or value min)
          (lambda (new-v) (lispm-ui-form--changed key new-v t))
          :min min :max max :step step
          :width (or (plist-get props :width) 140)
          :disabled ro))))))

(lispm-ui-form-define-type 'number
  :normalize
  (lambda (raw _props)
    (cond
     ((numberp raw) raw)
     ((and (stringp raw) (string-match-p "\\`-?[0-9]+\\(?:\\.[0-9]+\\)?\\'" (string-trim raw)))
      (string-to-number (string-trim raw)))
     (t raw)))
  :validate
  (lambda (val props)
    (cond
     ((and val (not (numberp val)))
      (lispm-ui-tr "Enter a valid number"))
     ((numberp val)
      (let ((min (plist-get props :min))
            (max (plist-get props :max)))
        (cond
         ((and min (< val min)) (format "Must be >= %s" min))
         ((and max (> val max)) (format "Must be <= %s" max)))))))
  :render
  (lambda (key _field props value label-fn row-fn)
    (let ((min (or (plist-get props :min) 0))
          (max (or (plist-get props :max) 100))
          (step (or (plist-get props :step) 1))
          (ro (or (plist-get props :disabled) (plist-get props :read-only))))
      (lispm-ui-form--simple-field
       key label-fn row-fn value
       (lambda ()
         (lispm-ui-insert-stepper
          (or value min)
          (lambda (new-v) (lispm-ui-form--changed key new-v t))
          :min min :max max :step step :disabled ro))))))

(lispm-ui-form-define-type 'file
  :render
  (lambda (key _field props value label-fn row-fn)
    (let ((ro (or (plist-get props :disabled) (plist-get props :read-only)))
          (pred (plist-get props :predicate))
          (lbl (or (plist-get props :label) (symbol-name key))))
      (lispm-ui-form--simple-field
       key label-fn row-fn value
       (lambda ()
         (insert (propertize (or value (plist-get props :placeholder) "None")
                             'face (if value 'default 'lispm-ui-muted))
                 "  ")
         (unless ro
           (lispm-ui-insert-button
            (lispm-ui-tr "Choose…")
            (lambda ()
              (let ((picked (if (functionp lispm-ui-form-choose-file-function)
                                (funcall lispm-ui-form-choose-file-function
                                         (if (plist-get props :directory) 'directory 'file)
                                         lbl value pred)
                              (read-file-name (format "%s: " lbl) nil value t))))
                (when picked
                  (lispm-ui-form--changed key picked t))))
            :variant 'secondary)
           (when value
             (insert " ")
             (lispm-ui-insert-button
              (lispm-ui-tr "Remove")
              (lambda () (lispm-ui-form--changed key nil t))
              :variant 'ghost))))))))

(lispm-ui-form-define-type 'combobox
  :render
  (lambda (key _field props value label-fn row-fn)
    (let ((cands (or (plist-get props :candidates) (plist-get props :choices)))
          (ro (or (plist-get props :disabled) (plist-get props :read-only))))
      (lispm-ui-form--simple-field
       key label-fn row-fn value
       (lambda ()
         (let ((h (lispm-ui-insert-combobox
                   cands
                   :value value
                   :placeholder (plist-get props :placeholder)
                   :filter-fn (plist-get props :filter-fn)
                   :disabled ro
                   :on-select (lambda (k _l) (lispm-ui-form--changed key k t)))))
           (push h lispm-ui-form--ext-handles)))))))

(lispm-ui-form-define-type 'tags
  :render
  (lambda (key _field props value label-fn row-fn)
    (let ((ro (or (plist-get props :disabled) (plist-get props :read-only)))
          (allow (if (plist-member props :allow-custom)
                     (plist-get props :allow-custom)
                   t)))
      (lispm-ui-form--simple-field
       key label-fn row-fn value
       (lambda ()
         (let ((h (lispm-ui-insert-tags
                   (or value nil)
                   :placeholder (plist-get props :placeholder)
                   :suggestions (plist-get props :suggestions)
                   :allow-custom allow
                   :disabled ro
                   :on-change (lambda (vals) (lispm-ui-form--changed key vals t)))))
           (push h lispm-ui-form--ext-handles)))))))

(lispm-ui-form-define-type 'color
  :validate
  (lambda (val _props)
    (when (and val (not (string-match-p
                         "\\`#[0-9a-fA-F]\\{3\\}\\(?:[0-9a-fA-F]\\{3\\}\\)?\\'"
                         val)))
      "Enter a #RGB or #RRGGBB hex color"))
  :render
  (lambda (key _field props value label-fn row-fn)
    (let ((ro (or (plist-get props :disabled) (plist-get props :read-only)))
          (col (or value "#2563eb")))
      (lispm-ui-form--simple-field
       key label-fn row-fn value
       (lambda ()
         (insert (propertize "  " 'face `(:background ,col)) " ")
         (if ro
             (insert (propertize (or value "") 'face 'shadow))
           (let ((h (lispm-ui-insert-input
                     :value (or value "")
                     :placeholder (or (plist-get props :placeholder) "#RRGGBB")
                     :on-change (lambda (v) (lispm-ui-form--changed key v nil)))))
             (push h lispm-ui-form--ext-handles))))))))

(lispm-ui-form-define-type 'date
  :validate
  (lambda (val _props)
    (when (and val (not (lispm-ui-form--valid-date-p val)))
      "Enter a valid YYYY-MM-DD date"))
  :render
  (lambda (key _field props value label-fn row-fn)
    (let ((ro (or (plist-get props :disabled) (plist-get props :read-only))))
      (lispm-ui-form--simple-field
       key label-fn row-fn value
       (lambda ()
         (if ro
             (insert (propertize (or value "") 'face 'shadow))
           (let ((h (lispm-ui-insert-input
                     :value (or value "")
                     :placeholder (or (plist-get props :placeholder) "YYYY-MM-DD")
                     :on-change (lambda (v) (lispm-ui-form--changed key v nil)))))
             (push h lispm-ui-form--ext-handles))
           (insert "  ")
           (lispm-ui-insert-button
            (lispm-ui-tr "Today")
            (lambda ()
              (lispm-ui-form--changed key (format-time-string "%Y-%m-%d") t))
            :variant 'secondary)))))))

(defun lispm-ui-form--repeater-swap (items i j)
  "Return a copy of ITEMS with elements at indices I and J swapped."
  (let ((vec (vconcat items)))
    (cl-rotatef (aref vec i) (aref vec j))
    (append vec nil)))

(defun lispm-ui-form--repeater-row (key items idx item ro)
  "Insert one repeater item IDX displaying ITEM for KEY."
  (let ((n (length items)))
    (insert (format "%d. %s  " (1+ idx) item))
    (unless ro
      (when (> idx 0)
        (lispm-ui-insert-button
         "↑"
         (lambda ()
           (lispm-ui-form--changed
            key (lispm-ui-form--repeater-swap items idx (1- idx)) t))
         :variant 'ghost))
      (when (< (1+ idx) n)
        (lispm-ui-insert-button
         "↓"
         (lambda ()
           (lispm-ui-form--changed
            key (lispm-ui-form--repeater-swap items idx (1+ idx)) t))
         :variant 'ghost))
      (lispm-ui-insert-button
       "×"
       (lambda ()
         (let ((next (append (seq-take items idx) (nthcdr (1+ idx) items))))
           (lispm-ui-form--changed key next t)))
       :variant 'ghost))))

(lispm-ui-form-define-type 'repeater
  :render
  (lambda (key _field props value label-fn row-fn)
    (let ((items (copy-sequence value))
          (ro (or (plist-get props :disabled) (plist-get props :read-only)))
          (def (or (plist-get props :item-default) "New item")))
      (lispm-ui-form--simple-field
       key label-fn row-fn items
       (lambda ()
         (if (null items)
             (insert (propertize "(empty)  " 'face 'lispm-ui-muted))
           (insert (format "(%d)" (length items)) "  "))
         (unless ro
           (lispm-ui-insert-button
            (or (plist-get props :add-label) "+ Add")
            (lambda ()
              (let ((next-item (if (functionp def) (funcall def items) def)))
                (lispm-ui-form--changed key (append items (list next-item)) t)))
            :variant 'secondary))))
      (cl-loop for item in items for idx from 0 do
               (funcall row-fn
                        (lambda ()
                          (insert (lispm-ui-form--at :control))
                          (lispm-ui-form--repeater-row key items idx item ro)))))))

(provide 'lispm-ui-form-ext)
;;; lispm-ui-form-ext.el ends here
