;;; lispm-ui-input.el --- Embeddable inputs, comboboxes, and tag inputs -*- lexical-binding: t; -*-

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

;; Embeddable interactive input widgets safe inside read-only UI buffers:
;;
;; - `lispm-ui-insert-input': single-line / search input with markers,
;;   placeholder (`'cursor t'), `M-a'/`C-x h' field select-all,
;;   `delete-selection' paste replacement, clear button, IME composition
;;   guard, debounced `on-change', `:read-only' / `:disabled' / `:invalid'.
;; - `lispm-ui-insert-combobox': filterable searchable select supporting
;;   arbitrary candidate list lengths.
;; - `lispm-ui-insert-tags': removable chip tag input + suggestions.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-ui-base)
(require 'lispm-ui-frp)
(require 'lispm-ui-frp-async)
(require 'lispm-ui-palette)
(require 'lispm-ui-tokens)
(require 'lispm-ui-icons)
(require 'lispm-ui-controls)

(defvar quail-current-key nil)

;;;; Inline embeddable input

(cl-defstruct (lispm-ui-input
               (:constructor lispm-ui-input--create)
               (:copier nil)
               (:predicate lispm-ui-input-p))
  "Handle to an inline embeddable text input field."
  buffer
  beg-marker
  end-marker
  overlay
  signal
  stream
  debounced
  on-change
  on-submit
  on-cancel
  placeholder
  disabled
  read-only
  invalid
  scope)

(defun lispm-ui-input--at (&optional pos)
  "Return the `lispm-ui-input' instance enclosing POS (default point)."
  (let ((p (or pos (point))))
    (or (get-text-property p 'lispm-ui-input)
        (and (> p (point-min)) (get-text-property (1- p) 'lispm-ui-input)))))

(defun lispm-ui-input-value (input)
  "Return the current string value stored in INPUT."
  (when (and (lispm-ui-input-p input)
             (buffer-live-p (lispm-ui-input-buffer input)))
    (with-current-buffer (lispm-ui-input-buffer input)
      (buffer-substring-no-properties
       (marker-position (lispm-ui-input-beg-marker input))
       (marker-position (lispm-ui-input-end-marker input))))))

(defun lispm-ui-input--sync-overlay (input &optional focused)
  "Refresh INPUT overlay placeholder and border face."
  (when-let* ((ov (lispm-ui-input-overlay input))
              ((overlay-buffer ov)))
    (let* ((val (lispm-ui-input-value input))
           (empty (or (null val) (string-empty-p val)))
           (ph (lispm-ui-input-placeholder input))
           (face (cond
                  ((lispm-ui-input-disabled input) 'shadow)
                  ((lispm-ui-input-invalid input)
                   `(:inherit lispm-ui-field
                     :box (:line-width (1 . 1)
                           :color ,(or (lispm-ui-token :danger-fg) "#dc2626"))))
                  (focused 'lispm-ui-field-focused)
                  (t 'lispm-ui-field))))
      (overlay-put ov 'face face)
      (overlay-put ov 'before-string
                   (when (and empty ph (not (string-empty-p ph)))
                     (propertize ph
                                 'face 'lispm-ui-muted
                                 'cursor t))))))

(defun lispm-ui-input--emit (input)
  "Synchronize INPUT signal, stream, and overlay after text edits."
  (let ((val (or (lispm-ui-input-value input) "")))
    (lispm-ui-input--sync-overlay input t)
    (lispm-ui-signal-set (lispm-ui-input-signal input) val)
    (when (lispm-ui-input-stream input)
      (lispm-ui-stream-emit (lispm-ui-input-stream input) val))
    (when (and (lispm-ui-input-on-change input)
               (null (lispm-ui-input-debounced input)))
      (funcall (lispm-ui-input-on-change input) val))
    val))

(defun lispm-ui-input-set-value (input text &optional silent)
  "Replace INPUT contents with TEXT safely inside read-only buffers."
  (when (and (lispm-ui-input-p input)
             (buffer-live-p (lispm-ui-input-buffer input)))
    (with-current-buffer (lispm-ui-input-buffer input)
      (let* ((beg (marker-position (lispm-ui-input-beg-marker input)))
             (end (marker-position (lispm-ui-input-end-marker input)))
             (map (get-text-property end 'keymap))
             (inhibit-read-only t))
        (delete-region beg end)
        (goto-char beg)
        (insert (propertize (or text "")
                            'lispm-ui-input input
                            'keymap map
                            'read-only nil))
        (move-marker (lispm-ui-input-end-marker input) (point))
        (move-overlay (lispm-ui-input-overlay input) beg (min (point-max) (1+ (point))))
        (if silent
            (lispm-ui-input--sync-overlay input nil)
          (lispm-ui-input--emit input))))))

(defun lispm-ui-input-clear (input)
  "Clear INPUT text and restore focus."
  (interactive)
  (when-let* ((target (or input (lispm-ui-input--at))))
    (when (and (not (lispm-ui-input-disabled target))
               (not (lispm-ui-input-read-only target)))
      (lispm-ui-input-set-value target "")
      (lispm-ui-input-focus target))))

(defun lispm-ui-input-focus (input)
  "Move point into INPUT field."
  (when (and (lispm-ui-input-p input)
             (buffer-live-p (lispm-ui-input-buffer input)))
    (with-current-buffer (lispm-ui-input-buffer input)
      (goto-char (marker-position (lispm-ui-input-end-marker input)))
      (lispm-ui-input--sync-overlay input t))))

(defun lispm-ui-input-set-invalid (input flag)
  "Mark INPUT invalid when FLAG is non-nil."
  (when (lispm-ui-input-p input)
    (setf (lispm-ui-input-invalid input) (and flag t))
    (lispm-ui-input--sync-overlay input nil)))

(defun lispm-ui-input--editable-p (input)
  "Return non-nil when INPUT may be edited now (respecting IME composition)."
  (and (lispm-ui-input-p input)
       (not (lispm-ui-input-disabled input))
       (not (lispm-ui-input-read-only input))
       (not (bound-and-true-p quail-current-key))))

(defun lispm-ui-input--replace-selection-if-active (input)
  "Delete active region inside INPUT bounds before insertion."
  (when (use-region-p)
    (let* ((beg (max (marker-position (lispm-ui-input-beg-marker input))
                     (region-beginning)))
           (end (min (marker-position (lispm-ui-input-end-marker input))
                     (region-end)))
           (inhibit-read-only t))
      (when (< beg end)
        (delete-region beg end)
        (goto-char beg))
      (deactivate-mark))))

(defun lispm-ui-input--self-insert ()
  "Insert typed character cleanly inside embedded `lispm-ui-input'."
  (interactive)
  (when-let* ((input (lispm-ui-input--at)))
    (if (not (lispm-ui-input--editable-p input))
        (user-error "Input is read-only")
      (let* ((beg (marker-position (lispm-ui-input-beg-marker input)))
             (end (marker-position (lispm-ui-input-end-marker input)))
             (map (get-text-property end 'keymap))
             (inhibit-read-only t)
             (ch (and (characterp last-command-event)
                      (char-to-string last-command-event))))
        (when ch
          (lispm-ui-input--replace-selection-if-active input)
          (goto-char (max beg (min (marker-position (lispm-ui-input-end-marker input)) (point))))
          (insert (propertize ch 'lispm-ui-input input 'keymap map 'read-only nil))
          (when (> (point) (marker-position (lispm-ui-input-end-marker input)))
            (move-marker (lispm-ui-input-end-marker input) (point)))
          (move-overlay (lispm-ui-input-overlay input)
                        beg (min (point-max) (1+ (marker-position (lispm-ui-input-end-marker input)))))
          (lispm-ui-input--emit input))))))

(defun lispm-ui-input--backspace ()
  "Delete backward inside embedded `lispm-ui-input'."
  (interactive)
  (when-let* ((input (lispm-ui-input--at)))
    (when (lispm-ui-input--editable-p input)
      (let* ((beg (marker-position (lispm-ui-input-beg-marker input)))
             (inhibit-read-only t))
        (if (use-region-p)
            (lispm-ui-input--replace-selection-if-active input)
          (when (> (point) beg)
            (delete-char -1)))
        (move-overlay (lispm-ui-input-overlay input)
                      beg (min (point-max) (1+ (marker-position (lispm-ui-input-end-marker input)))))
        (lispm-ui-input--emit input)))))

(defun lispm-ui-input--select-all ()
  "Select all text inside the active `lispm-ui-input' field."
  (interactive)
  (when-let* ((input (lispm-ui-input--at)))
    (let ((beg (marker-position (lispm-ui-input-beg-marker input)))
          (end (marker-position (lispm-ui-input-end-marker input))))
      (goto-char end)
      (push-mark beg t t)
      (activate-mark))))

(defun lispm-ui-input--yank ()
  "Paste text into embedded `lispm-ui-input', replacing active selection."
  (interactive)
  (when-let* ((input (lispm-ui-input--at)))
    (when (lispm-ui-input--editable-p input)
      (let* ((beg (marker-position (lispm-ui-input-beg-marker input)))
             (end (marker-position (lispm-ui-input-end-marker input)))
             (map (get-text-property end 'keymap))
             (txt (substring-no-properties (current-kill 0)))
             (inhibit-read-only t))
        (lispm-ui-input--replace-selection-if-active input)
        (goto-char (max beg (min (marker-position (lispm-ui-input-end-marker input)) (point))))
        (insert (propertize txt 'lispm-ui-input input 'keymap map 'read-only nil))
        (when (> (point) (marker-position (lispm-ui-input-end-marker input)))
          (move-marker (lispm-ui-input-end-marker input) (point)))
        (move-overlay (lispm-ui-input-overlay input)
                      beg (min (point-max) (1+ (marker-position (lispm-ui-input-end-marker input)))))
        (lispm-ui-input--emit input)))))

(defun lispm-ui-input--submit ()
  "Invoke `:on-submit' handler of active `lispm-ui-input'."
  (interactive)
  (when-let* ((input (lispm-ui-input--at)))
    (when (lispm-ui-input-on-submit input)
      (funcall (lispm-ui-input-on-submit input) (lispm-ui-input-value input)))))

(defun lispm-ui-input--make-keymap (input)
  "Build local keymap for embedded INPUT."
  (let ((m (make-sparse-keymap)))
    ;; Bind printable ASCII explicitly so `special-mode' / `lispm-ui-view-mode'
    ;; bindings (`undefined', `digit-argument') cannot shadow text entry.
    (cl-loop for ch from ?\s to ?~ do
             (define-key m (vector ch) #'lispm-ui-input--self-insert))
    (keymap-set m "<remap> <self-insert-command>" #'lispm-ui-input--self-insert)
    (keymap-set m "DEL" #'lispm-ui-input--backspace)
    (keymap-set m "<backspace>" #'lispm-ui-input--backspace)
    (keymap-set m "M-a" #'lispm-ui-input--select-all)
    (keymap-set m "C-x h" #'lispm-ui-input--select-all)
    (keymap-set m "C-y" #'lispm-ui-input--yank)
    (keymap-set m "C-a" (lambda () (interactive)
                          (goto-char (marker-position (lispm-ui-input-beg-marker input)))))
    (keymap-set m "C-e" (lambda () (interactive)
                          (goto-char (marker-position (lispm-ui-input-end-marker input)))))
    (keymap-set m "RET" #'lispm-ui-input--submit)
    (keymap-set m "<escape>" (lambda () (interactive)
                               (when (lispm-ui-input-on-cancel input)
                                 (funcall (lispm-ui-input-on-cancel input)))))
    m))

(defun lispm-ui-input-dispose (input)
  "Dispose reactive streams, timers, markers, and overlays owned by INPUT."
  (when (lispm-ui-input-p input)
    (when (lispm-ui-scope-p (lispm-ui-input-scope input))
      (lispm-ui-scope-dispose (lispm-ui-input-scope input)))
    (when (and (lispm-ui-input-overlay input)
               (overlay-buffer (lispm-ui-input-overlay input)))
      (delete-overlay (lispm-ui-input-overlay input)))
    (when (markerp (lispm-ui-input-beg-marker input))
      (set-marker (lispm-ui-input-beg-marker input) nil))
    (when (markerp (lispm-ui-input-end-marker input))
      (set-marker (lispm-ui-input-end-marker input) nil)))
  nil)

;;;###autoload
(cl-defun lispm-ui-insert-input (&key value placeholder width icon clearable
                                      debounce on-change on-submit on-cancel
                                      disabled read-only invalid)
  "Insert an embeddable inline text input at point and return `lispm-ui-input'."
  (ignore width)
  (let* ((scope (lispm-ui-scope-create))
         (sig (lispm-ui-signal (or value "")))
         (stream (lispm-ui-stream))
         (deb (when (and debounce (> debounce 0))
                (lispm-ui-with-scope scope
                  (lispm-ui-stream-debounce debounce stream))))
         (input (lispm-ui-input--create
                 :buffer (current-buffer)
                 :signal sig
                 :stream stream
                 :debounced deb
                 :on-change on-change
                 :on-submit on-submit
                 :on-cancel on-cancel
                 :placeholder placeholder
                 :disabled disabled
                 :read-only read-only
                 :invalid invalid
                 :scope scope))
         (map (lispm-ui-input--make-keymap input)))
    (when (and deb on-change)
      (lispm-ui-with-scope scope
        (lispm-ui-stream-subscribe deb on-change)))
    (lispm-ui-scope-attach-buffer scope (current-buffer))
    (when icon
      (insert (lispm-ui-icon icon) " "))
    (let ((beg (copy-marker (point) nil)))
      (insert (propertize (or value "")
                          'lispm-ui-input input
                          'lispm-ui-focusable t
                          'keymap map
                          'read-only nil))
      (let ((end (copy-marker (point) nil)))
        ;; Trailing 1-char sentinel keeps keymap & input property anchored at end.
        (insert (propertize " "
                            'lispm-ui-input input
                            'lispm-ui-focusable t
                            'keymap map
                            'rear-nonsticky t
                            'read-only nil))
        (let ((ov (make-overlay beg (point) nil nil t)))
          (setf (lispm-ui-input-beg-marker input) beg
                (lispm-ui-input-end-marker input) end
                (lispm-ui-input-overlay input) ov)
          (overlay-put ov 'evaporate nil)
          (lispm-ui-input--sync-overlay input nil))))
    (when (and clearable (not disabled) (not read-only))
      (let ((cmap (let ((m (make-sparse-keymap)))
                    (keymap-set m "RET" (lambda () (interactive) (lispm-ui-input-clear input)))
                    (keymap-set m "<down-mouse-1>"
                                (lambda (_e) (interactive "e") (lispm-ui-input-clear input)))
                    (keymap-set m "<mouse-1>" #'ignore)
                    m)))
        (insert " " (propertize "×" 'keymap cmap 'face 'shadow 'pointer 'hand
                                'help-echo "Clear"))))
    input))

;;;; Combobox (searchable inline candidate selector)

(cl-defstruct (lispm-ui-combobox
               (:constructor lispm-ui-combobox--create)
               (:copier nil)
               (:predicate lispm-ui-combobox-p))
  "Handle for a searchable inline combobox widget."
  input
  candidates
  filter-fn
  on-select
  value
  (index 0 :type integer)
  (max-visible 8 :type integer))

(defun lispm-ui-combobox--norm (cand)
  "Return `(VALUE . LABEL)' pair for Combobox candidate CAND."
  (if (consp cand) cand (cons cand (format "%s" cand))))

(defun lispm-ui-combobox-filter (query candidates &optional filter-fn)
  "Filter CANDIDATES by QUERY (case-insensitive prefix before substring)."
  (let* ((q (downcase (string-trim (or query ""))))
         (pairs (mapcar #'lispm-ui-combobox--norm candidates)))
    (if (string-empty-p q)
        pairs
      (let (prefix contains)
        (dolist (pair pairs)
          (let ((label (downcase (format "%s" (cdr pair)))))
            (when (if filter-fn
                      (funcall filter-fn q (cdr pair) (car pair))
                    (string-search q label))
              (if (string-prefix-p q label)
                  (push pair prefix)
                (push pair contains)))))
        (append (nreverse prefix) (nreverse contains))))))

(defun lispm-ui-combobox-matches (combo)
  "Return currently matched candidates for COMBO up to `:max-visible'."
  (let ((q (or (lispm-ui-input-value (lispm-ui-combobox-input combo)) "")))
    (seq-take (lispm-ui-combobox-filter q
                                        (lispm-ui-combobox-candidates combo)
                                        (lispm-ui-combobox-filter-fn combo))
              (lispm-ui-combobox-max-visible combo))))

(defun lispm-ui-combobox-select (combo &optional index)
  "Select candidate at INDEX (or current index) in COMBO and invoke callback."
  (let* ((matches (lispm-ui-combobox-matches combo))
         (idx (max 0 (min (or index (lispm-ui-combobox-index combo))
                          (max 0 (1- (length matches))))))
         (picked (nth idx matches)))
    (when picked
      (setf (lispm-ui-combobox-value combo) (car picked)
            (lispm-ui-combobox-index combo) idx)
      (lispm-ui-input-set-value (lispm-ui-combobox-input combo)
                                (format "%s" (cdr picked)) t)
      (when (lispm-ui-combobox-on-select combo)
        (funcall (lispm-ui-combobox-on-select combo) (car picked) (cdr picked))))
    picked))

(defun lispm-ui-combobox-move (combo delta)
  "Move highlighted match index in COMBO by DELTA."
  (let ((count (length (lispm-ui-combobox-matches combo))))
    (when (> count 0)
      (setf (lispm-ui-combobox-index combo)
            (mod (+ (lispm-ui-combobox-index combo) delta) count)))))

(defun lispm-ui-combobox-dispose (combo)
  "Dispose resources owned by COMBO."
  (when (lispm-ui-combobox-p combo)
    (lispm-ui-input-dispose (lispm-ui-combobox-input combo)))
  nil)

;;;###autoload
(cl-defun lispm-ui-insert-combobox (candidates &key value placeholder width
                                         (max-visible 8) filter-fn on-select disabled)
  "Insert an inline searchable Combobox over CANDIDATES and return its handle."
  (let* ((combo (lispm-ui-combobox--create
                 :candidates candidates
                 :filter-fn filter-fn
                 :on-select on-select
                 :value value
                 :max-visible max-visible))
         (init-label (when value
                       (or (cdr (assoc value (mapcar #'lispm-ui-combobox--norm candidates)))
                           (format "%s" value))))
         (input (lispm-ui-insert-input
                 :value (or init-label "")
                 :placeholder (or placeholder "Filter…")
                 :width width
                 :icon 'search
                 :clearable t
                 :disabled disabled
                 :on-submit (lambda (_q) (lispm-ui-combobox-select combo)))))
    (setf (lispm-ui-combobox-input combo) input)
    combo))

;;;; Tag Input / Multi-select

(cl-defstruct (lispm-ui-tags
               (:constructor lispm-ui-tags--create)
               (:copier nil)
               (:predicate lispm-ui-tags-p))
  "Handle for a removable chip tag input widget."
  values
  suggestions
  (allow-custom t :type boolean)
  on-change
  disabled
  input)

(defun lispm-ui-tags-add (handle tag)
  "Append normalized non-empty TAG to HANDLE if allowed and not duplicate."
  (let ((clean (string-trim (format "%s" (or tag "")))))
    (when (and (lispm-ui-tags-p handle)
               (not (lispm-ui-tags-disabled handle))
               (not (string-empty-p clean))
               (or (lispm-ui-tags-allow-custom handle)
                   (member clean (lispm-ui-tags-suggestions handle)))
               (not (member clean (lispm-ui-tags-values handle))))
      (setf (lispm-ui-tags-values handle)
            (append (lispm-ui-tags-values handle) (list clean)))
      (when (lispm-ui-tags-on-change handle)
        (funcall (lispm-ui-tags-on-change handle) (lispm-ui-tags-values handle))))
    (lispm-ui-tags-values handle)))

(defun lispm-ui-tags-remove (handle tag)
  "Remove TAG from HANDLE and notify `:on-change'."
  (when (and (lispm-ui-tags-p handle)
             (not (lispm-ui-tags-disabled handle))
             (member tag (lispm-ui-tags-values handle)))
    (setf (lispm-ui-tags-values handle)
          (delete tag (copy-sequence (lispm-ui-tags-values handle))))
    (when (lispm-ui-tags-on-change handle)
      (funcall (lispm-ui-tags-on-change handle) (lispm-ui-tags-values handle))))
  (lispm-ui-tags-values handle))

(defun lispm-ui-tags-dispose (handle)
  "Dispose resources owned by tag input HANDLE."
  (when (and (lispm-ui-tags-p handle) (lispm-ui-tags-input handle))
    (lispm-ui-input-dispose (lispm-ui-tags-input handle)))
  nil)

;;;###autoload
(cl-defun lispm-ui-insert-tags (tags &key placeholder suggestions on-change
                                     (allow-custom t) disabled)
  "Insert a chip tag input initialized with TAGS and return its handle."
  (let ((handle (lispm-ui-tags--create
                 :values (copy-sequence tags)
                 :suggestions suggestions
                 :allow-custom (and allow-custom t)
                 :on-change on-change
                 :disabled disabled)))
    (dolist (tag (lispm-ui-tags-values handle))
      (let* ((tcopy tag)
             (rm-map (unless disabled
                       (let ((m (make-sparse-keymap)))
                         (keymap-set m "RET" (lambda () (interactive)
                                               (lispm-ui-tags-remove handle tcopy)))
                         (keymap-set m "<down-mouse-1>"
                                     (lambda (_e) (interactive "e")
                                       (lispm-ui-tags-remove handle tcopy)))
                         (keymap-set m "<mouse-1>" #'ignore)
                         m))))
        (insert (propertize (concat "[" tag (unless disabled " ×") "]")
                            'face `(:background ,(or (lispm-ui-token :surface-2) "#e4e4e7")
                                    :foreground ,(or (lispm-ui-color :text) "#000000"))
                            'lispm-ui-tag tcopy
                            'keymap rm-map)
                " ")))
    (setf (lispm-ui-tags-input handle)
          (lispm-ui-insert-input
           :placeholder (or placeholder "Add tag…")
           :disabled disabled
           :on-submit (lambda (txt)
                        (when (lispm-ui-tags-add handle txt)
                          (lispm-ui-input-set-value (lispm-ui-tags-input handle) "")))))
    handle))

(provide 'lispm-ui-input)
;;; lispm-ui-input.el ends here
