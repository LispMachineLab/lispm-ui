;;; lispm-ui-form-widgets.el --- Field widgets, shaping and mouse in forms  -*- lexical-binding: t; -*-

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

;; Field widget shaping (`lispm-ui-form--shape', `lispm-ui-form--shape-field',
;; `lispm-ui-form--shape-area'), single-line viewport scrolling, password
;; star masking (`lispm-ui-form-password'), and mouse interaction
;; (`lispm-ui-form--mouse', `lispm-ui-form--press-button') for `lispm-ui-form'.

;;; Code:

(require 'cl-lib)
(require 'delsel)
(require 'seq)
(require 'subr-x)
(require 'wid-edit)
(require 'lispm-ui-base)
(require 'lispm-ui-i18n)
(require 'lispm-ui-text)
(require 'lispm-ui-modal)
(require 'lispm-ui-popup)
(require 'lispm-ui-ask)

(defvar lispm-ui-form--geometry)
(defvar lispm-ui-form--field-at-point)
(defvar lispm-ui-form--focus)
(defvar lispm-ui-form-choose-file-function)

(declare-function lispm-ui-form--measure "lispm-ui-form" (string))
(declare-function lispm-ui-form--at "lispm-ui-form" (key &optional less face))
(declare-function lispm-ui-form--edge "lispm-ui-form" (&optional thin))
(declare-function lispm-ui-form--sides "lispm-ui-form" (group &optional thin))
(declare-function lispm-ui-form--place "lispm-ui-form" ())
(declare-function lispm-ui-form--go-to "lispm-ui-form" (place))
(declare-function lispm-ui-form--remember "lispm-ui-form" (key widget))
(declare-function lispm-ui-form--changed "lispm-ui-form" (key value &optional redraw))
(declare-function lispm-ui-form--row "lispm-ui-form" (group insert &optional thin))
(declare-function lispm-ui-form--glyph "lispm-ui-form" (kind state &optional label))
(declare-function lispm-ui-form--button "lispm-ui-form" (label action &optional default))
(declare-function lispm-ui-form--rows "lispm-ui-form" (field))
(declare-function lispm-ui-form--columns "lispm-ui-form" (rows start))
(declare-function lispm-ui-form--draw-row "lispm-ui-form" (key row columns))
(declare-function lispm-ui-form--help "lispm-ui-form" (help at))
(declare-function lispm-ui-form--apply-text-props "lispm-ui-form-ext" (widget props value))
(declare-function lispm-ui-form--choices "lispm-ui-form" (field))
(declare-function lispm-ui-form--insert-buttons "lispm-ui-form" (key buttons))
(declare-function lispm-ui-form-submit "lispm-ui-form" ())
(declare-function lispm-ui-form--control-at "lispm-ui-form-dress" (position))
(declare-function lispm-ui-form--show "lispm-ui-form-dress" (start end image))
(defvar lispm-ui-form--title)
(defvar lispm-ui-form-choose-function)

;;;; Field drawers

(defun lispm-ui-form--draw-text-field (key type props value label-fn row-fn)
  "Insert a `text' or `password' field KEY with PROPS and VALUE."
  (funcall
   row-fn
   (lambda ()
     (funcall label-fn)
     (insert (lispm-ui-form--at :control))
     (lispm-ui-form--remember
      key (widget-create (if (eq type 'password) 'lispm-ui-form-password 'editable-field)
                         :size (max 200 (+ 32 (length (or value ""))))
                         :format "%v"
                         :value (or value "")
                         :lispm-form-width (plist-get props :width)
                         :lispm-form-props props
                         :action (if (plist-get props :submit)
                                     (lambda (&rest _) (lispm-ui-form-submit))
                                   (lambda (&rest _) (widget-forward 1)))
                         :notify (lambda (widget &optional _child event)
                                   (unless (eq (car-safe event) 'before-change)
                                     (when (eq type 'password)
                                       (lispm-ui-form--mask-password widget event))
                                     (lispm-ui-form--one-line widget)
                                     (lispm-ui-form--changed key (widget-value widget))
                                     (lispm-ui-form--shape-field widget))))))))

(defun lispm-ui-form--draw-text-area-field (key props value group label-fn)
  "Insert a multi-line `text-area' field KEY with PROPS and VALUE in GROUP."
  (let* ((lines (or (plist-get props :lines) 6))
         (shown (string-trim-right (or value ""))))
    (lispm-ui-form--row group (lambda () (lispm-ui-form--area-rule)) t)
    (let ((sides (lispm-ui-form--sides group)))
      (when sides (insert (car sides)))
      (funcall label-fn)
      (insert (lispm-ui-form--at :control 1) (lispm-ui-form--edge))
      (lispm-ui-form--remember
       key (widget-create 'text
                          :format "%v"
                          :value-face 'lispm-ui-form-area
                          :value (concat shown (make-string (max 0 (- lines 1 (cl-count ?\n shown))) ?\n))
                          :lispm-form-group group
                          :notify (lambda (widget &optional _child event)
                                    (unless (eq (car-safe event) 'before-change)
                                      (lispm-ui-form--changed key (widget-value widget))
                                      (lispm-ui-form--shape-area widget))))))
    (lispm-ui-form--row group (lambda () (lispm-ui-form--area-rule)) t)))

(defun lispm-ui-form--draw-picture-field (key label value label-fn row-fn)
  "Insert a `picture' field KEY with LABEL and VALUE."
  (funcall
   row-fn
   (lambda ()
     (funcall label-fn)
     (insert (lispm-ui-form--at :control))
     (let ((glyph (lispm-ui-form--glyph 'picture nil value)))
       (cond (glyph (insert-image glyph (if value (file-name-nondirectory value) " ")))
             ((and value (display-graphic-p) (file-readable-p value))
              (insert-image (create-image value nil nil :max-width 64 :max-height 64
                                          :ascent 'center)
                            (file-name-nondirectory value)))
             (t (insert (propertize (if value (file-name-nondirectory value)
                                      (lispm-ui-tr "No picture"))
                                    'face 'lispm-ui-form-help)))))
     (insert (propertize " " 'display `(space :width (,(plist-get lispm-ui-form--geometry :em)))))
     (lispm-ui-form--remember
      key (lispm-ui-form--button (lispm-ui-tr "Choose…")
                                 (lambda ()
                                   (when-let* ((file (lispm-ui-form--read-picture value label)))
                                     (lispm-ui-form--changed key file t)))))
     (when value
       (insert " ")
       (lispm-ui-form--button (lispm-ui-tr "Remove") (lambda () (lispm-ui-form--changed key nil t)))))))

(defun lispm-ui-form--draw-rows-field (key field props start label-fn row-fn)
  "Insert a `rows' field KEY with FIELD definition and PROPS at START."
  (let ((rows (lispm-ui-form--rows field)))
    (if rows
        (let ((columns (or (lispm-ui-form--columns rows start)
                           (list (plist-get lispm-ui-form--geometry start)))))
          (dolist (each rows)
            (funcall row-fn (lambda ()
                              (funcall label-fn)
                              (lispm-ui-form--draw-row key each columns)))
            (when-let* ((help (plist-get each :help)))
              (dolist (line (split-string help "\n"))
                (funcall row-fn (lambda () (insert (lispm-ui-form--at start) (lispm-ui-form--help line start))))))))
      (funcall row-fn (lambda ()
                        (funcall label-fn)
                        (insert (lispm-ui-form--at start)
                                (lispm-ui-form--help (or (plist-get props :empty) "") start)))))))

(defun lispm-ui-form--draw-choice-field (key field props label value label-fn row-fn)
  "Insert a `choice' popup field KEY with FIELD definition, PROPS, and VALUE."
  (let* ((choices (lispm-ui-form--choices field))
         (shown (or (cdr (assoc value choices)) (lispm-ui-tr "Choose…"))))
    (funcall
     row-fn
     (lambda ()
       (funcall label-fn)
       (insert (lispm-ui-form--at :control))
       (lispm-ui-form--remember
        key (widget-create 'push-button
                           :tag (concat shown " ▾")
                           :tag-glyph (lispm-ui-form--glyph 'popup nil shown)
                           :lispm-form-control (list 'popup nil shown)
                           :notify (lambda (_widget &optional _child event)
                                     (when-let* ((choice (funcall lispm-ui-form-choose-function
                                                                  (or label lispm-ui-form--title)
                                                                  choices value event)))
                                       (lispm-ui-form--changed key (car choice) t)))))
       (when-let* ((buttons (plist-get props :buttons)))
         (insert (propertize " " 'display `(space :width (,(plist-get lispm-ui-form--geometry :em)))))
         (lispm-ui-form--insert-buttons (or (plist-get props :buttons-key) key) buttons))))))

;;;; Binary search & fitting

(defalias 'lispm-ui-form--bisect #'lispm-ui-text-bisect)

(defun lispm-ui-form--fit (text pixels)
  "TEXT cut to PIXELS wide in this buffer's faces, ending in an ellipsis
when it was cut."
  (if (<= (lispm-ui-form--measure text) pixels)
      text
    (let ((length (lispm-ui-form--bisect
                   (lambda (n)
                     (<= (lispm-ui-form--measure (concat (substring text 0 n) "…"))
                         pixels))
                   0 (length text))))
      (concat (string-trim-right (substring text 0 length))
              (propertize "…" 'face (get-text-property (max 0 (1- length)) 'face text))))))

(defun lispm-ui-form--area-rule ()
  "Insert the thin line above or below a text area: its frame's top or bottom."
  (insert (lispm-ui-form--at :control 1 '(:height 10))
          (propertize " " 'display `(space :width (,(+ 2 (- (plist-get lispm-ui-form--geometry :right)
                                                           (plist-get lispm-ui-form--geometry :control))))
                                           :height (1))
                      'face '(lispm-ui-form-area-edge (:height 10)))))

;;;; Field shaping & scrolling

(defun lispm-ui-form--shape ()
  "Shape the fields of the form in this buffer: each text field as wide as
the form's fields, each text area a panel as wide, the form's keys
working in each."
  (dolist (field widget-field-list)
    (lispm-ui-form--field-keys field)
    (pcase (widget-type field)
      ((or 'editable-field 'lispm-ui-form-password) (lispm-ui-form--shape-field field))
      ('text (lispm-ui-form--shape-area field)))))

(defun lispm-ui-form--field-keys (field)
  "Let the keys of the form's mode work in FIELD too, as C-c C-c: Emacs's
widgets make a field's keymap the local one, in place of the mode's,
and here it goes before the mode's instead."
  (when-let* ((overlay (widget-get field :field-overlay))
              ((overlayp overlay))
              (start (overlay-start overlay)))
    ;; Its last newline too, in an overlay of its own.
    (dolist (each (overlays-in start (min (point-max) (+ 2 (overlay-end overlay)))))
      (when-let* (((or (eq (overlay-get each 'field) field) (eq (overlay-get each 'real-field) field)))
                  (map (overlay-get each 'local-map)))
        (overlay-put each 'local-map nil)
        (overlay-put each 'keymap map)))))

(defun lispm-ui-form--shape-field (widget)
  "Make the text field WIDGET as wide as the form's fields, whatever its text.
It shows as much of its text as that holds, the rest hidden: its start,
or, while point is in it, the text about point, scrolled as a dialog's
field scrolls, so that a long text, as one pasted, never runs past the
field's edge onto a line of its own.  Its padding, the blanks Emacs's
widgets end a field of a fixed size with, is drawn as one blank reaching
to the right edge, the rest of it hidden, so that the field is one run
of its face, framed once."
  (when (and (widget-field-start widget)
             (widget-field-end widget)
             (widget-field-text-end widget)
             (>= (widget-field-text-end widget) (1- (widget-field-end widget))))
    (with-silent-modifications
      (save-excursion
        (goto-char (widget-field-end widget))
        (if (overlayp (widget-get widget :field-overlay))
            (insert (make-string 32 ?\s))
          (insert-before-markers (make-string 32 ?\s))))
      (widget-put widget :size (- (widget-field-end widget) (widget-field-start widget)))))
  (let* ((start (widget-field-start widget))
         (end (widget-field-end widget))
         (text-end (widget-field-text-end widget))
         (here (and start end (<= start (point) end)))
         ;; Blanks just typed are shown, though they end the text, until
         ;; point leaves them: they are as Emacs's widgets pad a field.
         (shown (if (and text-end end (< text-end (point) end)) (point) text-end))
         (left (plist-get lispm-ui-form--geometry :control))
         (right (if-let* ((width (widget-get widget :lispm-form-width)))
                    (+ left (* width (plist-get lispm-ui-form--geometry :em)))
                  (plist-get lispm-ui-form--geometry :right))))
    (when (and start end text-end)
      (with-silent-modifications
        (remove-list-of-text-properties start end '(display invisible lispm-ui-form-padding))
        (pcase-let ((`(,from . ,to) (lispm-ui-form--field-window
                                     widget start shown (and here (point)) (- right left))))
          (widget-put widget :lispm-form-from (- from start))
          (put-text-property start from 'invisible t)
          (put-text-property to shown 'invisible t))
        (when (< shown end)
          ;; The last blank reaches the edge, the others before it are
          ;; hidden: the frame closes only where the field's face ends
          ;; right after what is shown.  Invisible, not displayed as "":
          ;; after each command Emacs takes point out of text displayed
          ;; as nothing, even from its start, to the field's end, and
          ;; what is typed next goes there, after the blanks.
          (put-text-property shown end 'lispm-ui-form-padding t)
          (put-text-property shown
                             (if (and (= shown start) (< shown (- end 2)))
                                 (- end 2)
                               (1- end))
                             'invisible t)
          (put-text-property (1- end) end 'display `(space :align-to (,right))))))
    (when-let* ((props (widget-get widget :lispm-form-props)))
      (lispm-ui-form--apply-text-props widget props (widget-value widget)))))

(defun lispm-ui-form--field-window (widget start end point width)
  "What the text field WIDGET shows of its text from START to END: (FROM . TO).
That is as much as WIDTH pixels hold, less an em for the cursor after
it.  While POINT, non-nil, is in the field, POINT is shown, the field
scrolled no further than that needs from where it was; else its start."
  (let* ((point (and point (min point end)))
         (room (- width (plist-get lispm-ui-form--geometry :em)))
         (fits (lambda (from to) (<= (lispm-ui-form--field-pixels widget from to) room)))
         (from (if point
                   (min point end (+ start (or (widget-get widget :lispm-form-from) 0)))
                 start))
         (to (lispm-ui-form--bisect (lambda (to) (funcall fits from to)) from end)))
    (when (and point (or (> point to) (and (= to end) (> from start))))
      ;; Point past what is shown, the field scrolls left as far as
      ;; that needs; text taken away, what was scrolled out comes back.
      (let ((last (max point to)))
        (setq from (- last (lispm-ui-form--bisect (lambda (n) (funcall fits (- last n) last))
                                                  0 (- last start)))
              to (lispm-ui-form--bisect (lambda (to) (funcall fits from to)) last end))))
    (cons from to)))

(defun lispm-ui-form--field-pixels (widget from to)
  "Pixels the text of the text field WIDGET from FROM to TO takes."
  (lispm-ui-form--measure (propertize (buffer-substring-no-properties from to)
                                      'face (or (widget-get widget :value-face) 'widget-field))))

(defun lispm-ui-form--one-line (widget)
  "Take the line breaks out of the text field WIDGET, as a text pasted into
it brings: a text field holds one line, a password too, its line breaks
shown as stars."
  (when-let* ((start (widget-field-start widget)))
    (save-excursion
      (goto-char start)
      (while (< (point) (widget-field-end widget))
        (if (memq (or (get-text-property (point) 'secret) (char-after)) '(?\n ?\r))
            (delete-char 1)
          (forward-char 1))))))

;;;; Password widget

(define-widget 'lispm-ui-form-password 'editable-field
  "A text field of a form whose text is shown as stars."
  :value-create #'lispm-ui-form--password-create
  :value-get #'lispm-ui-form--password)

(defun lispm-ui-form--password-stars (text)
  "TEXT as stars, each keeping the character it stands for."
  (let ((stars (make-string (length text) ?*)))
    (dotimes (i (length text))
      (put-text-property i (1+ i) 'secret
                         (or (and (eq (aref text i) ?*) (get-text-property i 'secret text))
                             (aref text i))
                         stars))
    (put-text-property 0 (length stars) 'rear-nonsticky '(secret) stars)
    stars))

(defun lispm-ui-form--password-create (widget)
  "Insert the password field WIDGET as stars from the start."
  (let ((value (widget-get widget :value)))
    (unwind-protect
        (progn
          (widget-put widget :value (lispm-ui-form--password-stars value))
          (widget-field-value-create widget))
      (widget-put widget :value value))))

(defun lispm-ui-form--mask-password (widget event)
  "Mask the text added to the password field WIDGET by EVENT."
  (when (eq (car-safe event) 'after-change)
    (let* ((from (nth 1 event))
           (to (min (nth 2 event) (widget-field-end widget)))
           (stars (lispm-ui-form--password-stars (buffer-substring from to)))
           (here (point))
           (inhibit-modification-hooks t)
           (buffer-undo-list t))
      ;; The changed range includes typed spaces, unlike the field's text end.
      (delete-region from to)
      (goto-char from)
      (insert stars)
      (goto-char here))
    ;; Drop plaintext/star undo steps from the current command so undo never
    ;; leaks plaintext or strips `secret' masking properties.
    (when (consp buffer-undo-list)
      (while (and (consp buffer-undo-list) (car buffer-undo-list))
        (pop buffer-undo-list)))))

(defun lispm-ui-form--password (widget)
  "The text of the password field WIDGET: what its stars stand for, a star
typed standing for itself, which Emacs's widgets take for none."
  (if-let* ((from (widget-field-start widget))
            (to (widget-field-text-end widget)))
      (with-current-buffer (widget-field-buffer widget)
        (apply #'string (mapcar (lambda (position)
                                  (or (get-text-property position 'secret) (char-after position)))
                                (number-sequence from (1- to)))))
    (widget-get widget :value)))

;;;; Cursor & mouse handling

(defun lispm-ui-form--field-limit (field)
  "Return the editable upper bound of one-line FIELD, excluding padding."
  (let ((start (widget-field-start field))
        (end (widget-field-end field)))
    (or (and start end (text-property-any start end 'lispm-ui-form-padding t))
        (widget-field-text-end field))))

(defun lispm-ui-form--clamp-field-selection (field)
  "Clamp point and any active mark into one-line FIELD's editable text."
  (when-let* (((memq (widget-type field) '(editable-field lispm-ui-form-password)))
              (start (widget-field-start field))
              (limit (lispm-ui-form--field-limit field)))
    (goto-char (min limit (max start (point))))
    (when (use-region-p)
      (set-marker (mark-marker) (min limit (max start (mark t)))))))

(defun lispm-ui-form--editable-field-at (pos)
  "Return the editable field widget at POS, or nil."
  (when-let* ((field (widget-field-find pos))
              ((memq (widget-type field) '(editable-field lispm-ui-form-password text))))
    field))

(defun lispm-ui-form-select-field ()
  "Select the whole value of the text field at point."
  (interactive)
  (when-let* ((field (lispm-ui-form--editable-field-at (point)))
              (start (widget-field-start field))
              (end (if (eq (widget-type field) 'text)
                       (save-excursion
                         (goto-char (widget-field-end field))
                         (skip-chars-backward "\n" start)
                         (point))
                     (widget-field-text-end field))))
    (goto-char end)
    (lispm-ui-form--keep-point)
    (push-mark start t t)
    (activate-mark)
    (setq deactivate-mark nil)))

(defun lispm-ui-form-select-all ()
  "Select the active field's text if point is in an input, else whole buffer."
  (interactive)
  (if (lispm-ui-form--editable-field-at (point))
      (lispm-ui-form-select-field)
    (push-mark (point-max) t t)
    (goto-char (point-min))))

(defun lispm-ui-form-activate-or-space ()
  "Insert a space inside text inputs, or press a focused form button/toggle."
  (interactive)
  (let ((field (lispm-ui-form--editable-field-at (point)))
        (widget (widget-at (point))))
    (cond
     (field
      (call-interactively #'self-insert-command))
     ((and widget (not (memq (widget-type widget) '(editable-field lispm-ui-form-password text))))
      (widget-button-press (point)))
     ((button-at (point))
      (push-button))
     (t
      (call-interactively #'self-insert-command)))))

(put 'lispm-ui-form-activate-or-space 'delete-selection
     (lambda () (and (lispm-ui-form--editable-field-at (point)) t)))

(defun lispm-ui-form--filter-substring (beg end &optional delete)
  "Filter copied form text from BEG to END, stripping internal properties."
  (let ((text (buffer-substring--filter beg end delete)))
    (remove-text-properties 0 (length text)
                            '(secret nil lispm-ui-form-padding nil)
                            text)
    text))

(defun lispm-ui-form--replace-marked ()
  "Replace an active selection inside a text field before typing or pasting.
Runs on `pre-command-hook' in `lispm-ui-form-mode' so selection
replacement works even when global `delete-selection-mode' is off."
  (when (and (not delete-selection-mode) (use-region-p))
    (let* ((beg (region-beginning))
           (end (region-end))
           (field (widget-field-find beg)))
      (when (and field
                 (eq field (widget-field-find end))
                 (memq (widget-type field) '(editable-field lispm-ui-form-password text)))
        (let ((delete-selection-mode t))
          (delete-selection-pre-hook))))))

(defun lispm-ui-form--keep-point ()
  "Keep point out of the padding of the text field it is in: at the end of
what the field holds, where a dialog's field keeps it, the field scrolled
to show it.  A text field point has left shows its start again.  Run
after each command in a form's buffer."
  (let ((field (widget-field-find (point)))
        (left lispm-ui-form--field-at-point))
    (unless (and field (memq (widget-type field) '(editable-field lispm-ui-form-password)))
      (setq field nil))
    (when-let* ((field)
                (padding (text-property-any (widget-field-start field) (widget-field-end field)
                                            'lispm-ui-form-padding t)))
      (when (< padding (point))
        (goto-char padding))
      (when (and (use-region-p)
                 (eq field (widget-field-find (mark t)))
                 (< padding (mark t)))
        (set-marker (mark-marker) padding)))
    (setq lispm-ui-form--field-at-point field)
    (when (and left (not (eq left field)) (widget-field-start left))
      (lispm-ui-form--shape-field left))
    (when field
      (lispm-ui-form--shape-field field))))

(defun lispm-ui-form--press-button (window button position event)
  "Hold the form's BUTTON at POSITION in WINDOW pressed from EVENT until the
mouse rises, and run its action when it rises over BUTTON, as Mac OS 9's
controls do."
  (select-window window)
  (let* ((buffer (window-buffer window))
         (overlay (widget-get button :button-overlay))
         (start (and (overlayp overlay) (overlay-start overlay)))
         (control (and start (with-current-buffer buffer
                               (lispm-ui-form--control-at start))))
         (place (and (not widget-button-click-moves-point)
                     (lispm-ui-form--place)))
         (restored (when control
                     (pcase-let ((`(,_ ,_ ,plain ,focused ,_) control))
                       (if (equal control (buffer-local-value 'lispm-ui-form--focus buffer))
                           focused
                         plain))))
         (pressed (nth 4 control))
         (same-buffer-p
          (lambda ()
            (and (window-live-p window)
                 (buffer-live-p buffer)
                 (eq (window-buffer window) buffer))))
         (chosen
          (lispm-ui-modal-hold
           (lambda (ev)
             (let* ((pos (event-end ev))
                    (win (posn-window pos))
                    (pt (posn-point pos))
                    (s (and (overlayp overlay) (eq (overlay-buffer overlay) buffer)
                            (overlay-start overlay)))
                    (e (and (overlayp overlay) (eq (overlay-buffer overlay) buffer)
                            (overlay-end overlay))))
               (and (funcall same-buffer-p)
                    (eq win window)
                    (integerp pt)
                    (or (and s e (<= s pt) (< pt e))
                        (eq (with-current-buffer buffer
                              (get-char-property pt 'button))
                            button)))))
           (lambda (in)
             (when (and control (funcall same-buffer-p)
                        (overlayp overlay) (eq (overlay-buffer overlay) buffer))
               (with-current-buffer buffer
                 (let ((s (overlay-start overlay))
                       (e (overlay-end overlay)))
                   (when (and s e (<= e (point-max)))
                     (lispm-ui-form--show s e (if in pressed restored)))))
               (redisplay t))))))
    (when (and chosen (funcall same-buffer-p))
      (with-selected-window window
        (when widget-button-click-moves-point
          (goto-char (or (and (overlayp overlay) (eq (overlay-buffer overlay) buffer)
                              (overlay-start overlay))
                         position)))
        (let ((lispm-ui-in-gui-action t))
          (widget-apply-action button event))
        (when (and place (funcall same-buffer-p))
          (with-current-buffer buffer
            (lispm-ui-form--go-to place)))))))

(defun lispm-ui-form--mouse (event)
  "Do what the mouse EVENT does in a form, as in a dialog: on a field or a
control, what it does in Emacs, a double or triple click in a field of
one line selecting its whole value, without its padding; on anything
else, nothing, point and the region staying where they are."
  (interactive "e")
  (let* ((start (event-start event))
         (window (posn-window start))
         (position (posn-point start))
         (modifiers (event-modifiers event)))
    (when (and (window-live-p window) (integerp position))
      (let* ((buffer (window-buffer window))
             (button (with-current-buffer buffer
                       (get-char-property position 'button)))
             (widget (or button
                         (with-current-buffer buffer
                           (widget-at position)))))
        (cond
         ((not widget)
          (select-window window))
         ((and (memq (widget-type widget) '(editable-field lispm-ui-form-password))
               (or (memq 'double modifiers) (memq 'triple modifiers)))
          (select-window window)
          (goto-char (widget-field-end widget))
          (lispm-ui-form--keep-point)
          (push-mark (widget-field-start widget) t t)
          (activate-mark)
          (setq deactivate-mark nil))
         (button
          (when (memq 'down modifiers)
            (lispm-ui-form--press-button window button position event)))
         ((memq 'down modifiers)
          (select-window window)
          (widget-button-click event)
          (lispm-ui-form--clamp-field-selection widget))
         ((memq 'drag modifiers)
          (mouse-set-region event)
          (lispm-ui-form--clamp-field-selection widget))
         (t (mouse-set-point event t)))))))

(defun lispm-ui-form--shape-area (widget)
  "Make the text area WIDGET a panel as wide as the form's fields, framed:
its lines after the first start where the first does, each runs to the
right edge."
  (let ((overlay (widget-get widget :field-overlay))
        (group (widget-get widget :lispm-form-group)))
    (mapc #'delete-overlay (widget-get widget :lispm-form-pads))
    (when-let* ((tail (widget-get widget :lispm-form-tail)))
      (delete-overlay tail))
    (when (overlayp overlay)
      (let* ((sides (lispm-ui-form--sides group))
             (start (overlay-start overlay))
             (last (or (widget-get widget :field-end-overlay)
                       (when (eq (char-after (overlay-end overlay)) ?\n)
                         (let ((tail (make-overlay (overlay-end overlay)
                                                   (1+ (overlay-end overlay)))))
                           (widget-put widget :lispm-form-tail tail)
                           tail))))
             (end (max (overlay-end overlay) (if (overlayp last) (or (overlay-end last) 0) 0)))
             pads)
        (dolist (overlay (list overlay last))
          (when (overlayp overlay)
            (overlay-put overlay 'line-prefix
                         (concat (car sides) (lispm-ui-form--at :control 1) (lispm-ui-form--edge)))
            (overlay-put overlay 'wrap-prefix
                         (concat (car sides) (lispm-ui-form--at :control 1) (lispm-ui-form--edge)))))
        (when (overlayp last)
          (overlay-put last 'after-string nil))
        (save-excursion
          (goto-char start)
          (while (search-forward "\n" end t)
            (let ((pad (make-overlay (1- (point)) (point))))
              (overlay-put pad 'before-string
                           (concat (propertize (lispm-ui-form--at :right nil 'lispm-ui-form-area) 'cursor t)
                                   (lispm-ui-form--edge)
                                   (or (cdr sides) "")))
              (overlay-put pad 'face 'default)
              (overlay-put pad 'priority 10)
              (push pad pads))))
        (widget-put widget :lispm-form-pads pads)))))

(defun lispm-ui-form-choose (label choices value event)
  "Show CHOICES, a list of (VALUE . TEXT), as LABEL's; return (VALUE) chosen.
A menu for EVENT, if one can pop up, else Emacs's own way for the
keyboard (see `widget-choose')."
  (if (lispm-ui-popup-available-p)
      (lispm-ui-choose choices value
                       (or (and (mouse-event-p event) (posn-point (event-start event)))
                           (point))
                       label)
    (widget-choose label
                   (mapcar (lambda (choice) (cons (cdr choice) (list (car choice)))) choices)
                   event)))

(defun lispm-ui-form--picture-p (file)
  "Non-nil if FILE is a picture: one Emacs shows, or named as pictures are.
An Emacs without a display shows none, but keeps them all the same."
  (or (image-supported-file-p file)
      (member (downcase (or (file-name-extension file) "")) '("png" "jpg" "jpeg" "gif" "svg" "webp"))))

(defun lispm-ui-form-choose-file (type label file predicate)
  "Show the files a form's field of TYPE and LABEL may choose, FILE chosen
now, those PREDICATE takes, with `lispm-ui-choose-file'; return the file
chosen, or nil.  A picture's are titled so, the picture selected
previewed beside them.  This is the default
`lispm-ui-form-choose-file-function'."
  (ignore label)
  (lispm-ui-choose-file :title (if (eq type 'picture)
                                   (lispm-ui-tr "Choose a Picture")
                                 (lispm-ui-tr "Choose a File"))
                        :file file :predicate predicate :preview (eq type 'picture)))

(defun lispm-ui-form--read-picture (old &optional label)
  "Choose the file of a picture for the field LABEL, from OLD's directory:
among the files `lispm-ui-form-choose-file-function' shows, or in the
minibuffer.  Return it, or nil when none was chosen."
  (when-let* ((file (if lispm-ui-form-choose-file-function
                        (funcall lispm-ui-form-choose-file-function 'picture label old #'lispm-ui-form--picture-p)
                      (read-file-name (lispm-ui-tr "Picture: ")
                                      (and old (file-name-directory old)) nil t nil
                                      (lambda (file)
                                        (or (file-directory-p file) (lispm-ui-form--picture-p file)))))))
    (unless (and (file-regular-p file) (lispm-ui-form--picture-p file))
      (user-error "%s" (lispm-ui-tr "Not a picture: %s" (abbreviate-file-name file))))
    (expand-file-name file)))

(provide 'lispm-ui-form-widgets)
;;; lispm-ui-form-widgets.el ends here
