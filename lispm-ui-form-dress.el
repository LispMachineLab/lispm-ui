;;; lispm-ui-form-dress.el --- Dressing forms and modal prompt dialogs  -*- lexical-binding: t; -*-

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

;; Visual dressing (`lispm-ui-form-dress', `lispm-ui-form-glyph',
;; `lispm-ui-form-group') and modal prompt dialogs (`lispm-ui-form-prompt')
;; for `lispm-ui-form'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'map)
(require 'image)
(require 'svg)
(require 'face-remap)
(require 'wid-edit)
(require 'lispm-ui-base)
(require 'lispm-ui-i18n)
(require 'lispm-ui-palette)
(require 'lispm-ui-draw)
(require 'lispm-ui-controls)
(require 'lispm-ui-ask)

(declare-function global-hl-line-unhighlight "hl-line" ())
(defvar global-hl-line-mode)
(defvar hl-line-mode)
(defvar lispm-ui-form-drawn-hook)

(declare-function lispm-ui-form "lispm-ui-form" (title fields &rest args))
(declare-function lispm-ui-form-geometry "lispm-ui-form" (&optional buffer))
(declare-function lispm-ui-form-value "lispm-ui-form" (key))
(declare-function lispm-ui-form-modified-p "lispm-ui-form" (&optional buffer))
(declare-function lispm-ui-form-set "lispm-ui-form" (key value &rest more))
(declare-function lispm-ui-form-redraw "lispm-ui-form" ())
(declare-function lispm-ui-form-cancel "lispm-ui-form" ())
(declare-function lispm-ui-form--first-field "lispm-ui-form" ())
(declare-function lispm-ui-form--keep-point "lispm-ui-form-widgets" ())
(declare-function lispm-ui--save-file-in-minibuffer "lispm-ui-ask" (title directory name))
(declare-function evil-normal-state "ext:evil-states" (&optional arg))
(defvar evil-local-mode)
(defvar evil-state)
(defvar lispm-ui-form--props)

;;;; Dressing

(defvar-local lispm-ui-form--focus nil
  "What of the form in this buffer is drawn focused: (START END PLAIN
FOCUSED) for a control, the text from START to END showing its image
FOCUSED in place of PLAIN; (field . OVERLAY) for a text field; or nil.")

(defvar-local lispm-ui-form--remaps nil
  "The face remappings dressing the form in this buffer, to undo.")

(defface lispm-ui-form-text-area '((t :extend t))
  "Face of a text area of several lines of a form not LispM UI's: a panel
of the content's colour."
  :group 'lispm-ui)

(defconst lispm-ui-form-well-size 64
  "Width and height in pixels of the picture in a form's image well.")

(defvar lispm-ui-form-control-properties '(:lispm-form-control)
  "The properties of a widget saying what control of a form it is, as
`lispm-ui-form-glyph-function' gives them: LispM UI's forms', and those
of applications drawing forms of their own the same way, which add
theirs.")

(defun lispm-ui-form--image (kind state label &optional focused pressed)
  "Return the image of a form's control of KIND in STATE reading LABEL, ringed
when FOCUSED, pushed in when PRESSED; nil without a palette, or on a
text terminal, where the form draws it as text."
  (when (and lispm-ui-current-palette (not (lispm-ui-tty-p)))
    (if (eq kind 'picture)
        (lispm-ui-form--well label)
      (lispm-ui-image-of
       (pcase kind
         ('check (lispm-ui-check-box (eq state 'on) (and pressed 'pressed) focused 2))
         ('radio (lispm-ui-radio (eq state 'on) (and pressed 'pressed) focused))
         ('button (lispm-ui-push-button label :default (eq state 'default)
                                        :state (and pressed 'pressed) :focused focused))
         ('popup (lispm-ui-popup-button label :state (and pressed 'pressed) :focused focused)))))))

(defun lispm-ui-form--well (file)
  "Return an image well holding the picture in FILE, or empty for nil: the
picture framed in a well sunk into the window, as Mac OS 9 drew one."
  (lispm-ui-image-well file lispm-ui-form-well-size))

(defun lispm-ui-form-glyph (kind state label)
  "Return the image of a form's control, LispM UI's, or nil for text.
KIND, STATE and LABEL are as `lispm-ui-form-glyph-function' gives them.
Without a palette, or on a text terminal, it is nil.  Emacs's widgets
centre a check box's or a radio button's image on its line themselves,
and an image saying so twice is no image."
  (when-let* ((image (lispm-ui-form--image kind state label)))
    (if (memq kind '(check radio))
        (cons 'image (map-delete (copy-sequence (cdr image)) :ascent))
      image)))

(defun lispm-ui-form-group (part &rest args)
  "Return PART of a form's group box, LispM UI's, a string; nil without a
palette.  PART and ARGS are as `lispm-ui-form-group-function' gives
them: the top edge, titled, and the bottom one are LispM UI's group
box's; a side is its grey line and its lit one, as tall as the line they
stand on.  On a text terminal they are drawn in lines of text."
  (when lispm-ui-current-palette
    (pcase part
      ('top (lispm-ui-group-edge (car args) (cadr args)))
      ('bottom (lispm-ui-group-edge (car args) nil t))
      ('side (if (lispm-ui-tty-p)
                 (lispm-ui-group-side 1)
               (let ((palette lispm-ui-current-palette))
                 (concat (propertize " " 'display '(space :width (1))
                                     'face `(:background ,(lispm-ui-grey palette #x88)))
                         (propertize " " 'display '(space :width (1))
                                     'face `(:background ,(lispm-ui-grey palette #xFF))))))))))

(defun lispm-ui-form--control-at (position)
  "The control of the form at POSITION drawn as an image, as (START END
PLAIN FOCUSED PRESSED): the text it is drawn over, and its images; or nil."
  (when-let* ((widget (widget-at position))
              (control (seq-some (lambda (property) (widget-get widget property))
                                 lispm-ui-form-control-properties))
              (overlay (widget-get widget :button-overlay))
              ((overlayp overlay))
              (start (overlay-start overlay))
              ((consp (get-text-property start 'display))))
    (pcase-let ((`(,kind ,state ,label)
                 (if (consp control)
                     control
                   (list control (if (widget-value widget) 'on 'off) nil))))
      (when-let* ((plain (lispm-ui-form--image kind state label))
                  (focused (lispm-ui-form--image kind state label t))
                  (pressed (lispm-ui-form--image kind state label nil t)))
        (list start (overlay-end overlay) plain focused pressed)))))

(defun lispm-ui-form--show (start end image)
  "Show IMAGE over the text from START to END, quietly."
  (with-silent-modifications
    (let ((inhibit-read-only t))
      (put-text-property start end 'display image))))

(defun lispm-ui-form--field-overlay (position)
  "The overlay of the form's text field of one line at POSITION, or nil.
A text area has no frame to ring."
  (when-let* ((field (widget-field-at position))
              ((not (eq (widget-type field) 'text))))
    (let ((overlay (widget-get field :field-overlay)))
      (and (overlayp overlay) overlay))))

(defun lispm-ui-form--follow ()
  "Draw focused what of the form in this buffer point is on, and plain what
was before: a control ringed, a text field framed in the focus colour.
Only a text field keeps the cursor when the form is dressed in images.
Run from the buffer's `post-command-hook', and once the form is drawn."
  (setq-local cursor-type
              (if (and lispm-ui-current-palette (not (lispm-ui-tty-p))
                       (not (widget-field-find (point))))
                  nil
                (default-value 'cursor-type)))
  (let* ((control (lispm-ui-form--control-at (point)))
         (field (and (not control) (lispm-ui-form--field-overlay (point))))
         (now (cond (control control)
                    (field (cons 'field field)))))
    (unless (equal now lispm-ui-form--focus)
      (pcase lispm-ui-form--focus
        (`(field . ,overlay)
         (when (overlay-buffer overlay)
           (overlay-put overlay 'face 'widget-field)))
        (`(,start ,end ,plain . ,_)
         (when (<= end (point-max))
           (lispm-ui-form--show start end plain))))
      (pcase now
        (`(field . ,overlay)
         (overlay-put overlay 'face 'lispm-ui-field-focused))
        (`(,start ,end ,_ ,focused . ,_) (lispm-ui-form--show start end focused)))
      (setq lispm-ui-form--focus now))))

(defun lispm-ui-form--faces ()
  "Remap the faces of the form in this buffer to the palette: the list face
on the chrome's ground, the fields framed in the content's colour, a text
area in it, framed in grey, the help small and muted.  LispM UI's forms
take them before they are drawn, to be measured in them.  Without a
palette, or on a text terminal, the faces are left as they are."
  (mapc #'face-remap-remove-relative lispm-ui-form--remaps)
  (setq lispm-ui-form--remaps
        (when (and lispm-ui-current-palette (not (lispm-ui-tty-p)))
          (let ((content (lispm-ui-color :content)))
            (list (face-remap-add-relative 'default 'lispm-ui-list
                                           `(:background ,(lispm-ui-color :chrome)))
                  (face-remap-add-relative 'widget-field 'lispm-ui-field)
                  (face-remap-add-relative 'lispm-ui-form-area `(:background ,content :foreground
                                                                ,(lispm-ui-color :text)))
                  (face-remap-add-relative 'lispm-ui-form-area-edge
                                           `(:background ,(lispm-ui-grey lispm-ui-current-palette #x88)))
                  (face-remap-add-relative 'lispm-ui-form-text-area `(:background ,content))
                  (face-remap-add-relative 'lispm-ui-form-help 'lispm-ui-small
                                           `(:foreground ,(lispm-ui-color :muted)))
                  (face-remap-add-relative 'lispm-ui-form-title 'lispm-ui-title))))))

(defun lispm-ui-form--text-areas ()
  "Draw the text areas of the form in this buffer as panels of the
content's colour, their lines after the first lined up with it: a form
not LispM UI's, which draws its own."
  (dolist (field widget-field-list)
    (when-let* (((eq (car field) 'text))
                (overlay (widget-get field :field-overlay))
                ((overlayp overlay)))
      (overlay-put overlay 'face 'lispm-ui-form-text-area)
      (overlay-put overlay 'line-prefix "  ")
      ;; Its last newline is in an overlay of its own.
      (when-let* ((end (widget-get field :field-end-overlay))
                  ((overlayp end)))
        (overlay-put end 'face 'lispm-ui-form-text-area)
        (overlay-put end 'line-prefix "  ")))))

(defun lispm-ui-form-dress ()
  "Dress the form in this buffer, just drawn, once LispM UI has a palette:
what point is on drawn focused, as the keyboard moves.  A form not LispM
UI's, drawn with Emacs's widgets as an application's settings are, is
put in the palette's colours too, its text areas panels.  Run
from `lispm-ui-form-drawn-hook', and from an application's like it."
  (when lispm-ui-current-palette
    (unless (derived-mode-p 'lispm-ui-form-mode)
      (setq-local global-hl-line-mode nil)
      (when (bound-and-true-p hl-line-mode)
        (hl-line-mode -1))
      (when (fboundp 'global-hl-line-unhighlight)
        (global-hl-line-unhighlight))
      (lispm-ui-form--faces)
      (lispm-ui-form--text-areas))
    (add-hook 'post-command-hook #'lispm-ui-form--follow nil t)
    ;; What was focused was drawn over.
    (setq lispm-ui-form--focus nil)
    (lispm-ui-form--follow)))

(defun lispm-ui-form-redraw-all (&rest _)
  "Draw every form again, in the palette and the language now in use."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'lispm-ui-form-mode)
        (lispm-ui-form--faces)
        (lispm-ui-form-redraw)))))

;;;; Modal prompt forms

(defconst lispm-ui-form-choice-buttons-supported t
  "Non-nil when `choice' fields support inline `:buttons' and `:buttons-key'.")

(defvar lispm-ui-form-modal-show-function #'lispm-ui-form--modal-show-default
  "Function showing a modal form's buffer and returning a cleanup function.
Called with BUFFER, it displays BUFFER in a selected window or frame and
returns a function of no arguments that closes it when the prompt ends.")

(defun lispm-ui-form--modal-tty-dimensions (buffer parent)
  "Return (COLS . ROWS) cell dimensions for modal BUFFER on TTY PARENT."
  (let ((max-cols 0)
        (rows 0))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (setq max-cols (max max-cols
                              (string-width (buffer-substring-no-properties
                                             (line-beginning-position)
                                             (line-end-position))))
                rows (1+ rows))
          (forward-line 1))))
    (cons (min (max 20 (+ max-cols 4))
               (max 20 (- (frame-width parent) 2)))
          (min (max 6 (+ rows 2))
               (max 6 (- (frame-height parent) 2))))))

(defun lispm-ui-form--modal-show-default (buffer)
  "Show modal form BUFFER in a child frame or window; return a cleanup function."
  (if (and (not noninteractive)
           (or (and (display-graphic-p) (not (lispm-ui-tty-p)))
               (and (not (display-graphic-p)) (featurep 'tty-child-frames))))
      (let* ((prev-frame (selected-frame))
             (parent prev-frame))
        (while (frame-parent parent)
          (setq parent (frame-parent parent)))
        (let* ((tty (lispm-ui-tty-p parent))
               (tty-dims (and tty (lispm-ui-form--modal-tty-dimensions buffer parent)))
               (geom (unless tty (lispm-ui-form-geometry buffer)))
               (outer (or (plist-get geom :outer) 420))
               (width (if tty (car tty-dims) (+ outer 40)))
               (height (if tty
                           (cdr tty-dims)
                         (max 140
                              (with-current-buffer buffer
                                (let ((truncate-lines t))
                                  (+ (cdr (buffer-text-pixel-size buffer nil t)) 40))))))
               (frame (let ((after-make-frame-functions nil)
                            (before-make-frame-hook nil))
                        (make-frame
                         `((parent-frame . ,parent)
                           (minibuffer . nil)
                           (undecorated . ,(not tty))
                           (unsplittable . t) (no-other-frame . t)
                           (child-frame-border-width . 1) (internal-border-width . 0)
                           (left-fringe . 0) (right-fringe . 0)
                           (vertical-scroll-bars . nil) (horizontal-scroll-bars . nil)
                           (menu-bar-lines . 0) (tool-bar-lines . 0) (tab-bar-lines . 0)
                           (desktop-dont-save . t) (visibility . nil)
                           (background-color . ,(or (lispm-ui-color :chrome)
                                                    (frame-parameter parent 'background-color)))
                           (foreground-color . ,(or (lispm-ui-color :text)
                                                    (frame-parameter parent 'foreground-color)))
                           (width . ,(if tty width 50))
                           (height . ,(if tty height 10))))))
               (root (frame-root-window frame)))
          (set-window-buffer root buffer)
          (set-window-dedicated-p root t)
          (set-window-fringes root 0 0)
          (if tty
              (progn
                (set-frame-size frame width height)
                (set-frame-position frame
                                    (max 0 (/ (- (frame-width parent) width) 2))
                                    (max 0 (/ (- (frame-height parent) height) 3))))
            (let ((frame-resize-pixelwise t))
              (set-frame-size frame width height t))
            (set-frame-position frame
                                (max 0 (/ (- (frame-pixel-width parent) width) 2))
                                (max 0 (/ (- (frame-pixel-height parent) height) 3))))
          (make-frame-visible frame)
          (select-frame-set-input-focus frame)
          (select-window root)
          (lambda ()
            (when (frame-live-p frame)
              (delete-frame frame t))
            (cond ((frame-live-p prev-frame)
                   (select-frame-set-input-focus prev-frame))
                  ((frame-live-p parent)
                   (select-frame-set-input-focus parent))))))
    (let ((config (current-window-configuration)))
      (pop-to-buffer buffer)
      (lambda ()
        (when (window-configuration-p config)
          (set-window-configuration config))))))

(defalias 'lispm-ui-form-modal-show-default #'lispm-ui-form--modal-show-default)

(defun lispm-ui-form--prompt-choose-dir (title start)
  "Open `lispm-ui-choose-file' for a folder starting at START, preserving focus."
  (let ((modal-frame (selected-frame)))
    (condition-case nil
        (unwind-protect
            (lispm-ui-choose-file
             :title title
             :directory (if (file-directory-p start)
                            (file-name-as-directory start)
                          (file-name-as-directory (expand-file-name "~")))
             :predicate #'file-directory-p)
          (when (frame-live-p modal-frame)
            (select-frame-set-input-focus modal-frame)))
      (quit nil))))

(defun lispm-ui-form--prompt-fields (title init-val password choice-pairs require-match validate choose-directory browse-buttons on-preset &optional on-change)
  "Construct the field specification list for `lispm-ui-form-prompt'.
TITLE, INIT-VAL, PASSWORD, CHOICE-PAIRS, REQUIRE-MATCH, VALIDATE,
CHOOSE-DIRECTORY, BROWSE-BUTTONS, ON-PRESET and ON-CHANGE configure the
field widgets."
  (let ((val-type (if password 'password 'text))
        (change-prop (when on-change (list :on-change on-change))))
    (cond
     (choose-directory
      `((:value :type ,val-type :label ,title
                :value ,init-val :submit t :validate ,validate ,@change-prop)
        ,@(if choice-pairs
              `((:preset :type choice :label ,(lispm-ui-tr "Choose")
                         :value ,(and (assoc init-val choice-pairs) init-val)
                         :choices ,choice-pairs
                         :buttons ,browse-buttons
                         :buttons-key :browse
                         :on-change ,on-preset))
            `((:browse :type buttons :buttons ,browse-buttons)))))
     ((and choice-pairs require-match)
      `((:value :type choice :label ,title
                :value ,init-val
                :choices ,choice-pairs
                :validate ,validate
                ,@change-prop)))
     (choice-pairs
      `((:preset :type choice :label ,(lispm-ui-tr "Choose")
                 :value ,(and (assoc init-val choice-pairs) init-val)
                 :choices ,choice-pairs
                 :on-change ,on-preset)
        (:value :type ,val-type :label ,title
                :value ,init-val :submit t :validate ,validate ,@change-prop)))
     (t
      `((:value :type ,val-type :label ,title
                :value ,init-val :submit t :validate ,validate ,@change-prop))))))

(defun lispm-ui-form--evil-editing-state-p ()
  "Return non-nil when Evil mode is active in insert or replace state."
  (and (bound-and-true-p evil-local-mode)
       (memq (bound-and-true-p evil-state) '(insert replace))
       (fboundp 'evil-normal-state)))

(defun lispm-ui-form-escape ()
  "Handle `<escape>' in a form without accidental data loss or Evil trap.
In Evil insert or replace state, switch to normal state instead of
cancelling.  Otherwise confirm before discarding modified user input."
  (interactive)
  (cond
   ((lispm-ui-form--evil-editing-state-p)
    (evil-normal-state))
   ((or noninteractive
        (plist-get lispm-ui-form--props :instant)
        (not (lispm-ui-form-modified-p))
        (lispm-ui-confirm (lispm-ui-tr "Discard changes?")))
    (lispm-ui-form-cancel))))

(defun lispm-ui-form--setup-modal-keymap ()
  "Bind `keyboard-quit', `<escape>', and `ESC ESC' for modal form buffers."
  (use-local-map (copy-keymap (current-local-map)))
  (local-set-key [remap keyboard-quit] #'lispm-ui-form-cancel)
  (local-set-key (kbd "<escape>") #'lispm-ui-form-escape)
  (local-set-key (kbd "ESC ESC") #'lispm-ui-form-cancel))

(defun lispm-ui-form--prompt-browse-buttons (title init-val base-dir choice-pairs buffer-fn)
  "Return the `Choose…' button spec for `lispm-ui-form-prompt'."
  `((,(lispm-ui-tr "Choose…") .
     ,(lambda ()
        (let ((buffer (funcall buffer-fn)))
          (when (buffer-live-p buffer)
            (let* ((current (with-current-buffer buffer
                              (or (lispm-ui-form-value :value) init-val)))
                   (start (if (and (stringp current)
                                   (not (string-empty-p (string-trim current))))
                              (expand-file-name (string-trim current) base-dir)
                            base-dir))
                   (chosen (lispm-ui-form--prompt-choose-dir title start)))
              (when (and chosen (buffer-live-p buffer))
                (let ((abbr (abbreviate-file-name
                             (file-name-as-directory (expand-file-name chosen base-dir)))))
                  (with-current-buffer buffer
                    (if choice-pairs
                        (lispm-ui-form-set :value abbr
                                           :preset (and (assoc abbr choice-pairs) abbr))
                      (lispm-ui-form-set :value abbr))))))))))))

(defun lispm-ui-form--modal-redirect-focus-p (closing modal-frame orig-frame buffer modal-depth)
  "Return non-nil when focus strayed to ORIG-FRAME while MODAL-FRAME owns input."
  (and (not closing)
       modal-frame
       (frame-live-p modal-frame)
       (frame-visible-p modal-frame)
       (buffer-live-p buffer)
       (= (recursion-depth) modal-depth)
       (null (active-minibuffer-window))
       (eq (selected-frame) orig-frame)))

(defun lispm-ui-form--make-modal-exiter (depth on-done)
  "Return modal exit closure for recursion DEPTH invoking ON-DONE."
  (lambda (ok &optional val from-kill)
    (if (and (not from-kill) (> (recursion-depth) (1+ depth)))
        (user-error "%s" (lispm-ui-tr "Finish the active prompt first"))
      (funcall on-done ok val)
      (when (= (recursion-depth) (1+ depth))
        (exit-recursive-edit)))))

(defun lispm-ui-form--run-modal-buffer (buffer)
  "Show modal form BUFFER, run `recursive-edit', and clean up on exit."
  (let ((orig-frame (selected-frame))
        (modal-frame nil)
        (modal-depth (1+ (recursion-depth)))
        (cleanup nil)
        (closing nil)
        (on-delete-frame nil)
        (on-selection-change nil))
    (setq on-delete-frame
          (lambda (frame)
            (when (and (not closing)
                       modal-frame
                       (eq frame modal-frame)
                       (buffer-live-p buffer))
              (with-current-buffer buffer
                (lispm-ui-form-cancel)))))
    (unwind-protect
        (progn
          (setq cleanup (funcall lispm-ui-form-modal-show-function buffer))
          (let ((shown-frame (selected-frame)))
            (unless (eq shown-frame orig-frame)
              (setq modal-frame shown-frame
                    on-selection-change
                    (lambda (_frame)
                      (when (lispm-ui-form--modal-redirect-focus-p
                             closing modal-frame orig-frame buffer modal-depth)
                        (raise-frame modal-frame)
                        (select-frame-set-input-focus modal-frame)
                        (let ((root (frame-root-window modal-frame)))
                          (when (window-live-p root)
                            (select-window root))))))
              (add-hook 'delete-frame-functions on-delete-frame)
              (add-hook 'window-selection-change-functions on-selection-change)))
          (with-current-buffer buffer
            (lispm-ui-form--first-field)
            (lispm-ui-form--keep-point)
            (when lispm-ui-current-palette
              (lispm-ui-form--follow)))
          (lispm-ui-without-gui-action
            (recursive-edit)))
      (setq closing t)
      (remove-hook 'delete-frame-functions on-delete-frame)
      (when on-selection-change
        (remove-hook 'window-selection-change-functions on-selection-change))
      (when (functionp cleanup)
        (ignore-errors (funcall cleanup)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(cl-defun lispm-ui-form-modal (title fields &key values submit cancel intro setup)
  "Open a modal `lispm-ui-form' of TITLE and FIELDS; return submitted values.
VALUES, SUBMIT, CANCEL, INTRO and SETUP are as `lispm-ui-form' takes
them.  Cancelling or closing the buffer signals `quit'."
  (if lispm-ui-form-modal-function
      (funcall lispm-ui-form-modal-function title fields
               :values values :submit submit :cancel cancel
               :intro intro :setup setup)
    (let* ((submitted nil)
           (closing nil)
           (result nil)
           (buffer nil)
           (depth (recursion-depth))
           (exit-with (lispm-ui-form--make-modal-exiter
                       depth
                       (lambda (ok vals)
                         (setq closing t
                               submitted ok
                               result vals)))))
      (setq buffer
            (lispm-ui-form
             title fields
             :values values :submit submit :cancel cancel :intro intro :modal t
             :on-submit (lambda (vals) (funcall exit-with t vals))
             :on-cancel (lambda () (funcall exit-with nil))
             :setup (lambda ()
                      (lispm-ui-form--setup-modal-keymap)
                      (when setup (funcall setup)))))
      (with-current-buffer buffer
        (add-hook 'kill-buffer-hook
                  (lambda () (unless closing (funcall exit-with nil nil t)))
                  nil t))
      (unwind-protect
          (lispm-ui-form--run-modal-buffer buffer)
        (setq closing t))
      (unless submitted
        (signal 'quit nil))
      result)))

(cl-defun lispm-ui-form-prompt (prompt &key initial default password choices
                                       require-match validate choose-directory
                                       on-change)
  "Ask PROMPT in a modal `lispm-ui-form' dialog and return the string entered.
INITIAL is the initial text in the field; DEFAULT is returned when the
field is submitted empty.  PASSWORD masks the input as stars.  CHOICES,
a list of strings, offers a pop-up menu of presets above the field (or
alone when REQUIRE-MATCH is non-nil).  VALIDATE, a function of the
value returning an error string or nil, checks the value before
submitting.  When CHOOSE-DIRECTORY is non-nil, include a `Choose…'
button that opens `lispm-ui-choose-file' for a folder.  ON-CHANGE, if
non-nil, is called with the field's value as it changes.  Cancelling
signals `quit'."
  (if lispm-ui-form-prompt-function
      (apply lispm-ui-form-prompt-function prompt
             `(:initial ,initial :default ,default :password ,password
               :choices ,choices :require-match ,require-match :validate ,validate
               ,@(when choose-directory `(:choose-directory ,choose-directory))
               ,@(when on-change `(:on-change ,on-change))))
    (let* ((clean (replace-regexp-in-string "[ \t]*:?[ \t]*\\'" "" (or prompt "")))
           (title (if (string-empty-p clean) (lispm-ui-tr "Choose") clean))
           (base-dir (file-name-as-directory (expand-file-name default-directory)))
           (choice-pairs (mapcar (lambda (c)
                                   (let ((text (format "%s" (if (consp c) (car c) c))))
                                     (cons text text)))
                                 choices))
           (init-val (cond ((and (stringp initial) (not (string-empty-p initial))) initial)
                           ((and (stringp default) (not (string-empty-p default))) default)
                           ((and require-match choice-pairs) (caar choice-pairs))
                           (t "")))
           (submitted nil)
           (closing nil)
           (result nil)
           (buffer nil)
           (depth (recursion-depth))
           (exit-with (lispm-ui-form--make-modal-exiter
                       depth
                       (lambda (ok val)
                         (setq closing t
                               submitted ok
                               result val))))
           (on-preset (lambda (val)
                        (when (and val (buffer-live-p buffer))
                          (with-current-buffer buffer
                            (lispm-ui-form-set :value val))
                          (when on-change
                            (funcall on-change val)))))
           (browse-buttons
            (when choose-directory
              (lispm-ui-form--prompt-browse-buttons
               title init-val base-dir choice-pairs (lambda () buffer))))
           (fields (lispm-ui-form--prompt-fields
                    title init-val password choice-pairs require-match
                    validate choose-directory browse-buttons on-preset on-change)))
      (setq buffer
            (lispm-ui-form
             title fields
             :modal t
             :on-submit (lambda (values)
                          (funcall exit-with t
                                   (or (alist-get :value values)
                                       (and (stringp default) default)
                                       "")))
             :on-cancel (lambda () (funcall exit-with nil))
             :setup #'lispm-ui-form--setup-modal-keymap))
      (with-current-buffer buffer
        (add-hook 'kill-buffer-hook
                  (lambda () (unless closing (funcall exit-with nil nil t)))
                  nil t))
      (unwind-protect
          (lispm-ui-form--run-modal-buffer buffer)
        (setq closing t))
      (unless submitted
        (signal 'quit nil))
      result)))

(cl-defun lispm-ui-save-file-default (&key title directory name)
  "Ask in a modal form where to save a file; return the path, or nil.
TITLE titles the dialog, DIRECTORY is the initial folder, and NAME is
the initial file name.  Falls back to the minibuffer when popups cannot
be shown."
  (if (not (or lispm-ui-form-modal-function (lispm-ui-popup-available-p)))
      (lispm-ui--save-file-in-minibuffer
       (or title (lispm-ui-tr "Save File")) directory name)
    (let* ((base-dir (file-name-as-directory
                      (expand-file-name (or directory default-directory))))
           (dialog-title (or title (lispm-ui-tr "Save File")))
           (form-buffer nil)
           (browse-buttons
            `((,(lispm-ui-tr "Choose…") .
               ,(lambda ()
                  (when (buffer-live-p form-buffer)
                    (let* ((cur (with-current-buffer form-buffer
                                  (lispm-ui-form-value :directory)))
                           (start (if (and (stringp cur)
                                           (not (string-empty-p (string-trim cur))))
                                      (expand-file-name (string-trim cur) base-dir)
                                    base-dir))
                           (chosen (lispm-ui-form--prompt-choose-dir
                                    dialog-title start)))
                      (when (and chosen (buffer-live-p form-buffer))
                        (with-current-buffer form-buffer
                          (lispm-ui-form-set
                           :directory
                           (abbreviate-file-name
                            (file-name-as-directory
                             (expand-file-name chosen base-dir))))))))))))
           (fields
            `((:name :type text :label ,(lispm-ui-tr "Save As")
                     :value ,(or name "") :required t :submit t)
              (:directory :type text :label ,(lispm-ui-tr "Where")
                          :value ,(abbreviate-file-name base-dir)
                          :validate ,(lambda (val)
                                       (let ((dir (and (stringp val)
                                                       (not (string-empty-p (string-trim val)))
                                                       (expand-file-name (string-trim val) base-dir))))
                                         (unless (and dir (file-directory-p dir))
                                           (lispm-ui-tr "Choose an existing folder")))))
              (:browse :type buttons :buttons ,browse-buttons)))
           (values (condition-case nil
                       (lispm-ui-form-modal
                        dialog-title fields
                        :submit (lispm-ui-tr "Save")
                        :setup (lambda () (setq form-buffer (current-buffer))))
                     (quit nil))))
      (when values
        (let* ((dir (expand-file-name (or (alist-get :directory values) base-dir) base-dir))
               (file-name (alist-get :name values)))
          (expand-file-name file-name (file-name-as-directory dir)))))))

(provide 'lispm-ui-form-dress)
;;; lispm-ui-form-dress.el ends here
