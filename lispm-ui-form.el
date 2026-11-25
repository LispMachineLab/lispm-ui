;;; lispm-ui-form.el --- Forms, written as data  -*- lexical-binding: t; -*-

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

;; A form is written as data: a title and a list of fields, each a key
;; and a plist.
;;
;;   (lispm-ui-form "New Character"
;;     '((title :type text :label "Name" :required t)
;;       (avatar :type picture :label "Picture")
;;       (soul :type text-area :label "Soul" :lines 8)
;;       (thinks :type radio :label "Model"
;;               :choices ((lispm . "LispM's") (endpoint . "Its own")))
;;       (url :type text :label "URL" :required t
;;            :when (lambda (values) (eq (alist-get 'thinks values) 'endpoint))))
;;     :submit "Create"
;;     :on-submit (lambda (values) ...))
;;
;; Fields are backed by per-field `lispm-ui-signal' cells and a derived
;; reactive graph (`lispm-ui-frp.el') with marker-based slot
;; reconciliation (`lispm-ui-form--places') for in-place status updates
;; and glitch-free transactional batching across `:on-change' callbacks.
;;
;; Widget shaping, password masking and mouse handling live in
;; `lispm-ui-form-widgets.el'; visual dressing and modal prompt dialogs
;; live in `lispm-ui-form-dress.el'.

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
(require 'lispm-ui-frp)
(require 'lispm-ui-i18n)
(require 'lispm-ui-palette)
(require 'lispm-ui-modal)
(require 'lispm-ui-popup)
(require 'lispm-ui-draw)
(require 'lispm-ui-controls)

;; Only in an Emacs that draws images.
(declare-function image-size "image.c" (spec &optional pixels frame))

(defconst lispm-ui-form-types
  '(text password text-area picture choice radio check buttons rows status heading)
  "The types a form's fields may be, see `lispm-ui-form'.
A program written for a later LispM UI may ask whether one it uses is here.")

(defvar lispm-ui-form-glyph-function #'lispm-ui-form-glyph
  "Function giving the picture of a control of a form, or nil for text.
It is called with KIND, STATE and LABEL:
  KIND `check' or `radio', STATE `on' or `off', LABEL nil;
  KIND `button', STATE nil, or `default' for the button C-c C-c
       presses, LABEL its label;
  KIND `popup', a pop-up menu, STATE nil, LABEL what it shows;
  KIND `picture', STATE nil, LABEL the picture's file, or nil for none.
It returns an image, or nil for the control drawn as text.  By default
it is LispM UI's control, once LispM UI has a palette.")

(defvar lispm-ui-form-group-function #'lispm-ui-form-group
  "Function giving the edges of a form's group boxes, or nil for none.
A group is the fields after a heading, up to the next.  It is called
with the PART wanted:
  `top', with WIDTH, in pixels, and TITLE: the top edge, titled;
  `bottom', with WIDTH: the bottom edge;
  `side': a side, as tall as the line it is on.
It returns a string to insert, as one displaying an image, or nil: then
the heading is its title in bold, and the group has no frame.  By
default it is LispM UI's group box, once LispM UI has a palette.")

(defvar lispm-ui-form-choose-function #'lispm-ui-form-choose
  "Function showing the choices of a pop-up menu of a form; it returns one.
It is called with the field's LABEL, its CHOICES, a list of (VALUE .
TEXT), the VALUE chosen now, and the EVENT that pressed the menu, if
any, with point on the menu.  It returns (VALUE), the choice made, or
nil when none was.")

(defvar lispm-ui-form-choose-file-function #'lispm-ui-form-choose-file
  "Function showing files to choose one of for a form's field, or nil.
It is called with the field's TYPE, `picture', its LABEL, the FILE
chosen now, or nil, and PREDICATE, a function of a file's name saying
whether it may be chosen, as whether it is a picture.  It returns the
file chosen, or nil when none was.  By default, `lispm-ui-form-choose-file'
shows them, as `lispm-ui-choose-file' does; without one, the file is
read in the minibuffer.")

(defvar lispm-ui-form-drawn-hook nil
  "Normal hook run in a form's buffer each time it is drawn.")

(defcustom lispm-ui-form-width 34
  "Width of the fields of a form, in widths of the letter n."
  :type 'integer
  :group 'lispm-ui)

(defvar-local lispm-ui-form-show-header t
  "Non-nil when the form shows its title and its keys above its fields.
Where its window is titled with it, and its keys are listed elsewhere,
this is nil.")

(defface lispm-ui-form-title '((t :inherit bold :height 1.2))
  "Face of a form's title, and of its headings."
  :group 'lispm-ui)

(defface lispm-ui-form-label '((t :inherit bold))
  "Face of the label of a field of a form."
  :group 'lispm-ui)

(defface lispm-ui-form-help '((t :inherit shadow :height 0.9))
  "Face of the help under a field of a form."
  :group 'lispm-ui)

(defface lispm-ui-form-area '((t :inherit widget-field :box nil :extend nil
                                 :underline nil :overline nil :strike-through nil))
  "Face of a text area of a form, a panel of several lines: a text field's
colours, none of the lines a theme draws about one, which would rule each
of its lines."
  :group 'lispm-ui)

(defface lispm-ui-form-area-edge '((t :background "gray55"))
  "Face of the frame about a text area of a form: its background is its colour."
  :group 'lispm-ui)

(defvar-local lispm-ui-form--title nil
  "The title of the form in this buffer.")

(defvar-local lispm-ui-form--fields nil
  "The fields of the form in this buffer, as `lispm-ui-form' takes them.")

(defvar-local lispm-ui-form--values nil
  "The values of the form in this buffer: an alist (KEY . VALUE).")

(defvar-local lispm-ui-form--signals nil
  "Alist `(KEY . SIGNAL)` of reactive source signals for each field in this form.")

(defvar-local lispm-ui-form--all-values-signal nil
  "Derived `lispm-ui-signal' projecting normalized `(KEY . VALUE)` for all fields.")

(defvar-local lispm-ui-form--props nil
  "The properties the form in this buffer was made with.")

(defvar-local lispm-ui-form--widgets nil
  "Alist (KEY . WIDGET): the first widget of each field shown.")

(defvar-local lispm-ui-form--geometry nil
  "Where the form in this buffer is drawn: a plist of pixels.
:em, the width of the letter n; :left and :outer, the left and right
edges of its group boxes; :start and :label, where its labels start
and end; :control and :right, where its controls start and end.")

(defvar-local lispm-ui-form--field-at-point nil
  "The text field point was in after the last command, or nil.")

(defvar-local lispm-ui-form--places nil
  "Where the status fields of the form in this buffer are drawn, to be
drawn again there: an alist (KEY START END GROUP), START and END
markers, GROUP non-nil when the field is in a group box.")

(defvar-local lispm-ui-form--stale nil
  "Non-nil when the form in this buffer is to be drawn again, a value of
it having changed while it could not be.")

(defvar-local lispm-ui-form--stale-status nil
  "List of status keys queued for in-place redraw when `:on-change' returns.")

(defvar lispm-ui-form--changing nil
  "The buffer of the form whose field's :on-change is being called, or
nil: the form is drawn again once it returns, not while it runs.")

(require 'lispm-ui-form-widgets)
(require 'lispm-ui-form-dress)

(defun lispm-ui-form-title (&optional buffer)
  "Return the title of the form in BUFFER, by default the current buffer."
  (buffer-local-value 'lispm-ui-form--title (or buffer (current-buffer))))

(defun lispm-ui-form-geometry (&optional buffer)
  "Return where the form in BUFFER, by default the current buffer, is drawn:
a plist of pixels, see `lispm-ui-form--geometry'."
  (buffer-local-value 'lispm-ui-form--geometry (or buffer (current-buffer))))

(defun lispm-ui-form-widgets (&optional buffer)
  "Return the first widget of each field shown of the form in BUFFER, by
default the current buffer: an alist (KEY . WIDGET)."
  (buffer-local-value 'lispm-ui-form--widgets (or buffer (current-buffer))))

(defun lispm-ui-form-signal (key &optional buffer)
  "Return the reactive `lispm-ui-signal' for field KEY in BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (alist-get key lispm-ui-form--signals)))

(defvar-keymap lispm-ui-form-mode-map
  :doc "Keymap of `lispm-ui-form-mode'."
  :parent widget-keymap
  "C-c C-c" #'lispm-ui-form-submit
  "C-c C-k" #'lispm-ui-form-cancel
  "C-x h" #'lispm-ui-form-select-all
  "SPC" #'lispm-ui-form-activate-or-space
  "M-a" #'lispm-ui-form-select-field
  "s-a" #'lispm-ui-form-select-field
  "<down-mouse-1>" #'lispm-ui-form--mouse
  "<mouse-1>" #'lispm-ui-form--mouse
  "<drag-mouse-1>" #'lispm-ui-form--mouse
  "<double-down-mouse-1>" #'lispm-ui-form--mouse
  "<double-mouse-1>" #'lispm-ui-form--mouse
  "<double-drag-mouse-1>" #'lispm-ui-form--mouse
  "<triple-down-mouse-1>" #'lispm-ui-form--mouse
  "<triple-mouse-1>" #'lispm-ui-form--mouse
  "<triple-drag-mouse-1>" #'lispm-ui-form--mouse)

(define-derived-mode lispm-ui-form-mode nil "Form"
  "A form: fields to fill in, written as data (see `lispm-ui-form').
TAB and S-TAB go from field to field, \\[lispm-ui-form-submit] submits it,
\\[lispm-ui-form-cancel] cancels it.

\\{lispm-ui-form-mode-map}"
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (setq-local display-line-numbers nil)
  (when (bound-and-true-p display-line-numbers-mode)
    (display-line-numbers-mode -1))
  (setq-local fringe-indicator-alist
              (cons '(continuation nil nil) fringe-indicator-alist))
  ;; A dialog does not highlight the line point is on.
  (setq-local global-hl-line-mode nil)
  ;; A new line of a text area starts where its panel does, not indented
  ;; under the label beside its first.
  (electric-indent-local-mode -1)
  (setq-local yank-excluded-properties
              (let ((base (default-value 'yank-excluded-properties)))
                (append '(face display lispm-ui-form-padding)
                        (and (listp base) base))))
  (setq-local filter-buffer-substring-function #'lispm-ui-form--filter-substring)
  (add-hook 'pre-command-hook #'lispm-ui-form--replace-marked nil t)
  (add-hook 'post-command-hook #'lispm-ui-form--keep-point nil t))

;;;; Reactive state graph

(defun lispm-ui-form--normalize-value (key raw &optional fields)
  "Normalize RAW value of field KEY (trimming non-password strings).
FIELDS defaults to `lispm-ui-form--fields' in the current buffer."
  (if (stringp raw)
      (let* ((type (plist-get (cdr (assq key (or fields lispm-ui-form--fields))) :type))
             (text (if (eq type 'password) raw (string-trim raw))))
        (and (not (string-empty-p text)) text))
    raw))

(defun lispm-ui-form--init-signals ()
  "Initialize per-field source signals and the derived all-values signal."
  (setq lispm-ui-form--signals
        (mapcar (pcase-lambda (`(,key . ,val))
                  (cons key (lispm-ui-signal val)))
                lispm-ui-form--values))
  (let* ((fields lispm-ui-form--fields)
         (keys (mapcar #'car fields))
         (sigs (mapcar #'cdr lispm-ui-form--signals)))
    (setq lispm-ui-form--all-values-signal
          (lispm-ui-signal-derived
           sigs
           (lambda (&rest vals)
             (cl-mapcar (lambda (k v)
                          (cons k (lispm-ui-form--normalize-value k v fields)))
                        keys vals))))))

(defun lispm-ui-form--set-raw-value (key value)
  "Update field KEY's raw VALUE in both the alist and its reactive signal."
  (setf (alist-get key lispm-ui-form--values) value)
  (when-let* ((sig (alist-get key lispm-ui-form--signals)))
    (lispm-ui-signal-set sig value)))

;;;; Making a form

(cl-defun lispm-ui-form (title fields &key values submit cancel intro on-submit on-cancel
                                 instant modal mode setup)
  "Make a form titled TITLE of FIELDS in a buffer of its own; return the buffer.
FIELDS are as lispm-ui-form.el says.  VALUES, an alist (KEY . VALUE),
are the fields' values at first, before their :value.  SUBMIT and
CANCEL label its buttons, \"OK\" and \"Cancel\" by default; INTRO is
a text before its fields.  ON-SUBMIT is called with its values, once
valid, an alist (KEY . VALUE) of those of the fields shown: a text
trimmed, nil when empty.  ON-CANCEL is called when it is cancelled.
INSTANT non-nil makes a form whose changes apply at once, through its
fields' :on-change: it has no Cancel button, and a button closing it,
labelled SUBMIT, only when SUBMIT is given.  MODAL non-nil marks a
compact modal dialog.  MODE is the major mode of its buffer,
`lispm-ui-form-mode' or one derived from it; SETUP, if non-nil, is
called with no argument in its buffer before it is drawn."
  (let ((buffer (generate-new-buffer (format "*%s*" title))))
    (with-current-buffer buffer
      (funcall (or mode #'lispm-ui-form-mode))
      (unless (derived-mode-p 'lispm-ui-form-mode)
        (error "%s is no mode of forms" mode))
      (setq lispm-ui-form--title title
            lispm-ui-form--fields fields
            lispm-ui-form--props (list :submit submit :cancel cancel :intro intro
                                    :on-submit on-submit :on-cancel on-cancel
                                    :instant instant :modal modal)
            lispm-ui-form--values
            (mapcar (lambda (field)
                      (let ((key (car field)))
                        (cons key (if (assq key values)
                                      (alist-get key values)
                                    (plist-get (cdr field) :value)))))
                    fields))
      (lispm-ui-form--init-signals)
      (when setup
        (funcall setup))
      (lispm-ui-form--draw)
      (goto-char (point-min))
      (lispm-ui-form--first-field))
    buffer))

(defun lispm-ui-form--first-field ()
  "Move to the first field of the form, if it has one."
  (when-let* ((widget (seq-some (lambda (field) (alist-get (car field) lispm-ui-form--widgets))
                                lispm-ui-form--fields)))
    (goto-char (or (widget-field-start widget) (widget-get widget :from)))))

(defalias 'lispm-ui-form-first-field #'lispm-ui-form--first-field)

(defun lispm-ui-form-value (key)
  "The value of the field KEY of the form in this buffer, as it is now.
A text is trimmed, a password not; either is nil when empty."
  (let ((raw (if-let* ((sig (alist-get key lispm-ui-form--signals)))
                 (lispm-ui-signal-get sig)
               (alist-get key lispm-ui-form--values))))
    (lispm-ui-form--normalize-value key raw)))

(defun lispm-ui-form--type (key)
  "The type of the field KEY of the form in this buffer."
  (plist-get (cdr (assq key lispm-ui-form--fields)) :type))

(defun lispm-ui-form--shown-p (field)
  "Non-nil if FIELD of the form in this buffer is shown."
  (let ((when (plist-get (cdr field) :when)))
    (or (null when) (funcall when (lispm-ui-form--all-values)))))

(defun lispm-ui-form--all-values ()
  "The values of every field of the form in this buffer: an alist."
  (if (lispm-ui-signal-p lispm-ui-form--all-values-signal)
      (lispm-ui-signal-get lispm-ui-form--all-values-signal)
    (mapcar (lambda (field) (cons (car field) (lispm-ui-form-value (car field))))
            lispm-ui-form--fields)))

(defun lispm-ui-form-values ()
  "The values of the fields of the form in this buffer that are shown: an alist."
  (mapcar (lambda (field) (cons (car field) (lispm-ui-form-value (car field))))
          (seq-filter (lambda (field)
                        (and (lispm-ui-form--shown-p field)
                             (not (memq (plist-get (cdr field) :type) '(heading buttons rows status)))))
                      lispm-ui-form--fields)))

(defun lispm-ui-form-set (key value &rest more)
  "Make VALUE the value of the field KEY of the form in this buffer, and
so for each KEY and VALUE of MORE, and show them: a status said again
where it stands, the rest of the form as it is; any other field with the
form drawn again, once, at once, or, from a field's :on-change, once
that returns."
  (unless (derived-mode-p 'lispm-ui-form-mode)
    (user-error "%s" (lispm-ui-tr "Not a form")))
  (let ((keys nil))
    (lispm-ui-frp-batch
      (while key
        (lispm-ui-form--set-raw-value key value)
        (push key keys)
        (setq key (pop more)
              value (pop more))))
    (cond ((eq lispm-ui-form--changing (current-buffer))
           (if (and (not lispm-ui-form--stale)
                    (seq-every-p (lambda (k) (eq (lispm-ui-form--type k) 'status)) keys))
               (dolist (k (nreverse keys))
                 (cl-pushnew k lispm-ui-form--stale-status :test #'eq))
             (setq lispm-ui-form--stale t
                   lispm-ui-form--stale-status nil)))
          ((and (seq-every-p (lambda (k) (eq (lispm-ui-form--type k) 'status)) keys)
                (seq-every-p #'lispm-ui-form--draw-in-place (nreverse keys))))
          (t (lispm-ui-form--draw)))))

(defun lispm-ui-form--flush-deferred (redraw)
  "Apply any deferred redraw or in-place status update in the current buffer."
  (cond
   ((or redraw lispm-ui-form--stale)
    (lispm-ui-form--draw))
   (lispm-ui-form--stale-status
    (let ((keys (nreverse lispm-ui-form--stale-status)))
      (setq lispm-ui-form--stale-status nil)
      (unless (seq-every-p #'lispm-ui-form--draw-in-place keys)
        (lispm-ui-form--draw))))))

(defun lispm-ui-form--changed (key value &optional redraw)
  "Make VALUE the value of the field KEY, as it was changed, and call the
field's :on-change with it.  With REDRAW, draw the form again; else, if
:on-change changed what it shows, once the command changing it is done:
not while the text of a field is being changed."
  (let ((buffer (current-buffer)))
    (unwind-protect
        (lispm-ui-frp-batch
          (lispm-ui-form--set-raw-value key value)
          (when-let* ((on-change (plist-get (cdr (assq key lispm-ui-form--fields)) :on-change)))
            (let ((lispm-ui-form--changing buffer)
                  (lispm-ui-in-gui-action t))
              (funcall on-change value))))
      ;; Shown as it now is, whatever :on-change refused.
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (cond
           (redraw (lispm-ui-form--draw))
           ((or lispm-ui-form--stale lispm-ui-form--stale-status)
            (run-at-time 0 nil (lambda ()
                                 (when (buffer-live-p buffer)
                                   (with-current-buffer buffer
                                     (when (or lispm-ui-form--stale lispm-ui-form--stale-status)
                                       (lispm-ui-form--flush-deferred nil)))))))))))))

(defun lispm-ui-form--choices (field)
  "The choices of FIELD, (VALUE . LABEL) each."
  (let ((choices (plist-get (cdr field) :choices)))
    (if (functionp choices) (funcall choices) choices)))

;;;; Laying it out

(defun lispm-ui-form--measure (string)
  "Pixels STRING takes in this buffer, in its faces as they are remapped here."
  (let ((remapping face-remapping-alist))
    (with-temp-buffer
      (setq-local face-remapping-alist remapping)
      (insert string)
      (car (buffer-text-pixel-size nil nil t)))))

(defun lispm-ui-form--label-text (label)
  "LABEL as a field shows it, a colon after it."
  (propertize (lispm-ui-tr "%s:" label) 'face 'lispm-ui-form-label))

(defun lispm-ui-form--layout ()
  "Work out where the form in this buffer is drawn; see `lispm-ui-form--geometry'."
  (let* ((em (max 1 (lispm-ui-form--measure "n")))
         (labels (mapcar (lambda (field)
                           (let ((props (cdr field)))
                             (if (and (plist-get props :label)
                                      (not (eq (plist-get props :type) 'heading))
                                      (lispm-ui-form--shown-p field))
                                 (lispm-ui-form--measure (lispm-ui-form--label-text (plist-get props :label)))
                               0)))
                         lispm-ui-form--fields))
         (left (* 2 em))
         (label (+ left (* 2 em) (apply #'max 0 labels)))
         (control (+ label em)))
    (setq lispm-ui-form--geometry
          (list :em em :left left :start (+ left (* 2 em)) :label label :control control))
    (let ((right (lispm-ui-form--right)))
      (setq lispm-ui-form--geometry
            (append lispm-ui-form--geometry (list :right right :outer (+ right (* 2 em))))))))

(defun lispm-ui-form--right ()
  "Where the controls of the form in this buffer end, in pixels, its
geometry's :em and :control worked out: `lispm-ui-form-width' ems after
they start, and further to hold the rows of its lists, the texts of its
check boxes and its statuses, up to half as wide again, as a dialog is
as wide as its list."
  (let ((control (plist-get lispm-ui-form--geometry :control))
        (width (* (plist-get lispm-ui-form--geometry :em) lispm-ui-form-width)))
    (max (+ control width)
         (min (+ control (/ (* 3 width) 2)) (lispm-ui-form--content-right)))))

(defun lispm-ui-form--rows (field)
  "The rows of the rows FIELD of the form in this buffer, a plist each."
  (let ((rows (plist-get (cdr field) :rows)))
    (if (functionp rows) (funcall rows (lispm-ui-form--all-values)) rows)))

(defun lispm-ui-form--content-right ()
  "The right edge, in pixels, the rows of the form's lists, the texts of its
check boxes and its statuses shown need, each laid out in full; 0
without any."
  (let ((em (plist-get lispm-ui-form--geometry :em))
        (control (plist-get lispm-ui-form--geometry :control))
        (right 0))
    (dolist (field lispm-ui-form--fields)
      (when (lispm-ui-form--shown-p field)
        (pcase (plist-get (cdr field) :type)
          ('rows
           (let* ((rows (lispm-ui-form--rows field))
                  (columns (lispm-ui-form--columns rows (if (plist-get (cdr field) :label) :control :start))))
             (dolist (row rows)
               (let* ((texts (ensure-list (plist-get row :text)))
                      (buttons (plist-get row :buttons))
                      (end (if texts
                               (+ (nth (1- (length texts)) columns) (lispm-ui-form--measure (car (last texts))))
                             (or (car columns) 0))))
                 (setq right (max right (if buttons (+ end em (lispm-ui-form--buttons-width buttons)) end)))))))
          ('check
           ;; Its box, drawn or as text, and the text beside it.
           (when-let* ((text (plist-get (cdr field) :text)))
             (setq right (max right (+ control (lispm-ui-form--measure (concat "[X] " text)))))))
          ('status
           (dolist (line (split-string (or (alist-get (car field) lispm-ui-form--values) "") "\n"))
             (setq right (max right (+ control (lispm-ui-form--measure line)))))))))
    right))

(defun lispm-ui-form--at (key &optional less face)
  "A space reaching to where KEY of the form's geometry says, LESS pixels before.
KEY may be a number of pixels instead.  FACE, if non-nil, is the space's."
  (let ((x (- (if (numberp key) key (plist-get lispm-ui-form--geometry key)) (or less 0))))
    (propertize " " 'display `(space :align-to (,x)) 'face face)))

(defun lispm-ui-form--indent (key)
  "The prefix of a line's continuations reaching to where KEY of the form's
geometry says: a stretch, as Emacs aligns none in a string there."
  `(space :align-to (,(plist-get lispm-ui-form--geometry key))))

(defun lispm-ui-form--part (part &rest args)
  "PART of a group box, a string, as `lispm-ui-form-group-function' gives
it; or nil."
  (and lispm-ui-form-group-function (apply lispm-ui-form-group-function part args)))

(defun lispm-ui-form--edge (&optional thin)
  "A side of a text area's frame, one pixel wide; THIN for a thin line.
What is typed after it, into the area, takes none of its face."
  (propertize " " 'display '(space :width (1)) 'face (if thin '(lispm-ui-form-area-edge (:height 10))
                                                       'lispm-ui-form-area-edge)
              'rear-nonsticky t))

(defun lispm-ui-form--sides (group &optional thin)
  "The sides of GROUP's box for a line, (LEFT . RIGHT): strings; THIN for a
thin line.  Nil out of a group, or when its box has no sides."
  (when-let* ((group)
              (side (lispm-ui-form--part 'side)))
    (let ((side (if thin
                    (let ((side (copy-sequence side)))
                      (add-face-text-property 0 (length side) '(:height 10) t side)
                      side)
                  side))
          (face (and thin '(:height 10))))
      (cons (concat (lispm-ui-form--at :left nil face) side)
            (concat (lispm-ui-form--at (- (plist-get lispm-ui-form--geometry :outer)
                                       (lispm-ui-form--measure side))
                                    nil face)
                    side)))))

(defun lispm-ui-form--row (group insert &optional thin)
  "Insert a row of the form, what INSERT, a function, inserts, framed by the
sides of GROUP's box, if it is in one.  THIN makes it a line a pixel tall."
  (let ((sides (lispm-ui-form--sides group thin)))
    (when sides (insert (car sides)))
    (funcall insert)
    (when sides (insert (cdr sides)))
    (insert (if thin (propertize "\n" 'face '(:height 10)) "\n"))))

(defun lispm-ui-form--open-group (title)
  "Insert the top of a group box titled TITLE."
  (let ((width (- (plist-get lispm-ui-form--geometry :outer) (plist-get lispm-ui-form--geometry :left))))
    (insert "\n" (propertize (concat (lispm-ui-form--at :left)
                                     (or (lispm-ui-form--part 'top width title)
                                         (propertize title 'face 'lispm-ui-form-title)))
                             'lispm-ui-form-heading title)
            "\n")
    (lispm-ui-form--row t #'ignore)))

(defun lispm-ui-form--close-group ()
  "Insert the bottom of the group box being drawn."
  (lispm-ui-form--row t #'ignore)
  (when-let* ((bottom (lispm-ui-form--part 'bottom (- (plist-get lispm-ui-form--geometry :outer)
                                                    (plist-get lispm-ui-form--geometry :left)))))
    (insert (lispm-ui-form--at :left) bottom "\n")))

;;;; Drawing

(defun lispm-ui-form--glyph (kind state &optional label)
  "The picture of a control, see `lispm-ui-form-glyph-function'; or nil.
Where pictures cannot be shown, the control is drawn as text anyway."
  (when lispm-ui-form-glyph-function
    (funcall lispm-ui-form-glyph-function kind state label)))

(defun lispm-ui-form--button (label action &optional default)
  "Insert a push button LABEL calling ACTION; DEFAULT for the one C-c C-c presses."
  (widget-create 'push-button
                 :tag label
                 :tag-glyph (lispm-ui-form--glyph 'button (and default 'default) label)
                 :lispm-form-control (list 'button (and default 'default) label)
                 :notify (lambda (&rest _)
                           (let ((lispm-ui-in-gui-action t))
                             (funcall action)))))

(defun lispm-ui-form--button-width (label default)
  "Pixels the push button LABEL takes, DEFAULT if it is the default one."
  (let ((glyph (lispm-ui-form--glyph 'button (and default 'default) label)))
    (if (and glyph (display-graphic-p))
        (car (image-size glyph t))
      (lispm-ui-form--measure (concat "[" label "]")))))

(defun lispm-ui-form--key-at (position)
  "The key of the field whose widget is at POSITION, or nil."
  (when-let* ((widget (or (widget-field-at position) (widget-at position))))
    (or (widget-get widget :lispm-form-key)
        (when-let* ((parent (widget-get widget :parent)))
          (widget-get parent :lispm-form-key)))))

(defun lispm-ui-form--place ()
  "Where point is in the form in this buffer: (KEY . OFFSET), the key of
the field it is in and how far into it, or nil."
  (when-let* ((key (lispm-ui-form--key-at (point)))
              (widget (alist-get key lispm-ui-form--widgets)))
    (cons key (max 0 (- (point) (or (widget-field-start widget) (widget-get widget :from) (point)))))))

(defun lispm-ui-form--go-to (place)
  "Put point at PLACE, as `lispm-ui-form--place' said it was, in the field
as it is drawn now."
  (when-let* ((widget (and place (alist-get (car place) lispm-ui-form--widgets))))
    (goto-char (min (+ (or (widget-field-start widget) (widget-get widget :from)) (cdr place))
                    (or (widget-field-end widget) (widget-get widget :to))))
    ;; Its text field, if it is one, shows point again.
    (lispm-ui-form--keep-point)))

(defun lispm-ui-form--draw ()
  "Draw the form in this buffer, point staying in the field it was in."
  (let* ((place (lispm-ui-form--place))
         (inhibit-read-only t)
         (buffer-undo-list t)
         (group nil))
    ;; The fields go first: erasing them would change them otherwise.
    (setq widget-field-list nil
          widget-field-new nil
          widget-field-last nil
          lispm-ui-form--widgets nil
          lispm-ui-form--field-at-point nil
          lispm-ui-form--stale nil
          lispm-ui-form--stale-status nil)
    (pcase-dolist (`(,_ ,start ,end ,_) lispm-ui-form--places)
      (set-marker start nil)
      (set-marker end nil))
    (setq lispm-ui-form--places nil)
    (erase-buffer)
    (remove-overlays)
    (lispm-ui-form--layout)
    (when lispm-ui-form-show-header
      (insert (lispm-ui-form--at :left) (propertize lispm-ui-form--title 'face 'lispm-ui-form-title) "\n"
              (lispm-ui-form--at :left)
              (propertize (if (plist-get lispm-ui-form--props :instant)
                              (lispm-ui-tr "TAB goes to the next field · changes take effect at once · C-c C-c closes")
                            (lispm-ui-tr "TAB goes to the next field · C-c C-c %s · C-c C-k cancels"
                                         (lispm-ui-form--submit-label)))
                          'face 'lispm-ui-form-help
                          'wrap-prefix (lispm-ui-form--indent :left))
              "\n"))
    (when-let* ((intro (plist-get lispm-ui-form--props :intro)))
      (insert "\n" (propertize intro 'line-prefix (lispm-ui-form--indent :left)
                               'wrap-prefix (lispm-ui-form--indent :left))
              "\n"))
    (when (and (plist-get lispm-ui-form--props :modal)
               (not lispm-ui-form-show-header)
               (not (plist-get lispm-ui-form--props :intro)))
      (insert (propertize "\n" 'face '(:height 0.7))))
    (dolist (field lispm-ui-form--fields)
      (when (lispm-ui-form--shown-p field)
        (pcase (plist-get (cdr field) :type)
          ('heading
           (when group (lispm-ui-form--close-group))
           (lispm-ui-form--open-group (plist-get (cdr field) :label))
           (setq group t))
          ('status
           ;; Its place is kept, to say it again there: its start
           ;; stays before what is put at its end.
           (let ((start (point)))
             (lispm-ui-form--draw-field field group)
             (push (list (car field) (copy-marker start t) (point-marker) group)
                   lispm-ui-form--places)))
          (_ (lispm-ui-form--draw-field field group)))))
    (when group (lispm-ui-form--close-group))
    (insert "\n")
    (lispm-ui-form--buttons)
    (widget-setup)
    (lispm-ui-form--shape)
    (lispm-ui-form--go-to place)
    (run-hooks 'lispm-ui-form-drawn-hook)))

(defun lispm-ui-form--buttons ()
  "Insert the form's buttons, at the bottom right: Cancel, and the default
one; of a form applying its changes at once, the one closing it, if any."
  (let* ((instant (plist-get lispm-ui-form--props :instant))
         (cancel (or (plist-get lispm-ui-form--props :cancel) (lispm-ui-tr "Cancel")))
         (submit (if instant (plist-get lispm-ui-form--props :submit) (lispm-ui-form--submit-label)))
         (gap (plist-get lispm-ui-form--geometry :em))
         (edge (if (plist-get lispm-ui-form--props :modal) :right :outer)))
    (cond (instant
           (when submit
             (insert (lispm-ui-form--at edge (lispm-ui-form--button-width submit t)))
             (lispm-ui-form--button submit #'lispm-ui-form-submit t)
             (insert "\n")))
          (t
           (insert (lispm-ui-form--at edge (+ (lispm-ui-form--button-width cancel nil) gap
                                              (lispm-ui-form--button-width submit t))))
           (lispm-ui-form--button cancel #'lispm-ui-form-cancel)
           (insert (propertize " " 'display `(space :width (,gap))))
           (lispm-ui-form--button submit #'lispm-ui-form-submit t)
           (insert "\n")))))

(defun lispm-ui-form--submit-label ()
  "The label of the button submitting the form in this buffer."
  (or (plist-get lispm-ui-form--props :submit) (lispm-ui-tr "OK")))

(defun lispm-ui-form--remember (key widget)
  "Note WIDGET as the first of the field KEY's."
  (widget-put widget :lispm-form-key key)
  (widget-put widget :lispm-form-field t)
  (unless (assq key lispm-ui-form--widgets)
    (push (cons key widget) lispm-ui-form--widgets))
  widget)

(defun lispm-ui-form--label-inserter (label)
  "Return a once-only zero-argument thunk inserting LABEL flush-right."
  (let ((labelled nil))
    (lambda ()
      ;; The label, flush right in its column, on the field's first row.
      (unless labelled
        (setq labelled t)
        (when label
          (let ((text (lispm-ui-form--label-text label)))
            (insert (lispm-ui-form--at :label (lispm-ui-form--measure text)) text)))))))

(defun lispm-ui-form--draw-field (field group)
  "Insert FIELD of the form, its label and its help, in GROUP's box if any."
  (pcase-let* ((`(,key . ,props) field)
               (type (plist-get props :type))
               (label (plist-get props :label))
               (value (alist-get key lispm-ui-form--values))
               ;; Where its help starts, and a list without a label.
               (start (if (and (eq type 'rows) (not label)) :start :control))
               (label-fn (lispm-ui-form--label-inserter label))
               (row-fn (apply-partially #'lispm-ui-form--row group)))
    (cl-flet ((label () (funcall label-fn))
              (row (insert) (funcall row-fn insert)))
      (pcase type
        ((or 'text 'password)
         (lispm-ui-form--draw-text-field key type props value label-fn row-fn))
        ('text-area
         (lispm-ui-form--draw-text-area-field key props value group label-fn))
        ('check
         (row (lambda ()
                (label)
                (insert (lispm-ui-form--at :control))
                (lispm-ui-form--remember
                 key (widget-create 'toggle
                                    :format "%[%v%]"
                                    :on "[X]" :off "[ ]"
                                    :on-glyph (lispm-ui-form--glyph 'check 'on)
                                    :off-glyph (lispm-ui-form--glyph 'check 'off)
                                    :lispm-form-control 'check
                                    :value value
                                    :notify (lambda (widget &rest _)
                                              (lispm-ui-form--changed key (widget-value widget) t))))
                (insert " " (or (plist-get props :text) "")))))
        ('radio
         ;; One choice a row, as a dialog stacks them.
         (pcase-dolist (`(,choice . ,text) (lispm-ui-form--choices field))
           (row (lambda ()
                  (label)
                  (insert (lispm-ui-form--at :control))
                  (lispm-ui-form--remember
                   key (widget-create 'toggle
                                      :format "%[%v%]"
                                      :on "(*)" :off "( )"
                                      :on-glyph (lispm-ui-form--glyph 'radio 'on)
                                      :off-glyph (lispm-ui-form--glyph 'radio 'off)
                                      :lispm-form-control 'radio
                                      :value (equal value choice)
                                      :action (lambda (&rest _) (lispm-ui-form--changed key choice t))))
                  (insert " " text)))))
        ('choice
         (lispm-ui-form--draw-choice-field key field props label value label-fn row-fn))
        ('picture
         (lispm-ui-form--draw-picture-field key label value label-fn row-fn))
        ('buttons
         (row (lambda ()
                (label)
                (insert (lispm-ui-form--at :control))
                (lispm-ui-form--insert-buttons key (plist-get props :buttons)))))
        ('rows
         (lispm-ui-form--draw-rows-field key field props start label-fn row-fn))
        ('status
         ;; A line a row, the rows after the first under the first.
         (dolist (line (split-string (or value "") "\n"))
           (row (lambda ()
                  (label)
                  (insert (lispm-ui-form--at :control)
                          (propertize line 'wrap-prefix (lispm-ui-form--indent :control)))))))
        (_ (error "No type of field %S" type)))
      (when-let* ((help (let ((help (plist-get props :help)))
                          (if (functionp help) (funcall help (lispm-ui-form--all-values)) help))))
        ;; A line a row.
        (dolist (line (split-string help "\n"))
          (row (lambda ()
                 (insert (lispm-ui-form--at start) (lispm-ui-form--help line start)))))))))

(defun lispm-ui-form--help (help at)
  "HELP, a line under a field, as the form shows it from AT, a key of its
geometry: in the help's face, beneath those it has, as of an error."
  (let ((text (copy-sequence help)))
    (add-face-text-property 0 (length text) 'lispm-ui-form-help t text)
    (propertize text 'wrap-prefix (lispm-ui-form--indent at))))

(defun lispm-ui-form--insert-buttons (key buttons)
  "Insert BUTTONS, a list of (LABEL . FUNCTION), an em apart, as the field
KEY's."
  (let ((first t))
    (pcase-dolist (`(,text . ,action) buttons)
      (unless first
        (insert (propertize " " 'display `(space :width (,(plist-get lispm-ui-form--geometry :em))))))
      (setq first nil)
      (lispm-ui-form--remember key (lispm-ui-form--button text action)))))

(defun lispm-ui-form--buttons-width (buttons)
  "Pixels BUTTONS, a list of (LABEL . FUNCTION), take, an em apart."
  (let ((em (plist-get lispm-ui-form--geometry :em)))
    (max 0 (- (apply #'+ (mapcar (lambda (button) (+ em (lispm-ui-form--button-width (car button) nil)))
                                 buttons))
              em))))

(defun lispm-ui-form--columns (rows start)
  "The pixels the columns of ROWS, as a rows field has them, start at: the
first where START, a key of the form's geometry, says, each other two ems
after the widest text of the one before."
  (let* ((em (plist-get lispm-ui-form--geometry :em))
         (texts (mapcar (lambda (row) (ensure-list (plist-get row :text))) rows))
         (x (plist-get lispm-ui-form--geometry start))
         (columns nil))
    (dotimes (column (apply #'max 0 (mapcar #'length texts)))
      (push x columns)
      (setq x (+ x (* 2 em) (apply #'max 0 (mapcar (lambda (each) (lispm-ui-form--measure (or (nth column each) "")))
                                                   texts)))))
    (nreverse columns)))

(defun lispm-ui-form--draw-row (key row columns)
  "Insert ROW of the rows field KEY: its texts at COLUMNS, pixels, the last
one cut to the room its buttons leave, and its buttons at the right; a
row of buttons alone has them where its text would start."
  (let* ((buttons (plist-get row :buttons))
         (texts (ensure-list (plist-get row :text)))
         (width (lispm-ui-form--buttons-width buttons))
         (room (- (plist-get lispm-ui-form--geometry :right) width
                  (if buttons (plist-get lispm-ui-form--geometry :em) 0))))
    (cl-loop for (text . more) on texts
             for x in columns
             do (insert (lispm-ui-form--at x) (if more text (lispm-ui-form--fit text (- room x)))))
    (when buttons
      (insert (if texts (lispm-ui-form--at :right width) (lispm-ui-form--at (car columns))))
      (lispm-ui-form--insert-buttons key buttons))))

(defun lispm-ui-form-redraw ()
  "Draw the form in this buffer again, as it is, point staying where it was;
from a field's :on-change, once that returns."
  (when (derived-mode-p 'lispm-ui-form-mode)
    (if (eq lispm-ui-form--changing (current-buffer))
        (setq lispm-ui-form--stale t)
      (lispm-ui-form--draw))))

(defun lispm-ui-form--draw-in-place (key)
  "Draw the status field KEY again where it is drawn, the rest of the form
left as it is, what is typed in its fields too; return nil when it is
not shown, or when the form must widen to hold it.  Those who dress the
form are told, as when it is drawn."
  (pcase-let ((`(,start ,end ,group) (alist-get key lispm-ui-form--places)))
    ;; Not when the form is to widen to hold it: then it is drawn anew.
    (when (and start (eq (marker-buffer start) (current-buffer))
               (= (lispm-ui-form--right) (plist-get lispm-ui-form--geometry :right)))
      (let ((inhibit-read-only t)
            ;; Not a change of a field: Emacs's widgets would refuse it.
            (inhibit-modification-hooks t)
            (old (marker-position end)))
        (save-excursion
          ;; Drawn after itself, then itself taken away: a status after
          ;; it, starting at its end, stays after it.
          (goto-char old)
          (lispm-ui-form--draw-field (assq key lispm-ui-form--fields) group)
          (set-marker end (point))
          (delete-region start old)))
      (run-hooks 'lispm-ui-form-drawn-hook)
      t)))

(add-hook 'lispm-ui-form-mode-hook #'lispm-ui-form--faces)
(add-hook 'lispm-ui-form-drawn-hook #'lispm-ui-form-dress)
;; Late, after what shows LispM UI has set its faces' fonts in the new
;; palette: the forms are measured in them.
(add-hook 'lispm-ui-palette-changed-hook #'lispm-ui-form-redraw-all 90)
(add-hook 'lispm-ui-language-changed-hook #'lispm-ui-form-redraw-all 90)

;;;; Submitting and cancelling

(defun lispm-ui-form--problem (field)
  "What is wrong with the value of FIELD of the form in this buffer, or nil."
  (let ((value (lispm-ui-form-value (car field)))
        (props (cdr field)))
    (cond ((and (plist-get props :required) (null value))
           (lispm-ui-tr "%s: fill this in" (or (plist-get props :label) (car field))))
          ((when-let* ((validate (plist-get props :validate))
                       (problem (funcall validate value)))
             (format "%s: %s" (or (plist-get props :label) (car field)) problem))))))

(defun lispm-ui-form-submit ()
  "Submit the form in this buffer: once its values are valid, it is done."
  (interactive)
  (unless (derived-mode-p 'lispm-ui-form-mode)
    (user-error "%s" (lispm-ui-tr "Not a form")))
  (when-let* ((failure (seq-some (lambda (field)
                                   (when-let* ((msg (lispm-ui-form--problem field)))
                                     (cons field msg)))
                                 (seq-filter #'lispm-ui-form--shown-p lispm-ui-form--fields))))
    (when-let* ((widget (alist-get (caar failure) lispm-ui-form--widgets)))
      (goto-char (or (widget-field-start widget) (widget-get widget :from))))
    (user-error "%s" (cdr failure)))
  (let ((buffer (current-buffer))
        (lispm-ui-in-gui-action t))
    (when-let* ((on-submit (plist-get lispm-ui-form--props :on-submit)))
      (funcall on-submit (lispm-ui-form-values)))
    (lispm-ui-form--close buffer)))

(defun lispm-ui-form-cancel ()
  "Cancel the form in this buffer."
  (interactive)
  (let ((buffer (current-buffer))
        (lispm-ui-in-gui-action t))
    (when-let* ((on-cancel (plist-get lispm-ui-form--props :on-cancel)))
      (funcall on-cancel))
    (lispm-ui-form--close buffer)))

(defun lispm-ui-form--close (buffer)
  "Put the form in BUFFER away."
  (when (buffer-live-p buffer)
    (let ((window (get-buffer-window buffer t)))
      (cond
       ((not (window-live-p window))
        (kill-buffer buffer))
       ((window-dedicated-p window)
        (set-window-dedicated-p window nil)
        (kill-buffer buffer))
       (t
        (quit-window t window))))))

(provide 'lispm-ui-form)
;;; lispm-ui-form.el ends here
