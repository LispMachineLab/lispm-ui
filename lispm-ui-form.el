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
;; Fields are of a :type:
;;
;;   text       one line of text
;;   text-area  several lines (:lines, 6 by default)
;;   picture    an image file, shown, chosen with a button
;;   choice     one of :choices, a pop-up menu
;;   radio      one of :choices, a radio button each
;;   check      on or off, a check box
;;   buttons    no value: a row of push buttons, :buttons a list of
;;              (LABEL . FUNCTION), each FUNCTION called with no
;;              argument in the form's buffer when its button is pressed
;;   heading    no value: a heading that starts a group of fields
;;
;; and take :label, :help (a line under the field, or a function of the
;; values returning one or nil, asked each time the form is drawn),
;; :value (its value at first), :required (empty is refused), :validate
;; (a function of the value returning nil, or what is wrong), :when (a
;; function of the values: the field is shown only while it returns
;; non-nil) and :width (of a text field, in columns).  :choices is a
;; list of (VALUE . LABEL), or a function returning one.  A form whose
;; help says how something outside it stands, as whether a server
;; answers, is drawn again with `lispm-ui-form-redraw' when that changes.
;;
;; `lispm-ui-form' draws it in a buffer of its own with Emacs's widgets
;; (wid-edit.el), and returns the buffer: TAB and S-TAB go from field to
;; field, Return to the next field (a new line in a text area), C-c C-c
;; submits it, C-c C-k cancels it, the mouse presses its buttons.  Once
;; its values are valid, :on-submit is called with them, an alist, and
;; the form goes, unless it signals an error, which is shown.
;;
;; It is laid out as a dialog: the labels in a column, flush right, each
;; control beside its label and the help under it, the fields as wide
;; as `lispm-ui-form-width' says, a text area a panel as wide, a heading's
;; fields framed in a group box, and the buttons at the bottom right.
;; Positions are pixels, measured in the buffer's own faces, so that
;; whatever remaps them, as to a proportional face, gets them right.
;;
;; A form knows nothing of where it is shown.  Whatever shows it may
;; dress it, as a desktop does in its look:
;; `lispm-ui-form-glyph-function' gives the pictures of its buttons,
;; check boxes, radio buttons, pop-up menus and pictures,
;; `lispm-ui-form-group-function' the edges of its group boxes,
;; `lispm-ui-form-choose-function' shows a pop-up menu's choices,
;; `lispm-ui-form-choose-file-function' the files to choose a picture
;; from, its faces may be remapped (`lispm-ui-form-mode-hook' runs
;; before it is drawn) and `lispm-ui-form-show-header' set to nil where
;; its window is titled with it, and `lispm-ui-form-drawn-hook' runs once
;; it is drawn, as `lispm-ui-form-redraw' draws it again, when the look
;; changes, say.  What it shows is read with `lispm-ui-form-title',
;; `lispm-ui-form-geometry' and `lispm-ui-form-widgets'.
;; Each control's widget says what it is, as the glyph function was
;; asked, in its :lispm-form-control: `check' or `radio' (its state is
;; its value), or (KIND STATE LABEL) for a button or a pop-up menu, so
;; that it can be drawn otherwise, as focused.  Without them, a form is
;; drawn as text, which a text terminal shows too.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'image)
(require 'wid-edit)
(require 'lispm-ui-i18n)

;; Only in an Emacs that draws images.
(declare-function image-size "image.c" (spec &optional pixels frame))

(defconst lispm-ui-form-types '(text text-area picture choice radio check buttons heading)
  "The types a form's fields may be, see `lispm-ui-form'.
A program written for a later LispM UI may ask whether one it uses is here.")

(defvar lispm-ui-form-glyph-function nil
  "Function giving the picture of a control of a form, or nil for text.
It is called with KIND, STATE and LABEL:
  KIND `check' or `radio', STATE `on' or `off', LABEL nil;
  KIND `button', STATE nil, or `default' for the button C-c C-c
       presses, LABEL its label;
  KIND `popup', a pop-up menu, STATE nil, LABEL what it shows;
  KIND `picture', STATE nil, LABEL the picture's file, or nil for none.
It returns an image, or nil for the control drawn as text.")

(defvar lispm-ui-form-group-function nil
  "Function giving the edges of a form's group boxes, or nil for none.
A group is the fields after a heading, up to the next.  It is called
with the PART wanted:
  `top', with WIDTH, in pixels, and TITLE: the top edge, titled;
  `bottom', with WIDTH: the bottom edge;
  `side': a side, as tall as the line it is on.
It returns a string to insert, as one displaying an image, or nil: then
the heading is its title in bold, and the group has no frame.")

(defvar lispm-ui-form-choose-function #'lispm-ui-form-choose
  "Function showing the choices of a pop-up menu of a form; it returns one.
It is called with the field's LABEL, its CHOICES, a list of (VALUE .
TEXT), the VALUE chosen now, and the EVENT that pressed the menu, if
any, with point on the menu.  It returns (VALUE), the choice made, or
nil when none was.")

(defvar lispm-ui-form-choose-file-function nil
  "Function showing files to choose one of for a form's field, or nil.
It is called with the field's TYPE, `picture', its LABEL, the FILE
chosen now, or nil, and PREDICATE, a function of a file's name saying
whether it may be chosen, as whether it is a picture.  It returns the
file chosen, or nil when none was.  Without one, the file is read in
the minibuffer.")

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

(defface lispm-ui-form-area '((t :inherit widget-field :box nil :extend nil))
  "Face of a text area of a form, a panel of several lines."
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

(defvar-local lispm-ui-form--props nil
  "The properties the form in this buffer was made with.")

(defvar-local lispm-ui-form--widgets nil
  "Alist (KEY . WIDGET): the first widget of each field shown.")

(defvar-local lispm-ui-form--geometry nil
  "Where the form in this buffer is drawn: a plist of pixels.
:em, the width of the letter n; :left and :outer, the left and right
edges of its group boxes; :label, where its labels end; :control and
:right, where its controls start and end.")

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

(defvar-keymap lispm-ui-form-mode-map
  :doc "Keymap of `lispm-ui-form-mode'."
  :parent widget-keymap
  "C-c C-c" #'lispm-ui-form-submit
  "C-c C-k" #'lispm-ui-form-cancel)

(define-derived-mode lispm-ui-form-mode nil "Form"
  "A form: fields to fill in, written as data (see `lispm-ui-form').
TAB and S-TAB go from field to field, \\[lispm-ui-form-submit] submits it,
\\[lispm-ui-form-cancel] cancels it.

\\{lispm-ui-form-mode-map}"
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  ;; A new line of a text area starts where its panel does, not indented
  ;; under the label beside its first.
  (electric-indent-local-mode -1)
  (add-hook 'post-command-hook #'lispm-ui-form--keep-point nil t))

;;;; Making a form

(cl-defun lispm-ui-form (title fields &key values submit cancel intro on-submit on-cancel)
  "Make a form titled TITLE of FIELDS in a buffer of its own; return the buffer.
FIELDS are as lispm-ui-form.el says.  VALUES, an alist (KEY . VALUE),
are the fields' values at first, before their :value.  SUBMIT and
CANCEL label its buttons, \"OK\" and \"Cancel\" by default; INTRO is
a text before its fields.  ON-SUBMIT is called with its values, once
valid, an alist (KEY . VALUE) of those of the fields shown: a text
trimmed, nil when empty.  ON-CANCEL is called when it is cancelled."
  (let ((buffer (generate-new-buffer (format "*%s*" title))))
    (with-current-buffer buffer
      (lispm-ui-form-mode)
      (setq lispm-ui-form--title title
            lispm-ui-form--fields fields
            lispm-ui-form--props (list :submit submit :cancel cancel :intro intro
                                    :on-submit on-submit :on-cancel on-cancel)
            lispm-ui-form--values
            (mapcar (lambda (field)
                      (let ((key (car field)))
                        (cons key (if (assq key values)
                                      (alist-get key values)
                                    (plist-get (cdr field) :value)))))
                    fields))
      (lispm-ui-form--draw)
      (goto-char (point-min))
      (lispm-ui-form--first-field))
    buffer))

(defun lispm-ui-form--first-field ()
  "Move to the first field of the form, if it has one."
  (when-let* ((widget (seq-some (lambda (field) (alist-get (car field) lispm-ui-form--widgets))
                                lispm-ui-form--fields)))
    (goto-char (or (widget-field-start widget) (widget-get widget :from)))))

(defun lispm-ui-form-value (key)
  "The value of the field KEY of the form in this buffer, as it is now."
  (let ((value (alist-get key lispm-ui-form--values)))
    (if (stringp value)
        (let ((text (string-trim value)))
          (and (not (string-empty-p text)) text))
      value)))

(defun lispm-ui-form--shown-p (field)
  "Non-nil if FIELD of the form in this buffer is shown."
  (let ((when (plist-get (cdr field) :when)))
    (or (null when) (funcall when (lispm-ui-form--all-values)))))

(defun lispm-ui-form--all-values ()
  "The values of every field of the form in this buffer: an alist."
  (mapcar (lambda (field) (cons (car field) (lispm-ui-form-value (car field))))
          lispm-ui-form--fields))

(defun lispm-ui-form-values ()
  "The values of the fields of the form in this buffer that are shown: an alist."
  (mapcar (lambda (field) (cons (car field) (lispm-ui-form-value (car field))))
          (seq-filter (lambda (field)
                        (and (lispm-ui-form--shown-p field)
                             (not (memq (plist-get (cdr field) :type) '(heading buttons)))))
                      lispm-ui-form--fields)))

(defun lispm-ui-form--set (key value &optional redraw)
  "Make VALUE the value of the field KEY; with REDRAW, draw the form again."
  (setf (alist-get key lispm-ui-form--values) value)
  (when redraw
    (lispm-ui-form--draw)))

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
         (control (+ label em))
         (right (+ control (* em lispm-ui-form-width))))
    (setq lispm-ui-form--geometry
          (list :em em :left left :label label :control control :right right
                :outer (+ right (* 2 em))))))

(defun lispm-ui-form--at (key &optional less face)
  "A space reaching to where KEY of the form's geometry says, LESS pixels before.
KEY may be a number of pixels instead.  FACE, if non-nil, is the space's."
  (let ((x (- (if (numberp key) key (plist-get lispm-ui-form--geometry key)) (or less 0))))
    (propertize " " 'display `(space :align-to (,x)) 'face face)))

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
                 :notify (lambda (&rest _) (funcall action))))

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

(defun lispm-ui-form--draw ()
  "Draw the form in this buffer, point staying in the field it was in."
  (let* ((key (lispm-ui-form--key-at (point)))
         (offset (when-let* ((widget (and key (alist-get key lispm-ui-form--widgets))))
                   (max 0 (- (point) (or (widget-field-start widget) (widget-get widget :from) (point))))))
         (inhibit-read-only t)
         (group nil))
    ;; The fields go first: erasing them would change them otherwise.
    (setq widget-field-list nil
          widget-field-new nil
          widget-field-last nil
          lispm-ui-form--widgets nil)
    (erase-buffer)
    (remove-overlays)
    (lispm-ui-form--layout)
    (when lispm-ui-form-show-header
      (insert (lispm-ui-form--at :left) (propertize lispm-ui-form--title 'face 'lispm-ui-form-title) "\n"
              (lispm-ui-form--at :left)
              (propertize (lispm-ui-tr "TAB goes to the next field · C-c C-c %s · C-c C-k cancels"
                                    (lispm-ui-form--submit-label))
                          'face 'lispm-ui-form-help)
              "\n"))
    (when-let* ((intro (plist-get lispm-ui-form--props :intro)))
      (insert "\n" (propertize intro 'line-prefix (lispm-ui-form--at :left) 'wrap-prefix (lispm-ui-form--at :left))
              "\n"))
    (dolist (field lispm-ui-form--fields)
      (when (lispm-ui-form--shown-p field)
        (if (eq (plist-get (cdr field) :type) 'heading)
            (progn (when group (lispm-ui-form--close-group))
                   (lispm-ui-form--open-group (plist-get (cdr field) :label))
                   (setq group t))
          (lispm-ui-form--draw-field field group))))
    (when group (lispm-ui-form--close-group))
    (insert "\n")
    (lispm-ui-form--buttons)
    (widget-setup)
    (lispm-ui-form--shape)
    (when-let* ((widget (and key (alist-get key lispm-ui-form--widgets))))
      (goto-char (min (+ (or (widget-field-start widget) (widget-get widget :from)) (or offset 0))
                      (or (widget-field-end widget) (widget-get widget :to)))))
    (run-hooks 'lispm-ui-form-drawn-hook)))

(defun lispm-ui-form--buttons ()
  "Insert the form's buttons, at the bottom right: Cancel, and the default one."
  (let* ((cancel (or (plist-get lispm-ui-form--props :cancel) (lispm-ui-tr "Cancel")))
         (submit (lispm-ui-form--submit-label))
         (gap (plist-get lispm-ui-form--geometry :em)))
    (insert (lispm-ui-form--at :outer (+ (lispm-ui-form--button-width cancel nil) gap
                                      (lispm-ui-form--button-width submit t))))
    (lispm-ui-form--button cancel #'lispm-ui-form-cancel)
    (insert (propertize " " 'display `(space :width (,gap))))
    (lispm-ui-form--button submit #'lispm-ui-form-submit t)
    (insert "\n")))

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

(defun lispm-ui-form--draw-field (field group)
  "Insert FIELD of the form, its label and its help, in GROUP's box if any."
  (pcase-let* ((`(,key . ,props) field)
               (type (plist-get props :type))
               (label (plist-get props :label))
               (value (alist-get key lispm-ui-form--values))
               (labelled nil))
    (cl-flet ((label ()
                ;; The label, flush right in its column, on the field's first row.
                (unless labelled
                  (setq labelled t)
                  (when label
                    (let ((text (lispm-ui-form--label-text label)))
                      (insert (lispm-ui-form--at :label (lispm-ui-form--measure text)) text)))))
              (row (insert) (lispm-ui-form--row group insert)))
      (pcase type
        ('text
         (row (lambda ()
                (label)
                (insert (lispm-ui-form--at :control))
                (lispm-ui-form--remember
                 key (widget-create 'editable-field
                                    ;; Padded far, its padding drawn as one
                                    ;; blank to the fields' right edge: see
                                    ;; `lispm-ui-form--shape-field'.
                                    :size 200
                                    :format "%v"
                                    :value (or value "")
                                    :lispm-form-width (plist-get props :width)
                                    :action (lambda (&rest _) (widget-forward 1))
                                    :notify (lambda (widget &rest _)
                                              (lispm-ui-form--set key (widget-value widget))
                                              (lispm-ui-form--shape-field widget)))))))
        ('text-area
         (let* ((lines (or (plist-get props :lines) 6))
                (shown (string-trim-right (or value ""))))
           ;; A panel framed a pixel wide: a thin line above, one below,
           ;; and a side about each of its lines.
           (lispm-ui-form--row group (lambda () (lispm-ui-form--area-rule)) t)
           (let ((sides (lispm-ui-form--sides group)))
             (when sides (insert (car sides)))
             (label)
             (insert (lispm-ui-form--at :control 1) (lispm-ui-form--edge))
             (lispm-ui-form--remember
              key (widget-create 'text
                                 :format "%v"
                                 :value-face 'lispm-ui-form-area
                                 ;; Its lines but the last end in a newline;
                                 ;; Emacs's widgets end that one, as they end
                                 ;; a text field of no fixed size.
                                 :value (concat shown (make-string (max 0 (- lines 1 (cl-count ?\n shown))) ?\n))
                                 :lispm-form-group group
                                 :notify (lambda (widget &rest _)
                                           (lispm-ui-form--set key (widget-value widget))
                                           (lispm-ui-form--shape-area widget)))))
           (lispm-ui-form--row group (lambda () (lispm-ui-form--area-rule)) t)))
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
                                              (lispm-ui-form--set key (widget-value widget) t))))
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
                                      :action (lambda (&rest _) (lispm-ui-form--set key choice t))))
                  (insert " " text)))))
        ('choice
         (let* ((choices (lispm-ui-form--choices field))
                (shown (or (cdr (assoc value choices)) (lispm-ui-tr "Choose…"))))
           (row (lambda ()
                  (label)
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
                                                  (lispm-ui-form--set key (car choice) t)))))))))
        ('picture
         (row (lambda ()
                (label)
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
                                             (lispm-ui-form--set key file t)))))
                (when value
                  (insert " ")
                  (lispm-ui-form--button (lispm-ui-tr "Remove") (lambda () (lispm-ui-form--set key nil t)))))))
        ('buttons
         (row (lambda ()
                (label)
                (insert (lispm-ui-form--at :control))
                (let ((first t))
                  (pcase-dolist (`(,text . ,action) (plist-get props :buttons))
                    (unless first
                      (insert (propertize " " 'display `(space :width (,(plist-get lispm-ui-form--geometry :em))))))
                    (setq first nil)
                    (lispm-ui-form--remember key (lispm-ui-form--button text action)))))))
        (_ (error "No type of field %S" type)))
      (when-let* ((help (let ((help (plist-get props :help)))
                          (if (functionp help) (funcall help (lispm-ui-form--all-values)) help))))
        (row (lambda ()
               (insert (lispm-ui-form--at :control)
                       (propertize help 'face 'lispm-ui-form-help
                                   'wrap-prefix (lispm-ui-form--at :control)))))))))

(defun lispm-ui-form--area-rule ()
  "Insert the thin line above or below a text area: its frame's top or bottom."
  (insert (lispm-ui-form--at :control 1 '(:height 10))
          (propertize " " 'display `(space :width (,(+ 2 (- (plist-get lispm-ui-form--geometry :right)
                                                           (plist-get lispm-ui-form--geometry :control))))
                                           :height (1))
                      'face '(lispm-ui-form-area-edge (:height 10)))))

(defun lispm-ui-form--shape ()
  "Shape the fields of the form in this buffer: each text field as wide as
the form's fields, each text area a panel as wide."
  (dolist (field widget-field-list)
    (pcase (widget-type field)
      ('editable-field (lispm-ui-form--shape-field field))
      ('text (lispm-ui-form--shape-area field)))))

(defun lispm-ui-form--shape-field (widget)
  "Make the text field WIDGET as wide as the form's fields, whatever its text.
Its padding, the blanks Emacs's widgets end a field of a fixed size
with, is drawn as one blank reaching to the right edge, the rest of it
hidden, so that the field is one run of its face, framed once."
  (let* ((start (widget-field-start widget))
         (end (widget-field-end widget))
         (text-end (widget-field-text-end widget))
         ;; Blanks just typed are shown, though they end the text, until
         ;; point leaves them: they are as Emacs's widgets pad a field.
         (shown (if (and text-end end (< text-end (point) end)) (point) text-end))
         (right (if-let* ((width (widget-get widget :lispm-form-width)))
                    (+ (plist-get lispm-ui-form--geometry :control)
                       (* width (plist-get lispm-ui-form--geometry :em)))
                  (plist-get lispm-ui-form--geometry :right))))
    (when (and start end text-end)
      (with-silent-modifications
        (remove-list-of-text-properties start end '(display invisible))
        (when (< shown end)
          ;; The last blank reaches the edge, the others before it are
          ;; hidden: the frame closes only where the field's face ends
          ;; right after what is shown.  Invisible, not displayed as "":
          ;; after each command Emacs takes point out of text displayed
          ;; as nothing, even from its start, to the field's end, and
          ;; what is typed next goes there, after the blanks.
          (put-text-property shown (1- end) 'invisible t)
          (put-text-property (1- end) end 'display `(space :align-to (,right))))))))

(defun lispm-ui-form--keep-point ()
  "Keep point out of the hidden padding of the text field it is in: at the
end of what the field shows, where a dialog's field keeps it.  Run after
each command in a form's buffer."
  (when-let* ((field (widget-field-at (point)))
              ((eq (widget-type field) 'editable-field))
              (start (widget-field-start field))
              (end (widget-field-end field))
              (hidden (text-property-any start end 'invisible t))
              ((< hidden (point))))
    (goto-char hidden)))

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
             ;; Its last newline ends it, in an overlay of its own; an
             ;; Emacs before 31 leaves that newline after the field, in
             ;; none, and the panel gives it one.
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
        ;; Emacs's widgets put a blank after the newline that ends a field,
        ;; for a frame drawn about the field's text: this one has none, and
        ;; the blank would make the line after it as tall as any.
        (when (overlayp last)
          (overlay-put last 'after-string nil))
        (save-excursion
          (goto-char start)
          (while (search-forward "\n" end t)
            ;; Over the newline: the panel runs on to the right edge before
            ;; it, and the newline itself is not the panel's.
            (let ((pad (make-overlay (1- (point)) (point))))
              ;; The cursor at the end of a line stays by its text, not
              ;; after the panel's edge.
              (overlay-put pad 'before-string
                           (concat (propertize (lispm-ui-form--at :right nil 'lispm-ui-form-area) 'cursor t)
                                   (lispm-ui-form--edge)
                                   (or (cdr sides) "")))
              (overlay-put pad 'face 'default)
              (overlay-put pad 'priority 10)
              (push pad pads))))
        (widget-put widget :lispm-form-pads pads)))))

(defun lispm-ui-form-choose (label choices _value event)
  "Show CHOICES, a list of (VALUE . TEXT), as LABEL's; return (VALUE) chosen.
A menu for EVENT, if it is the mouse's, else Emacs's own way for the
keyboard (see `widget-choose')."
  (widget-choose label
                 (mapcar (lambda (choice) (cons (cdr choice) (list (car choice)))) choices)
                 event))

(defun lispm-ui-form-redraw ()
  "Draw the form in this buffer again, as it is, point staying where it was."
  (when (derived-mode-p 'lispm-ui-form-mode)
    (lispm-ui-form--draw)))

(defun lispm-ui-form--picture-p (file)
  "Non-nil if FILE is a picture: one Emacs shows, or named as pictures are.
An Emacs without a display shows none, but keeps them all the same."
  (or (image-supported-file-p file)
      (member (downcase (or (file-name-extension file) "")) '("png" "jpg" "jpeg" "gif" "svg" "webp"))))

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
  (when-let* ((field (seq-find #'lispm-ui-form--problem
                               (seq-filter #'lispm-ui-form--shown-p lispm-ui-form--fields))))
    (when-let* ((widget (alist-get (car field) lispm-ui-form--widgets)))
      (goto-char (or (widget-field-start widget) (widget-get widget :from))))
    (user-error "%s" (lispm-ui-form--problem field)))
  (let ((buffer (current-buffer)))
    (when-let* ((on-submit (plist-get lispm-ui-form--props :on-submit)))
      (funcall on-submit (lispm-ui-form-values)))
    (lispm-ui-form--close buffer)))

(defun lispm-ui-form-cancel ()
  "Cancel the form in this buffer."
  (interactive)
  (let ((buffer (current-buffer)))
    (when-let* ((on-cancel (plist-get lispm-ui-form--props :on-cancel)))
      (funcall on-cancel))
    (lispm-ui-form--close buffer)))

(defun lispm-ui-form--close (buffer)
  "Put the form in BUFFER away."
  (when (buffer-live-p buffer)
    (if-let* ((window (get-buffer-window buffer)))
        (quit-window t window)
      (kill-buffer buffer))))

(provide 'lispm-ui-form)
;;; lispm-ui-form.el ends here
