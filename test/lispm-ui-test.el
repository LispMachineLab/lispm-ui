;;; lispm-ui-test.el --- Tests of LispM UI  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
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

;; The tests of every part of LispM UI, run with `make test'; `make i18n'
;; runs `lispm-ui-test-report-catalogs'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'wid-edit)
(require 'lispm-ui)

(defconst lispm-ui-test--directory
  (file-name-directory (directory-file-name (file-name-directory (or load-file-name buffer-file-name))))
  "The directory of LispM UI's sources.")

(defun lispm-ui-test--wait (predicate &optional seconds)
  "Run the event loop until PREDICATE holds or SECONDS (default 5) pass."
  (let ((deadline (+ (float-time) (or seconds 5))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (funcall predicate)))

;;;; Standing alone

(defun lispm-ui-test--source-files ()
  "Return LispM UI's Lisp files."
  (directory-files lispm-ui-test--directory t "\\`lispm-.*\\.el\\'"))

(ert-deftest lispm-ui-test-knows-no-desktop ()
  "LispM UI knows of no desktop: no file of it but its tests names the
desktop's code or its look, as the App Store's tests check of every
package; a desktop dresses LispM UI, never the reverse."
  (dolist (file (directory-files-recursively lispm-ui-test--directory "" nil
                                             (lambda (dir)
                                               (not (member (file-name-nondirectory dir) '(".git" "test"))))))
    (unless (member (file-name-nondirectory file) '("LICENSE"))
      (with-temp-buffer
        (insert-file-contents file)
        (let ((case-fold-search t))
          (should-not (and (re-search-forward (concat "lispm-" "desktop\\|" "plat" "inum") nil t)
                           (format "%s:%d" (file-relative-name file lispm-ui-test--directory)
                                   (line-number-at-pos)))))))))

(ert-deftest lispm-ui-test-requires-only-itself ()
  "LispM UI requires no other LispM package: each part requires Emacs's own
libraries and parts of LispM UI, and the main file every part."
  (dolist (file (lispm-ui-test--source-files))
    (with-temp-buffer
      (insert-file-contents file)
      (emacs-lisp-mode)
      (while (re-search-forward "(require '\\([^ )]+\\)" nil t)
        (let ((feature (match-string 1)))
          (unless (nth 8 (syntax-ppss))
            (should-not (and (string-prefix-p "lispm-" feature)
                             (not (string-prefix-p "lispm-ui" feature))
                             (format "%s requires %s" (file-name-nondirectory file) feature))))))))
  (let ((main (with-temp-buffer
                (insert-file-contents (expand-file-name "lispm-ui.el" lispm-ui-test--directory))
                (buffer-string))))
    (dolist (file (lispm-ui-test--source-files))
      (let ((feature (file-name-base file)))
        ;; lispm-form.el is required only by what asks for its names.
        (unless (member feature '("lispm-ui" "lispm-form"))
          (should (string-search (format "(require '%s)" feature) main)))))))

;;;; lispm-ui-base

(ert-deftest lispm-ui-test-base ()
  "A batch Emacs is no text terminal, and lengths stay pixels there; a
text terminal counts a cell for eight pixels."
  (should (file-directory-p (expand-file-name "i18n" lispm-ui-directory)))
  (should-not (lispm-ui-tty-p))
  (should (= (lispm-ui-px 12) 12))
  (cl-letf (((symbol-function 'lispm-ui-tty-p) (lambda (&rest _) t)))
    (should (= (lispm-ui-px 12) 2))
    (should (= (lispm-ui-px 3) 0))))

;;;; lispm-ui-modal

(ert-deftest lispm-ui-test-modal-key-roles ()
  "Every role of a key in a modal panel has a key the HHKB types: C-i for
Tab, C-m for Return, C-[ and C-g for Escape, DEL for Back, and C-f, C-b,
C-n and C-p for the arrows, which stay as well."
  (should (eq (lispm-ui-modal-role ?\C-i) 'next))
  (should (eq (lispm-ui-modal-role 'backtab) 'previous))
  (should (eq (lispm-ui-modal-role ?\C-m) 'confirm))
  (should (eq (lispm-ui-modal-role ?\C-\[) 'cancel))
  (should (eq (lispm-ui-modal-role ?\C-g) 'cancel))
  (should (eq (lispm-ui-modal-role ?\M-.) 'cancel))
  (should (eq (lispm-ui-modal-role ?\d) 'back))
  (should (eq (lispm-ui-modal-role ?\s) 'press))
  (should (equal (mapcar #'lispm-ui-modal-step '(?\C-f ?\C-n tab right ?\C-b ?\C-p backtab up ?a))
                 '(1 1 1 1 -1 -1 -1 -1 nil)))
  (should-not (lispm-ui-modal-role ?a)))

(ert-deftest lispm-ui-test-modal-cycle ()
  "A focus ring wraps around; from no control it goes to the first or the
last, and it passes over the controls it is told to."
  (should (= (lispm-ui-modal-cycle 2 1 3) 0))
  (should (= (lispm-ui-modal-cycle 0 -1 3) 2))
  (should (= (lispm-ui-modal-cycle nil 1 3) 0))
  (should (= (lispm-ui-modal-cycle nil -1 3) 2))
  (should (= (lispm-ui-modal-cycle 1 4 6) 5))
  (should (= (lispm-ui-modal-cycle 0 1 4 (lambda (index) (/= index 1))) 2))
  (should-not (lispm-ui-modal-cycle 0 1 4 #'ignore))
  (should-not (lispm-ui-modal-cycle nil 1 0)))

(ert-deftest lispm-ui-test-modal-hit ()
  "A click is on a control when it lies in its rectangle, the left and top
edges in, the right and bottom out."
  (let ((layout '((a 10 10 20 10) (b 30 10 20 10) (c 0 0 0 0))))
    (should (eq (lispm-ui-modal-hit layout 10 10) 'a))
    (should (eq (lispm-ui-modal-hit layout 29 19) 'a))
    (should (eq (lispm-ui-modal-hit layout 30 10) 'b))
    (should-not (lispm-ui-modal-hit layout 50 10))
    (should-not (lispm-ui-modal-hit layout 20 20))
    (should-not (lispm-ui-modal-hit layout 0 0))
    (should-not (lispm-ui-modal-inside-p nil 0 0))))

(ert-deftest lispm-ui-test-modal-flash ()
  "A control chosen blinks for its kind's time: a button eight ticks, a
menu item three, drawn back as it was when asked."
  (let ((log nil))
    (cl-letf (((symbol-function 'sit-for) (lambda (seconds) (push (list 'wait seconds) log)))
              ((symbol-function 'redisplay) #'ignore))
      (lispm-ui-modal-flash 'button (lambda () (push 'pressed log)))
      (should (equal (reverse log) '(pressed (wait 0.133))))
      (setq log nil)
      (lispm-ui-modal-flash 'menu-item (lambda () (push 'off log)) (lambda () (push 'on log)))
      (should (equal (reverse log) '(off (wait 0.05) on))))))

(ert-deftest lispm-ui-test-modal-hold ()
  "A control held with the mouse is drawn pushed in only while the mouse
is over it, and chosen only when the button rises over it; Escape gives
up."
  (let ((draws nil))
    (cl-flet ((hold (events)
                (setq draws nil)
                (let ((unread-command-events events))
                  (lispm-ui-modal-hold (lambda (event) (eq (cadr event) 'on))
                                       (lambda (in) (push in draws))))))
      (should (hold '((mouse-movement off) (mouse-movement on) (mouse-1 on))))
      (should (equal (reverse draws) '(t nil t nil)))
      (should-not (hold '((mouse-movement off) (mouse-1 off))))
      (should (equal (reverse draws) '(t nil)))
      (should-not (hold '(escape)))
      (should (equal (reverse draws) '(t nil))))))

(ert-deftest lispm-ui-test-modal-moved-p ()
  "A move of the mouse by more than `double-click-fuzz' is a drag; less is
a hand's jitter in a click.  In a text terminal's cells, of which Emacs
reads the fuzz as eighths, a cell is a drag."
  (let ((double-click-fuzz 3))
    (should-not (lispm-ui-modal-moved-p '(3 . -3)))
    (should (lispm-ui-modal-moved-p '(4 . 0)))
    (should (lispm-ui-modal-moved-p '(0 . -4)))
    (should-not (lispm-ui-modal-moved-p nil))
    (should (lispm-ui-modal-moved-p '(1 . 0) t))
    (should (lispm-ui-modal-moved-p '(0 . -1) t))
    (should-not (lispm-ui-modal-moved-p '(0 . 0) t))
    (should-not (lispm-ui-modal-moved-p nil t)))
  (let ((double-click-fuzz 16))
    (should-not (lispm-ui-modal-moved-p '(2 . 0) t))
    (should (lispm-ui-modal-moved-p '(3 . 0) t))))

;;;; lispm-ui-form

(defun lispm-ui-test--form-field (key)
  "Move to the start of the field KEY of the form in this buffer."
  (let ((widget (alist-get key lispm-ui-form--widgets)))
    (goto-char (or (widget-field-start widget) (widget-get widget :from)))))

(defun lispm-ui-test--form-type (key text)
  "Type TEXT into the field KEY of the form in this buffer."
  (lispm-ui-test--form-field key)
  (insert text))

(defun lispm-ui-test--form-press (key &optional nth)
  "Press the NTH (default first) widget of the field KEY, as Return would."
  (let ((widgets (seq-filter (lambda (widget) (eq (widget-get widget :lispm-form-key) key))
                             (lispm-ui-test--form-buttons))))
    (widget-apply-action (nth (or nth 0) widgets))))

(defun lispm-ui-test--form-buttons ()
  "The buttons of the form in this buffer, in order."
  (let (buttons)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when-let* ((widget (widget-at (point))))
          (unless (memq widget buttons) (push widget buttons)))
        (forward-char 1)))
    (nreverse buttons)))

(ert-deftest lispm-ui-test-form ()
  "A form written as data is drawn, filled in, checked and submitted."
  (let* ((submitted nil)
         (glyphs nil)
         (drawn 0)
         (lispm-ui-form-glyph-function (lambda (kind state label) (push (list kind state label) glyphs) nil))
         (lispm-ui-form-drawn-hook (list (lambda () (cl-incf drawn))))
         (buffer (lispm-ui-form
                  "Pet"
                  `((name :type text :label "Name" :required t)
                    (notes :type text-area :label "Notes" :lines 3 :help "Anything.")
                    (kind :type radio :label "Kind" :choices ((cat . "Cat") (dog . "Dog")) :value cat)
                    (barks :type check :text "Barks"
                           :when ,(lambda (values) (eq (alist-get 'kind values) 'dog)))
                    (size :type choice :label "Size" :choices ((small . "Small") (big . "Big")))
                    (age :type text :label "Age"
                         :validate ,(lambda (value) (and value (not (string-match-p "\\`[0-9]+\\'" value))
                                                         "a number"))))
                  :submit "Adopt"
                  :on-submit (lambda (values) (setq submitted values)))))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (should (eq major-mode 'lispm-ui-form-mode))
            (let ((text (buffer-string)))
              (should (string-prefix-p " Pet\n" text))
              (should (string-match-p "^ Name: " text))
              (should (string-match-p "^ Anything\\.$" text))
              (should (string-match-p "^ Kind: (\\*) Cat\n ( ) Dog$" text))
              (should (string-match-p "Choose… ▾" text))
              (should (string-match-p "\\[Cancel\\] \\[Adopt\\]" text))
              (should-not (string-match-p "Barks" text)))
            ;; Those who dress it are asked for pictures, and told when it is drawn.
            (should (member '(radio on nil) glyphs))
            (should (member '(button default "Adopt") glyphs))
            (should (member '(popup nil "Choose…") glyphs))
            ;; Each control says what it is, to be drawn otherwise, as focused.
            (should (equal (delq nil (mapcar (lambda (widget) (widget-get widget :lispm-form-control))
                                             (lispm-ui-test--form-buttons)))
                           '(radio radio (popup nil "Choose…") (button nil "Cancel") (button default "Adopt"))))
            (should (= drawn 1))
            ;; It starts in its first field.
            (should (eq (lispm-ui-form--key-at (point)) 'name))
            ;; A required field left empty is refused, and point goes there.
            (lispm-ui-test--form-field 'notes)
            (should-error (lispm-ui-form-submit) :type 'user-error)
            (should (eq (lispm-ui-form--key-at (point)) 'name))
            (lispm-ui-test--form-type 'name "Rex")
            (lispm-ui-test--form-type 'notes "Good boy.\nVery good.")
            (should (equal (lispm-ui-form-value 'name) "Rex"))
            (should (equal (lispm-ui-form-value 'notes) "Good boy.\nVery good."))
            ;; Choosing shows the fields that depend on it, keeping what was typed.
            (lispm-ui-test--form-press 'kind 1)
            (should (eq (lispm-ui-form-value 'kind) 'dog))
            (should (string-match-p "( ) Cat\n (\\*) Dog" (buffer-string)))
            (should (string-match-p "\\[ \\] Barks" (buffer-string)))
            (should (equal (lispm-ui-form-value 'name) "Rex"))
            (lispm-ui-test--form-press 'barks)
            (should (eq (lispm-ui-form-value 'barks) t))
            ;; Whatever shows the form shows a pop-up menu's choices.
            (let* ((asked nil)
                   (lispm-ui-form-choose-function
                    (lambda (label choices value _event) (setq asked (list label choices value)) '(big))))
              (lispm-ui-test--form-press 'size)
              (should (equal asked '("Size" ((small . "Small") (big . "Big")) nil))))
            (should (eq (lispm-ui-form-value 'size) 'big))
            (should (string-match-p "Big ▾" (buffer-string)))
            ;; Drawn again, as when the look changes, it keeps its values.
            (lispm-ui-form-redraw)
            (should (= drawn 5))
            (should (equal (lispm-ui-form-value 'name) "Rex"))
            (should (string-match-p "Big ▾" (buffer-string)))
            ;; A value its check refuses is refused.
            (lispm-ui-test--form-type 'age "two")
            (should (equal (cadr (should-error (lispm-ui-form-submit) :type 'user-error)) "Age: a number"))
            (lispm-ui-test--form-field 'age)
            (delete-region (point) (+ (point) 3))
            (insert "2")
            (lispm-ui-form-submit))
          (should (equal submitted '((name . "Rex") (notes . "Good boy.\nVery good.") (kind . dog)
                                     (barks . t) (size . big) (age . "2"))))
          (should-not (buffer-live-p buffer)))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest lispm-ui-test-form-buttons ()
  "A row of push buttons does what each says, in the form's buffer, and a
help that is a function says how things stand each time the form is drawn."
  (let* ((pressed nil)
         (stands "Not asked")
         (glyphs nil)
         (submitted nil)
         (lispm-ui-form-glyph-function (lambda (kind state label) (push (list kind state label) glyphs) nil))
         (buffer (lispm-ui-form
                  "Server"
                  `((address :type text :label "Address" :value "http://a:1"
                             :help ,(lambda (values) (format "%s: %s" (alist-get 'address values) stands)))
                    (actions :type buttons
                             :buttons (("Test" . ,(lambda () (push (cons 'test (lispm-ui-form-value 'address)) pressed)))
                                       ("Open" . ,(lambda () (push 'open pressed))))))
                  :on-submit (lambda (values) (setq submitted values)))))
    (unwind-protect
        (progn
          (should (memq 'buttons lispm-ui-form-types))
          (with-current-buffer buffer
            (let ((text (buffer-string)))
              (should (string-match-p "\\[Test\\] \\[Open\\]" text))
              (should (string-match-p "http://a:1: Not asked" text)))
            (should (member '(button nil "Test") glyphs))
            (should (member '(button nil "Open") glyphs))
            ;; Each button is the field's, as the keyboard and a focus ring find it.
            (lispm-ui-test--form-press 'actions 0)
            (lispm-ui-test--form-press 'actions 1)
            (should (equal pressed '(open (test . "http://a:1"))))
            ;; Drawn again, its help says how things stand now.
            (setq stands "Answers")
            (lispm-ui-form-redraw)
            (should (string-match-p "http://a:1: Answers" (buffer-string)))
            (should-not (string-match-p "Not asked" (buffer-string)))
            (lispm-ui-form-submit))
          ;; Buttons have no value of their own.
          (should (equal submitted '((address . "http://a:1")))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest lispm-ui-test-form-layout ()
  "A form is laid out as a dialog: its labels end in one column, its
controls start in the next, its fields reach one right edge; a heading's
fields are framed in a group box whose edges whatever shows the form
gives, a text area is a panel with a side on each of its lines, and the
title goes where the window shows it instead."
  (let* ((parts nil)
         (lispm-ui-form-group-function
          (lambda (part &rest args)
            (push (cons part args) parts)
            (pcase part ('top (format "[%s]" (cadr args))) ('bottom "[end]") ('side "|"))))
         (buffer (with-temp-buffer
                   (lispm-ui-form "Pet" '((who :type heading :label "Pet")
                                       (name :type text :label "Name")
                                       (notes :type text-area :label "Notes" :lines 2)
                                       (kind :type choice :label "Kind" :choices ((cat . "Cat"))))))))
    (unwind-protect
        (with-current-buffer buffer
          (let ((geometry lispm-ui-form--geometry)
                (text (buffer-substring-no-properties (point-min) (point-max))))
            (should (< (plist-get geometry :left) (plist-get geometry :label)
                       (plist-get geometry :control) (plist-get geometry :right)
                       (plist-get geometry :outer)))
            (should (= (plist-get geometry :right)
                       (+ (plist-get geometry :control) (* lispm-ui-form-width (plist-get geometry :em)))))
            ;; The group box, its sides on the rows of its fields.
            (should (string-match-p "^ \\[Pet\\]$" text))
            (should (string-match-p "^ | Name:  .*|$" text))
            (should (string-match-p "^ \\[end\\]$" text))
            (should (assq 'top parts))
            ;; Each label ends where the column does, as its space says.
            (goto-char (point-min))
            (search-forward "Name:")
            (let ((space (get-text-property (- (point) 6) 'display)))
              (should (equal (car space) 'space))
              (should (= (+ (car (plist-get (cdr space) :align-to)) (lispm-ui-form--measure (lispm-ui-form--label-text "Name")))
                         (plist-get geometry :label))))
            ;; A text field reaches the right edge, its padding one blank.
            (let ((field (alist-get 'name lispm-ui-form--widgets)))
              (should (equal (get-text-property (1- (widget-field-end field)) 'display)
                             `(space :align-to (,(plist-get geometry :right)))))
              (should (eq (get-text-property (widget-field-text-end field) 'invisible) t))
              (lispm-ui-test--form-type 'name "Rex")
              (should (equal (get-text-property (1- (widget-field-end field)) 'display)
                             `(space :align-to (,(plist-get geometry :right)))))
              (should-not (get-text-property (- (widget-field-text-end field) 1) 'display))
              (should-not (get-text-property (- (widget-field-text-end field) 1) 'invisible))
              (should (equal (lispm-ui-form-value 'name) "Rex")))
            ;; A text area: its later lines lined up, a side at each line's end.
            (let* ((area (alist-get 'notes lispm-ui-form--widgets))
                   (pads (widget-get area :lispm-form-pads)))
              (should (= (length pads) 2))
              (should (string-match-p "|\\'" (overlay-get (car pads) 'before-string)))
              (should (string-prefix-p " |" (overlay-get (widget-get area :field-overlay) 'line-prefix)))
              ;; Each of its lines starts at its side, the last one too.
              (save-excursion
                (goto-char (overlay-start (widget-get area :field-overlay)))
                (dotimes (_ 2)
                  (should (get-char-property (point) 'line-prefix))
                  (forward-line 1)))
              ;; Typed in, it is shaped again.
              (lispm-ui-test--form-type 'notes "a\nb")
              (should (= (length (widget-get area :lispm-form-pads)) 3)))
            ;; Shown where its window is titled with it, it shows no title.
            (setq lispm-ui-form-show-header nil)
            (lispm-ui-form-redraw)
            (should-not (string-match-p "^ Pet\n" (buffer-string)))
            (should-not (string-match-p "TAB goes" (buffer-string)))))
      (kill-buffer buffer))))

;; Keys go through the command loop, which moves point after each
;; command out of what is displayed otherwise, as a field's padding.
(ert-deftest lispm-ui-test-form-typing ()
  "What is typed into a form, key by key, goes where point is: a text
field keeps what is typed together, its blanks too, and point does not
wander into its padding; a text area takes its lines."
  (let ((buffer (lispm-ui-form "Pet" '((name :type text :label "Name")
                                    (motto :type text :label "Motto")
                                    (notes :type text-area :label "Notes" :lines 2)))))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (execute-kbd-macro (kbd "R e x TAB G o o d SPC d o g C-f C-f ! TAB"))
          (execute-kbd-macro (kbd "S i t s . RET S t a y s ."))
          (should (equal (lispm-ui-form-value 'name) "Rex"))
          (should (equal (lispm-ui-form-value 'motto) "Good dog!"))
          (should (equal (lispm-ui-form-value 'notes) "Sits.\nStays."))
          ;; Typed into a text area, its text takes no face of its frame.
          (let ((area (widget-get (alist-get 'notes lispm-ui-form--widgets) :field-overlay)))
            (should-not (text-property-not-all (overlay-start area) (overlay-end area) 'face nil)))
          ;; What is shown of a field ends where its text does.
          (let ((field (alist-get 'motto lispm-ui-form--widgets)))
            (should (eq (get-text-property (widget-field-text-end field) 'invisible) t))
            (should-not (get-text-property (1- (widget-field-text-end field)) 'invisible))))
      (kill-buffer buffer))))

(ert-deftest lispm-ui-test-form-accessors ()
  "What a form shows is read by its accessors, from its buffer or another."
  (let ((buffer (lispm-ui-form "Pet" '((name :type text :label "Name")))))
    (unwind-protect
        (progn
          (should (equal (lispm-ui-form-title buffer) "Pet"))
          (should (assq 'name (lispm-ui-form-widgets buffer)))
          (should (plist-get (lispm-ui-form-geometry buffer) :right))
          (with-current-buffer buffer
            (should (equal (lispm-ui-form-title) "Pet"))))
      (kill-buffer buffer))))

(ert-deftest lispm-ui-test-form-former-names ()
  "A program written for LispM's forms before they moved here works by
their former names: the functions, the variables that dress them, its
buffer's own, the faces, and the mode, as `derived-mode-p' asks."
  (require 'lispm-form)
  (let* ((drawn 0)
         (lispm-form-drawn-hook (list (lambda () (setq drawn (1+ drawn)))))
         (buffer (lispm-form "Pet" '((name :type text :label "Name" :value "Rex")))))
    (unwind-protect
        (with-current-buffer buffer
          (should (eq major-mode 'lispm-ui-form-mode))
          (should (derived-mode-p 'lispm-form-mode))
          (should (= drawn 1))
          (should (equal lispm-form--title "Pet"))
          (should (equal (lispm-form-value 'name) "Rex"))
          (should (assq 'name lispm-form--widgets))
          (should (eq (widget-get (alist-get 'name lispm-form--widgets) :lispm-form-key) 'name))
          (should (equal (lispm-form-values) '((name . "Rex"))))
          (should (memq 'buttons lispm-form-types))
          (should (eq (get 'lispm-form-help 'face-alias) 'lispm-ui-form-help))
          (lispm-form-redraw)
          (should (= drawn 2)))
      (kill-buffer buffer))))

;;;; lispm-ui-ask

(ert-deftest lispm-ui-test-ask-in-minibuffer ()
  "Without an interface drawing them, an alert is a letter a button in the
minibuffer, quitting choosing Cancel; a question to agree to, `yes-or-no-p';
choices, completion; a file, `read-file-name', never in a dialog box."
  (let ((buttons '(("Don’t Save" discard nil) ("Cancel" cancel cancel) ("Save" save default)))
        (lispm-ui-alert-function nil)
        (lispm-ui-choose-function nil)
        (lispm-ui-choose-file-function nil)
        (asked nil))
    (should (equal (lispm-ui--alert-keys buttons) '((?d "Don’t Save" discard) (?c "Cancel" cancel)
                                                    (?s "Save" save))))
    (cl-letf (((symbol-function 'read-multiple-choice) (lambda (&rest _) '(?s "Save"))))
      (should (eq (lispm-ui-alert 'caution "Save?" buttons) 'save)))
    (cl-letf (((symbol-function 'read-multiple-choice) (lambda (&rest _) (signal 'quit nil))))
      (should (eq (lispm-ui-alert 'caution "Save?" buttons) 'cancel)))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (prompt) (push (list prompt use-dialog-box) asked) t)))
      (should (lispm-ui-confirm "Delete Ada?" "Her memory goes too." "Delete"))
      (should (equal asked '(("Delete Ada? Her memory goes too. " nil)))))
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "Big")))
      (should (equal (lispm-ui-choose '((small . "Small") (big . "Big")) 'small) '(big))))
    (cl-letf (((symbol-function 'read-file-name)
               (lambda (prompt directory &rest _) (push (list prompt directory use-dialog-box) asked)
                 (expand-file-name "lispm-ui.el" lispm-ui-directory))))
      (setq asked nil)
      (should (equal (lispm-ui-choose-file :directory lispm-ui-directory)
                     (expand-file-name "lispm-ui.el" lispm-ui-directory)))
      (should (equal asked `(("Choose a File: " ,lispm-ui-directory nil)))))))

(ert-deftest lispm-ui-test-ask-drawn ()
  "An interface drawing what is asked is called with what was asked, and
its answer is the answer; a question to agree to is an alert, Cancel
beside the default button its ACTION names."
  (let* ((called nil)
         (lispm-ui-alert-function (lambda (&rest args) (push (cons 'alert args) called) t))
         (lispm-ui-choose-function (lambda (&rest args) (push (cons 'choose args) called) '(big)))
         (lispm-ui-choose-file-function (lambda (&rest args) (push (cons 'file args) called) "/tmp/ada.png")))
    (should (eq (lispm-ui-alert 'note "Done." '(("OK" ok default))) t))
    (should (lispm-ui-confirm "Delete Ada?" nil "Delete"))
    (should (equal (car called) '(alert caution "Delete Ada?" (("Cancel" nil cancel) ("Delete" t default)) nil)))
    (should (equal (lispm-ui-choose '((big . "Big")) nil 1 "Size") '(big)))
    (should (equal (car called) '(choose ((big . "Big")) nil 1 "Size")))
    (should (equal (lispm-ui-choose-file :title "Choose a Picture" :directory "/tmp/" :preview t) "/tmp/ada.png"))
    (should (equal (car called) '(file :title "Choose a Picture" :file nil :directory "/tmp/"
                                       :predicate nil :preview t)))))

;;;; lispm-ui-i18n

(defun lispm-ui-test--messages ()
  "Return the messages the code gives to translate: a hash table from a
catalog key (see `lispm-ui-i18n--key') to a plist: :message, :context,
:plural, the plural's English, and :places, where the message is
written, as FILE:LINE; and :time for a time format, as
`lispm-ui-format-time' takes."
  (let ((messages (make-hash-table :test #'equal)))
    (dolist (file (lispm-ui-test--source-files))
      (with-temp-buffer
        (insert-file-contents file)
        (emacs-lisp-mode)
        (let ((name (file-name-nondirectory file)))
          (goto-char (point-min))
          (while (re-search-forward
                  "(\\(lispm-ui-\\(?:tr\\(?:-n\\|-in\\|-noop\\)?\\|format-time\\)\\)[ \t\n]+\"" nil t)
            (let ((function (intern (match-string 1)))
                  (start (match-beginning 0)))
              (unless (nth 8 (save-excursion (syntax-ppss start)))
                (let* ((line (line-number-at-pos start))
                       (form (save-excursion (goto-char start) (read (current-buffer))))
                       (args (cdr form))
                       (context (cond ((eq function 'lispm-ui-tr-in) (pop args))
                                      ((eq function 'lispm-ui-tr-noop) (nth 1 args))))
                       (message (car args))
                       (key (lispm-ui-i18n--key message context))
                       (entry (gethash key messages)))
                  (puthash key
                           (list :message message :context context
                                 :plural (or (plist-get entry :plural)
                                             (and (eq function 'lispm-ui-tr-n) (nth 1 args)))
                                 :time (or (plist-get entry :time) (eq function 'lispm-ui-format-time))
                                 :places (append (plist-get entry :places) (list (format "%s:%d" name line))))
                           messages))))))))
    messages))

(defun lispm-ui-test--format-arguments (text)
  "Return the arguments format string TEXT takes, as (INDEX . CONVERSION),
sorted: `%1$s' takes argument 1, and a plain `%s' the one after the
last taken."
  (let ((arguments nil) (next 1) (start 0))
    (while (string-match "%\\(?:\\([0-9]+\\)\\$\\)?[-+ #0]*[0-9]*\\(?:\\.[0-9]+\\)?\\([a-zA-Z%]\\)" text start)
      (setq start (match-end 0))
      (unless (equal (match-string 2 text) "%")
        (let ((index (if (match-string 1 text) (string-to-number (match-string 1 text)) next)))
          (push (cons index (match-string 2 text)) arguments)
          (setq next (1+ index)))))
    (sort arguments (lambda (a b) (< (car a) (car b))))))

(defconst lispm-ui-test--translated-languages '(de fr ja zh-Hans zh-Hant)
  "The languages whose catalogs translate every message.")

(defconst lispm-ui-test--spellings
  '(("colour" . "color") ("centre" . "center") ("grey" . "gray") ("favourite" . "favorite")
    ("behaviour" . "behavior") ("cancelled" . "canceled") ("labelled" . "labeled"))
  "Words Britain and the United States spell apart: (BRITISH . AMERICAN),
the start of a word.")

(defun lispm-ui-test--spelled-p (text spellings)
  "Return non-nil when TEXT has a word starting as one of SPELLINGS."
  (let ((case-fold-search t))
    (seq-some (lambda (spelling) (string-match-p (concat "\\b" spelling) text)) spellings)))

(ert-deftest lispm-ui-test-i18n-translates-every-message ()
  "Every language but English translates every message the code gives,
a plural in as many forms as the language has; no catalog holds a
message the code no longer gives; each names its language."
  (let ((messages (lispm-ui-test--messages)))
    (dolist (language (mapcar #'car lispm-ui-languages))
      (let ((catalog (plist-get (lispm-ui-i18n-read-catalog language) :messages))
            (forms (if (alist-get language lispm-ui-i18n-plural-forms) 2 1)))
        (should (plist-get (lispm-ui-i18n-read-catalog language) :name))
        (maphash (lambda (key _)
                   (unless (gethash key messages)
                     (should-not (list language 'stale key))))
                 catalog)
        (when (memq language lispm-ui-test--translated-languages)
          (maphash (lambda (key entry)
                     (let ((translation (gethash key catalog)))
                       (unless (and translation
                                    (if (plist-get entry :plural)
                                        (or (and (stringp translation) (= forms 1))
                                            (and (consp translation) (= (length translation) forms)))
                                      (stringp translation)))
                         (should-not (list language 'missing key)))))
                   messages))))))

(ert-deftest lispm-ui-test-i18n-keeps-arguments ()
  "A translation takes the arguments its message takes, in any order; a
time format, its own."
  (let ((messages (lispm-ui-test--messages)))
    (dolist (language (mapcar #'car lispm-ui-languages))
      (maphash (lambda (key translation)
                 (when-let* ((entry (gethash key messages))
                             ((not (plist-get entry :time))))
                   (let ((expected (lispm-ui-test--format-arguments (plist-get entry :message))))
                     (dolist (text (ensure-list translation))
                       (let ((arguments (lispm-ui-test--format-arguments text)))
                         ;; A plural's form may leave the count unsaid, as
                         ;; "one item" does.
                         (unless (or (equal arguments expected)
                                     (and (plist-get entry :plural)
                                          (equal arguments (cl-remove-if (lambda (a) (equal (cdr a) "d"))
                                                                         expected))))
                           (should-not (list language key text))))))))
               (plist-get (lispm-ui-i18n-read-catalog language) :messages)))))

(ert-deftest lispm-ui-test-i18n-english-spelling ()
  "The code's English spells as Britain does: American English changes
every message spelled so, and British English every one spelled the
American way; neither keeps the other's spelling."
  (let ((messages (lispm-ui-test--messages))
        (british (mapcar #'car lispm-ui-test--spellings))
        (american (mapcar #'cdr lispm-ui-test--spellings)))
    (pcase-dolist (`(,language ,from) `((en-US ,british) (en-GB ,american)))
      (let ((catalog (plist-get (lispm-ui-i18n-read-catalog language) :messages)))
        (maphash (lambda (key entry)
                   (dolist (text (cons (plist-get entry :message) (ensure-list (plist-get entry :plural))))
                     (when (and (lispm-ui-test--spelled-p text from) (not (gethash key catalog)))
                       (should-not (list language 'spelled-as-written key))))
                   (dolist (text (ensure-list (gethash key catalog)))
                     (should-not (and (lispm-ui-test--spelled-p text from) (list language key text)))))
                 messages)))))

(defmacro lispm-ui-test--in-french (&rest body)
  "Run BODY speaking French, from a catalog of the test's own."
  (declare (indent 0))
  `(let ((lispm-ui-i18n--catalogs nil)
         (lispm-ui-language 'fr))
     (cl-letf (((symbol-function 'lispm-ui-i18n-read-catalog)
                (lambda (_language)
                  (let ((table (make-hash-table :test #'equal)))
                    (puthash "Step %d of %d" "Étape %1$d sur %2$d" table)
                    (puthash "%d item" '("%d élément" "%d éléments") table)
                    (puthash (lispm-ui-i18n--key "Open" "state") "Ouvert" table)
                    (puthash "Open" "Ouvrir" table)
                    (list :name "Français" :messages table)))))
       ,@body)))

(ert-deftest lispm-ui-test-i18n-translates ()
  "A message shows in the language spoken, formatted, as written where
the catalog lacks it; a plural takes the language's form for the count;
a message said in a context is looked up in it; the match data stays."
  (lispm-ui-test--in-french
    (should (equal (lispm-ui-tr "Step %d of %d" 2 3) "Étape 2 sur 3"))
    (should (equal (lispm-ui-tr "Not in the catalog") "Not in the catalog"))
    (should (equal (lispm-ui-tr "%d%% done" 5) "5% done"))
    (should (equal (lispm-ui-tr "Open") "Ouvrir"))
    (should (equal (lispm-ui-tr-in "state" "Open") "Ouvert"))
    (should (equal (lispm-ui-tr (lispm-ui-tr-noop "Open" "state")) "Ouvert"))
    (should (equal (mapcar (lambda (n) (lispm-ui-tr-n "%d item" "%d items" n)) '(0 1 2))
                   '("0 élément" "1 élément" "2 éléments")))
    (should (equal (lispm-ui-tr-n "%d page" "%d pages" 0) "0 pages"))
    (should (equal (lispm-ui-language-name 'fr) "Français"))
    (should (equal (string-match "b" "abc") 1))
    (lispm-ui-tr "Open")
    (should (equal (match-beginning 0) 1))))

(ert-deftest lispm-ui-test-i18n-catalogs ()
  "Each language's catalog names it in itself."
  (let ((lispm-ui-i18n--catalogs nil))
    (should (equal (mapcar #'lispm-ui-language-name '(de fr ja zh-Hans zh-Hant))
                   '("Deutsch" "Français" "日本語" "简体中文"
                     "繁體中文")))))

(ert-deftest lispm-ui-test-i18n-language ()
  "The language spoken is `lispm-ui-language', else that of the program
showing LispM UI, else the computer's: the first of its preferred
languages LispM UI speaks, Chinese by script, else by region, English
American in the United States, else British, American English when none
is spoken."
  (should (equal (mapcar #'lispm-ui-i18n-language-of
                         '("zh-Hant-HK" "zh-Hans-HK" "zh-TW" "zh_HK.UTF-8" "zh-CN" "zh" "en" "en-US"
                           "en_US.UTF-8" "en-HK" "en-GB" "fr-CA" "de_CH" "ja-JP" "ko-KR" "C"))
                 '(zh-Hant zh-Hans zh-Hant zh-Hant zh-Hans zh-Hans en-US en-US
                   en-US en-GB en-GB fr de ja nil nil)))
  (let ((lispm-ui-i18n--system-languages '("yue-Hant-HK" "ja-HK" "en-US")))
    (should (eq (lispm-ui-system-language) 'ja)))
  (let ((lispm-ui-i18n--system-languages '("ko-KR")))
    (should (eq (lispm-ui-system-language) 'en-US)))
  (let ((lispm-ui-i18n--system-languages '("zh-Hant-TW"))
        (lispm-ui-language nil))
    (should (eq (lispm-ui-current-language) 'zh-Hant))
    (let ((lispm-ui-language-functions (list #'ignore (lambda () 'de))))
      (should (eq (lispm-ui-current-language) 'de))
      (let ((lispm-ui-language 'fr))
        (should (eq (lispm-ui-current-language) 'fr))))
    ;; A program's language LispM UI does not speak is not followed.
    (let ((lispm-ui-language-functions (list (lambda () 'en))))
      (should (eq (lispm-ui-current-language) 'zh-Hant)))))

(ert-deftest lispm-ui-test-i18n-language-changed ()
  "Setting `lispm-ui-language' runs `lispm-ui-language-changed-hook' once
the command ends, when the language spoken changes; so does a program
saying its language changed, unless `lispm-ui-language' chooses one."
  (let ((lispm-ui-language 'en)
        (lispm-ui-language-changed-hook nil)
        (lispm-ui-i18n--system-languages '("ja-JP"))
        (drawn 0))
    (add-hook 'lispm-ui-language-changed-hook (lambda () (setq drawn (1+ drawn))))
    (unwind-protect
        (progn
          (setq lispm-ui-language 'de)
          (should (lispm-ui-test--wait (lambda () (= drawn 1)) 1))
          (setq lispm-ui-language 'de)
          (accept-process-output nil 0.05)
          (should (= drawn 1))
          (lispm-ui-language-changed)
          (should (= drawn 1))
          (setq lispm-ui-language nil)
          (should (lispm-ui-test--wait (lambda () (= drawn 2)) 1))
          (lispm-ui-language-changed)
          (should (= drawn 3)))
      (setq lispm-ui-language 'en))))

(defun lispm-ui-test-report-catalogs ()
  "Print, for each language, the messages its catalog lacks and those it
holds no longer.  Run with `make i18n'."
  (let ((messages (lispm-ui-test--messages)))
    (dolist (language (mapcar #'car lispm-ui-languages))
      (let ((catalog (plist-get (lispm-ui-i18n-read-catalog language) :messages))
            (missing 0) (stale 0))
        (when (memq language lispm-ui-test--translated-languages)
          (maphash (lambda (key entry)
                     (unless (gethash key catalog)
                       (setq missing (1+ missing))
                       (princ (format "%s: missing %S (%s)\n" language key
                                      (string-join (plist-get entry :places) " ")))))
                   messages))
        (maphash (lambda (key _)
                   (unless (gethash key messages)
                     (setq stale (1+ stale))
                     (princ (format "%s: stale %S\n" language key))))
                 catalog)
        (princ (format "%s: %d missing, %d stale\n" language missing stale))))))

(provide 'lispm-ui-test)
;;; lispm-ui-test.el ends here
