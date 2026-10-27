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
        (unless (equal feature "lispm-ui")
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
