;;; lispm-finder-test.el --- Tests for lispm-finder -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; ERT test suite for `lispm-finder':
;; - Host independence (zero references to `lispm-desktop')
;; - Icon assets & rendering
;; - Directory reading, sorting, navigation, and selection
;; - Trash, rename, and New Folder operations
;; - Modal file & folder chooser (`lispm-finder-choose-file',
;;   `lispm-finder-choose-directory')
;; - Internationalization (`lispm-finder-i18n') completeness

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'lispm-finder)
(require 'lispm-kit-i18n-check)

(defconst lispm-finder-test-root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Root directory of the `lispm-finder' repository.")

;;; Host independence

(ert-deftest lispm-finder-test-no-host-references ()
  "Ensure `lispm-finder' contains no references to `lispm-desktop' or `lispm-agents'."
  (dolist (file '("lispm-finder-i18n.el" "lispm-finder.el"
                  "lispm-finder-render.el" "lispm-finder-choose.el"))
    (with-temp-buffer
      (insert-file-contents (expand-file-name file lispm-finder-test-root))
      (goto-char (point-min))
      (should-not (re-search-forward "lispm-desktop\\|lispm-agent" nil t)))))

;;; Icons & Classification

(ert-deftest lispm-finder-test-icon-classification ()
  "Verify file and directory icon classification."
  (let ((tmp (make-temp-file "lispm-finder-icons-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "src" tmp))
          (make-directory (expand-file-name "repo" tmp))
          (make-directory (expand-file-name "repo/.git" tmp))
          (should (equal (lispm-finder--icon-name tmp '("src" . t)) "folder-32"))
          (should (equal (lispm-finder--icon-name tmp '("repo" . t)) "folder-git-32"))
          (should (equal (lispm-finder--icon-name tmp '("init.el")) "document-lisp-32"))
          (should (equal (lispm-finder--icon-name tmp '("README.md")) "document-markdown-32"))
          (should (equal (lispm-finder--icon-name tmp '("README.org")) "document-org-32"))
          (should (equal (lispm-finder--icon-name tmp '("main.py")) "document-code-32"))
          (should (equal (lispm-finder--icon-name tmp '("photo.png")) "document-image-32"))
          (should (equal (lispm-finder--icon-name tmp '("unknown.xyz")) "document-32")))
      (delete-directory tmp t))))

(ert-deftest lispm-finder-test-icons-exist ()
  "Every icon bundled by `lispm-finder' is resolved and exists in its icon directory."
  (dolist (name '("disk" "document-32" "document-blank" "document-code-32"
                  "document-code" "document-image-32" "document-image"
                  "document-lisp-32" "document-lisp" "document-markdown-32"
                  "document-markdown" "document-org-32" "document-org"
                  "document" "folder-32" "folder-git-32" "folder-git"
                  "folder" "hard-disk-32"))
    (should (file-readable-p (lispm-ui-icon-file name)))
    (should (file-readable-p (expand-file-name (concat name ".svg") lispm-finder-icon-directory)))))

;;; Reading & Rendering

(ert-deftest lispm-finder-test-read-and-render ()
  "Verify `lispm-finder--read' and `lispm-finder--render' in a temp directory."
  (let ((tmp (make-temp-file "lispm-finder-test-" t)))
    (unwind-protect
        (save-window-excursion
          (make-directory (expand-file-name "alpha-dir" tmp))
          (with-temp-file (expand-file-name "beta.txt" tmp)
            (insert "hello"))
          (with-temp-file (expand-file-name "gamma.el" tmp)
            (insert "(provide 'gamma)"))
          (let ((items (lispm-finder--read tmp)))
            (should (= (length items) 3))
            (should (equal (nth 0 items) '("alpha-dir" . t)))
            (should (equal (nth 1 items) '("beta.txt")))
            (should (equal (nth 2 items) '("gamma.el"))))
          (with-temp-buffer
            (set-window-buffer (selected-window) (current-buffer))
            (lispm-finder-mode)
            (setq lispm-finder--directory (file-name-as-directory tmp)
                  lispm-finder--items (lispm-finder--read tmp)
                  lispm-finder--selected "alpha-dir")
            (lispm-finder--render)
            (should (> (buffer-size) 0))
            (goto-char (point-min))
            (let ((found nil))
              (while (and (not found) (< (point) (point-max)))
                (when (get-text-property (point) 'lispm-finder-item)
                  (setq found (get-text-property (point) 'lispm-finder-item)))
                (forward-char 1))
              (should (equal found "alpha-dir")))))
      (delete-directory tmp t))))

;;; Navigation & File Operations

(ert-deftest lispm-finder-test-navigation-history ()
  "Verify `lispm-finder--go', `lispm-finder-back', `lispm-finder-forward', and `lispm-finder-up'."
  (let* ((root (make-temp-file "lispm-finder-nav-" t))
         (sub (expand-file-name "sub" root)))
    (unwind-protect
        (save-window-excursion
          (make-directory sub)
          (with-temp-buffer
            (set-window-buffer (selected-window) (current-buffer))
            (lispm-finder-mode)
            (setq lispm-finder--directory (file-name-as-directory root)
                  lispm-finder--items (lispm-finder--read root))
            (lispm-finder--render)
            (lispm-finder--go sub t)
            (should (equal lispm-finder--directory (file-name-as-directory sub)))
            (should (equal default-directory (file-name-as-directory sub)))
            (should (equal (car lispm-finder--back) (file-name-as-directory root)))
            (lispm-finder-back)
            (should (equal lispm-finder--directory (file-name-as-directory root)))
            (should (equal (car lispm-finder--forward) (file-name-as-directory sub)))
            (lispm-finder-forward)
            (should (equal lispm-finder--directory (file-name-as-directory sub)))
            (lispm-finder-up)
            (should (equal lispm-finder--directory (file-name-as-directory root)))))
      (delete-directory root t))))

(ert-deftest lispm-finder-test-tilde-and-refresh-reconcile ()
  "Literal `~' filenames stay inside the directory, and refresh drops deleted selections."
  (let ((tmp (make-temp-file "lispm-finder-tilde-" t)))
    (unwind-protect
        (save-window-excursion
          (with-temp-file (lispm-finder--child-path "~" tmp)
            (insert "tilde"))
          (with-temp-file (lispm-finder--child-path "gone.txt" tmp)
            (insert "gone"))
          (should (equal (lispm-finder--child-path "~" tmp)
                         (concat (file-name-as-directory (expand-file-name tmp)) "~")))
          (with-temp-buffer
            (set-window-buffer (selected-window) (current-buffer))
            (lispm-finder-mode)
            (lispm-finder--go tmp nil "gone.txt")
            (should (equal lispm-finder--selected "gone.txt"))
            (delete-file (lispm-finder--child-path "gone.txt" tmp))
            (lispm-finder-refresh)
            (should-not lispm-finder--selected)
            (lispm-finder-new-folder "created-folder")
            (should (file-directory-p (lispm-finder--child-path "created-folder" tmp)))
            (should (equal lispm-finder--selected "created-folder"))
            (lispm-finder-rename "renamed-folder")
            (should (file-directory-p (lispm-finder--child-path "renamed-folder" tmp)))
            (should (equal lispm-finder--selected "renamed-folder"))
            (should-error (lispm-finder-new-folder "../escape") :type 'user-error)))
      (delete-directory tmp t))))

;;; Chooser filtering and rendering

(ert-deftest lispm-finder-test-chooser-render-and-filter ()
  "Verify chooser mode filters files in directory-only mode and evaluates footer."
  (let ((tmp (make-temp-file "lispm-finder-chooser-" t)))
    (unwind-protect
        (save-window-excursion
          (make-directory (expand-file-name "folder-a" tmp))
          (with-temp-file (expand-file-name "file-b.txt" tmp)
            (insert "content"))
          (with-temp-buffer
            (set-window-buffer (selected-window) (current-buffer))
            (lispm-finder-choose-mode)
            (setq lispm-finder--predicate #'file-directory-p
                  lispm-finder--choose-title "Choose a Folder")
            (lispm-finder--go tmp nil)
            ;; In directory-only mode, only folder-a is listed
            (should (equal lispm-finder--items '(("folder-a" . t))))
            (should (> (buffer-size) 0))
            (should mode-line-format)
            (let ((footer (lispm-finder--choose-footer)))
              (should (stringp footer))
              (should (text-property-any 0 (length footer)
                                         'lispm-finder-target
                                         lispm-finder--footer-new-folder-target
                                         footer)))
            (should (equal (lispm-finder--choose-target-path)
                           (file-name-as-directory (expand-file-name tmp))))
            (cl-letf (((symbol-function 'read-string)
                       (lambda (_prompt &optional _initial &rest _)
                         "my-new-dir")))
              (call-interactively #'lispm-finder-new-folder))
            (should (equal lispm-finder--selected "my-new-dir"))
            (should (equal (lispm-finder--choose-target-path)
                           (file-name-as-directory
                            (expand-file-name "my-new-dir" tmp))))))
      (delete-directory tmp t))))

(ert-deftest lispm-finder-test-ui-choose-file-registration ()
  "Loading `lispm-finder' sets `lispm-ui-choose-file-function' to `lispm-finder-choose-file'
and exposes public aliases and keybindings for external packages."
  (should (eq lispm-ui-choose-file-function #'lispm-finder-choose-file))
  (should (eq (lookup-key lispm-finder-mode-map (kbd "<right>")) #'lispm-finder-select-right))
  (should (eq (keymap-parent lispm-finder-choose-mode-map) lispm-finder-mode-map))
  (should (eq (lookup-key lispm-finder-choose-mode-map (kbd "<right>")) #'lispm-finder-select-right))
  (should (fboundp 'lispm-finder-path-items))
  (should (fboundp 'lispm-finder-keyboard-menu))
  (should (fboundp 'lispm-finder-choose-header))
  (should (fboundp 'lispm-finder-choose-footer))
  (should (fboundp 'lispm-finder-modal-show-default))
  (should (boundp 'lispm-finder-directory)))

(ert-deftest lispm-finder-test-svg-header-and-footer-drawing ()
  "Verify SVG header strip, bevel buttons, and chooser footer draw cleanly."
  (let ((palette (lispm-ui-derive-palette "#DDDDDD" "#000000" "#888888" "#333399")))
    (should (consp (lispm-finder-draw-header-strip palette 240 "3 items" 40)))
    (should (consp (lispm-finder-draw-header-button palette "▲ Home" nil nil)))
    (should (consp (lispm-finder-draw-header-button palette "New Folder…" 'pressed t)))
    (should (consp (lispm-finder--draw-footer-left palette 260 "~/agents/")))
    (should (consp (lispm-finder--draw-footer-button palette 80 "Cancel")))
    (should (consp (lispm-finder--draw-footer-button palette 88 "Choose" :default t :trailing t)))))

(ert-deftest lispm-finder-test-init-choose-buffer-cleans-up-on-error ()
  "If `lispm-finder--init-choose-buffer' fails mid-init, it kills the newly allocated buffer."
  (let ((tmp (make-temp-file "lispm-finder-err-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "a.txt" tmp)
            (insert "x"))
          (when-let* ((old (get-buffer lispm-finder-choose-buffer-name)))
            (kill-buffer old))
          (should-error
           (lispm-finder--init-choose-buffer
            tmp "Choose" (lambda (_f) (error "Predicate failed")) nil))
          (should-not (get-buffer lispm-finder-choose-buffer-name)))
      (delete-directory tmp t))))

;;; Internationalization

(defun lispm-finder-test--messages ()
  "Return the messages `lispm-finder' gives to translate."
  (lispm-kit-i18n-messages
   (list (expand-file-name "lispm-finder-i18n.el" lispm-finder-test-root)
         (expand-file-name "lispm-finder.el" lispm-finder-test-root)
         (expand-file-name "lispm-finder-render.el" lispm-finder-test-root)
         (expand-file-name "lispm-finder-choose.el" lispm-finder-test-root))
   :tr 'lispm-finder-tr
   :tr-n 'lispm-finder-tr-n
   :tr-in 'lispm-finder-tr-in
   :tr-noop 'lispm-finder-tr-noop
   :format-time 'lispm-finder-format-time))

(ert-deftest lispm-finder-test-i18n-catalogs ()
  "Verify all `lispm-finder' message catalogs are complete and have no missing keys."
  (let ((messages (lispm-finder-test--messages)))
    (should (> (hash-table-count messages) 20))
    (should-not (lispm-kit-i18n-problems 'lispm-finder messages
                                         '(nameless missing arguments spelled-as-written spelling)))))

(ert-deftest lispm-finder-test-pure-keyboard-bindings ()
  "Verify pure-keyboard navigation and modal cancel bindings in Finder and Chooser."
  (should (eq (keymap-lookup lispm-finder-mode-map "M-<left>") #'lispm-finder-back))
  (should (eq (keymap-lookup lispm-finder-mode-map "M-<right>") #'lispm-finder-forward))
  (should (eq (keymap-lookup lispm-finder-choose-mode-map "^") #'lispm-finder-up))
  (should (eq (keymap-lookup lispm-finder-choose-mode-map "q") #'lispm-finder-choose-cancel))
  (should (eq (keymap-lookup lispm-finder-choose-mode-map "M-w") #'lispm-finder-choose-cancel))
  (should (eq (keymap-lookup lispm-finder-choose-mode-map "M-N") #'lispm-finder-new-folder)))

(ert-deftest lispm-finder-test-render-dedup-and-icon-cache ()
  "Verify repeated `lispm-finder--render' calls at unchanged state skip buffer rebuilds and cache icons."
  (let ((tmp (make-temp-file "lispm-finder-dedup-" t)))
    (unwind-protect
        (save-window-excursion
          (make-directory (expand-file-name "sub" tmp))
          (with-temp-file (expand-file-name "one.el" tmp)
            (insert "(provide 'one)"))
          (with-temp-file (expand-file-name "two.txt" tmp)
            (insert "two"))
          (with-temp-buffer
            (set-window-buffer (selected-window) (current-buffer))
            (lispm-finder-mode)
            (setq lispm-finder--directory (file-name-as-directory tmp)
                  lispm-finder--items (lispm-finder--read tmp)
                  lispm-finder--selected "one.el")
            (let ((icon-lookups 0))
              (cl-letf* ((orig-file-icon (symbol-function 'lispm-finder--file-icon-name))
                         ((symbol-function 'lispm-finder--file-icon-name)
                          (lambda (dir item)
                            (setq icon-lookups (1+ icon-lookups))
                            (funcall orig-file-icon dir item))))
                (lispm-finder--render)
                (should (= icon-lookups 3))
                (let ((tick-1 (buffer-chars-modified-tick)))
                  ;; Second render with identical state skips erase-buffer & icon lookups
                  (lispm-finder--render)
                  (should (= (buffer-chars-modified-tick) tick-1))
                  (should (= icon-lookups 3))
                  ;; Changing selection triggers a fresh render, but reuses cached icon names
                  (lispm-finder--select "two.txt")
                  (should (> (buffer-chars-modified-tick) tick-1))
                  (should (= icon-lookups 3)))))))
      (delete-directory tmp t))))

(ert-deftest lispm-finder-test-phase0-validation-and-preview ()
  "Verify invalid name diagnostics and `:preview t' delegation to `lispm-ui-file-dialog'."
  (should (equal (lispm-finder--validate-item-name "notes") "notes"))
  (dolist (bad '("" ".." "a/b"))
    (let ((err (condition-case e
                   (progn (lispm-finder--validate-item-name bad) nil)
                 (user-error (error-message-string e)))))
      (should (stringp err))
      (should (string-prefix-p "Invalid name:" err))))
  (let (delegated)
    (cl-letf (((symbol-function 'lispm-ui-popup-graphic-p) (lambda () t))
              ((symbol-function 'lispm-ui-file-dialog)
               (lambda (&rest args) (setq delegated args) "/tmp/preview.png")))
      (should (equal (lispm-finder-choose-file :title "Pick" :directory "/tmp/" :preview t)
                     "/tmp/preview.png"))
      (should (eq (plist-get delegated :preview) t))
      (should (equal (plist-get delegated :title) "Pick")))))

(ert-deftest lispm-finder-test-fs-info-ttl-cache ()
  "Verify `lispm-finder--info' caches `file-system-info' within `lispm-finder-fs-info-ttl'."
  (let ((lispm-finder--fs-info-cache (make-hash-table :test #'equal))
        (lispm-finder-fs-info-ttl 5.0)
        (fs-calls 0)
        (now 1000.0))
    (cl-letf (((symbol-function 'float-time) (lambda (&optional _) now))
              ((symbol-function 'file-system-info)
               (lambda (_dir) (setq fs-calls (1+ fs-calls)) '(1000 500 500))))
      (let ((lispm-finder--directory "/tmp/dir-a/")
            (lispm-finder--items '(("one.el"))))
        (should (string-match-p "500" (lispm-finder--info)))
        (should (string-match-p "500" (lispm-finder--info)))
        (should (= fs-calls 1))
        ;; Advancing past TTL triggers a fresh query.
        (setq now (+ now 10.0))
        (should (string-match-p "500" (lispm-finder--info)))
        (should (= fs-calls 2)))
      ;; Different directory does not hit dir-a's cached entry.
      (let ((lispm-finder--directory "/tmp/dir-b/")
            (lispm-finder--items '(("two.el"))))
        (lispm-finder--info)
        (should (= fs-calls 3))))))

(provide 'lispm-finder-test)
;;; lispm-finder-test.el ends here
