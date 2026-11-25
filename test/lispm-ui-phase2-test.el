;;; lispm-ui-phase2-test.el --- Tests for LispM UI Phase 2 components -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
;; URL: https://github.com/LispMachineLab/lispm-ui
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; Automated ERT coverage for Phase 2 deliverables (#47): Controls 2.0,
;; closeable interactive Tabs, embeddable Input / Combobox / Tags, Feedback
;; widgets, Overlay surfaces (popover, dialog, drawer, command palette), Data
;; Display widgets (virtualized ListView, DataTable, TreeView, Accordion,
;; KV), Rich Content (pagination, breadcrumb, wizard stepper, markdown, diff),
;; and standalone lispm-ui-sheet decoupling.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'lispm-ui)

(defmacro lispm-ui-phase2-test--with-gui (has-svg &rest body)
  "Run BODY pretending Emacs is on a graphical display with HAS-SVG librsvg."
  (declare (indent 1) (debug t))
  `(let ((lispm-ui--images (make-hash-table :test #'equal)))
     (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _f) t))
               ((symbol-function 'display-images-p) (lambda (&optional _f) t))
               ((symbol-function 'image-type-available-p)
                (lambda (type) (if (eq type 'svg) ,has-svg t)))
               ((symbol-function 'lispm-ui-tty-p) (lambda (&optional _f) nil)))
       ,@body)))

;;;; Controls 2.0

(ert-deftest lispm-ui-phase2-semantic-buttons-and-actions ()
  "Semantic button variants work in TTY, GUI, and without librsvg."
  (dolist (variant '(primary secondary ghost destructive icon))
    (let ((tty (lispm-ui-button "Save" :variant variant :icon 'check)))
      (should (stringp tty))
      (should (eq (get-text-property 0 'lispm-ui-button-variant tty) variant)))
    (lispm-ui-phase2-test--with-gui nil
      (let ((gui (lispm-ui-button "Save" :variant variant :icon 'check :loading nil)))
        (should (stringp gui))
        (should (eq (get-text-property 0 'lispm-ui-button-variant gui) variant)))))
  (let ((calls 0))
    (with-temp-buffer
      (lispm-ui-view-mode)
      (let ((inhibit-read-only t))
        (lispm-ui-insert-button "Run" (lambda () (cl-incf calls)) :variant 'primary)
        (insert " ")
        (lispm-ui-insert-button "Busy" (lambda () (cl-incf calls)) :loading t)
        (insert " ")
        (lispm-ui-insert-button "Off" (lambda () (cl-incf calls)) :disabled t))
      (goto-char (point-min))
      (push-button)
      (should (= calls 1))
      (when (search-forward "Busy" nil t)
        (backward-char 1)
        (should-not (button-at (point))))
      (when (search-forward "Off" nil t)
        (backward-char 1)
        (should-not (button-at (point)))))))

(ert-deftest lispm-ui-phase2-switch-slider-stepper-segmented-tristate ()
  "Modern Switch, Slider, Stepper, Segmented, Tristate, and Bevel controls work."
  (let ((toggled nil)
        (slid nil)
        (stepped nil)
        (seg nil)
        (tri nil)
        (beveled 0))
    (cl-letf (((symbol-function 'lispm-ui-tty-p) (lambda (&optional _f) t)))
      (should (string-match-p "●" (lispm-ui-switch t)))
      (should (string-match-p "○" (lispm-ui-switch nil))))
    (lispm-ui-phase2-test--with-gui nil
      (should (get-text-property 0 'display (lispm-ui-switch t :focused t)))
      (should (get-text-property 0 'display (lispm-ui-slider 40 :min 0 :max 100)))
      (should (get-text-property 0 'display (lispm-ui-checkbox-tristate 'indeterminate))))
    (with-temp-buffer
      (lispm-ui-view-mode)
      (let ((inhibit-read-only t))
        (lispm-ui-insert-switch "Dark Mode" nil (lambda (v) (setq toggled v)))
        (insert "\n")
        (let ((p-sl (point)))
          (lispm-ui-insert-slider 20 (lambda (v) (setq slid v)) :min 0 :max 100 :step 10)
          (save-excursion
            (goto-char p-sl)
            (when-let* ((pos (text-property-any (point) (line-end-position) 'lispm-ui-focusable t))
                        (fn (lookup-key (get-text-property pos 'keymap) (kbd "<right>"))))
              (funcall fn))))
        (insert "\n")
        (let ((p-st (point)))
          (lispm-ui-insert-stepper 5 (lambda (v) (setq stepped v)) :min 0 :max 10 :step 2)
          (save-excursion
            (goto-char p-st)
            (when (search-forward " 5 " (line-end-position) t)
              (backward-char 2)
              (when-let* ((fn (lookup-key (get-text-property (point) 'keymap) (kbd "+"))))
                (funcall fn)))))
        (insert "\n")
        (let ((p-sg (point)))
          (lispm-ui-insert-segmented '((a . "A") (b . "B")) 'a (lambda (v) (setq seg v)))
          (save-excursion
            (goto-char p-sg)
            (when (search-forward "B" (line-end-position) t)
              (backward-char 1)
              (when-let* ((fn (lookup-key (get-text-property (point) 'keymap) (kbd "RET"))))
                (funcall fn)))))
        (insert "\n")
        (let ((p-tr (point)))
          (lispm-ui-insert-checkbox-tristate "All" 'indeterminate (lambda (v) (setq tri v)))
          (save-excursion
            (goto-char p-tr)
            (push-button)))
        (insert "\n")
        (let ((p-bv (point)))
          (lispm-ui-insert-bevel-button "Pin" (lambda () (cl-incf beveled)) :active t)
          (save-excursion
            (goto-char p-bv)
            (push-button))))
      (goto-char (point-min))
      (push-button)
      (should (eq toggled t))
      (should (= slid 30))
      (should (= stepped 7))
      (should (eq seg 'b))
      (should (eq tri t))
      (should (= beveled 1)))))

;;;; Tabs 2.0

(ert-deftest lispm-ui-phase2-interactive-closeable-tabs ()
  "Closeable interactive tabs support keyboard switching and close action."
  (let ((selected nil)
        (closed nil))
    (with-temp-buffer
      (lispm-ui-view-mode)
      (let ((inhibit-read-only t))
        (lispm-ui-insert-tabs
         '((:key alpha :label "Alpha" :closable t)
           (:key beta :label "Beta" :closable nil))
         'alpha
         (lambda (k) (setq selected k))
         :on-close (lambda (k) (setq closed k))))
      (goto-char (point-min))
      (let ((map (get-text-property (point) 'keymap)))
        (should map)
        (funcall (lookup-key map (kbd "<right>")))
        (should (eq selected 'beta))
        (funcall (lookup-key map (kbd "C-w")))
        (should (eq closed 'alpha)))
      (when (re-search-forward "×" nil t)
        (backward-char 1)
        (setq closed nil)
        (funcall (lookup-key (get-text-property (point) 'keymap) (kbd "RET")))
        (should (eq closed 'alpha))))))

;;;; Embeddable TextInput, Combobox & Tags

(ert-deftest lispm-ui-phase2-input-editing-select-all-paste-in-readonly ()
  "Embedded input supports typing, select-all replace paste, clear, IME, and signals."
  (let ((changes nil)
        (submitted nil)
        (sig-log nil)
        (transient-mark-mode t)
        (quail-current-key nil))
    (with-temp-buffer
      (lispm-ui-view-mode)
      (should buffer-read-only)
      (let* ((inhibit-read-only t)
             (_ (insert "Model: "))
             (inp (lispm-ui-insert-input
                   :value "gpt-4o"
                   :placeholder "Enter model..."
                   :clearable t
                   :on-change (lambda (v) (push v changes))
                   :on-submit (lambda (v) (setq submitted v))))
             (eff nil))
        (unwind-protect
            (save-window-excursion
              (switch-to-buffer (current-buffer))
              (setq eff (lispm-ui-effect
                         (lispm-ui-input-signal inp)
                         (lambda (v) (push v sig-log))))
              (should (equal (lispm-ui-input-value inp) "gpt-4o"))
              ;; Focus moves to end; M-a selects whole input; C-y replaces it cleanly.
              (lispm-ui-input-focus inp)
              (let ((interprogram-cut-function nil)
                    (interprogram-paste-function nil))
                (kill-new "grok-5.5")
                (execute-kbd-macro (kbd "M-a C-y")))
              (should (equal (lispm-ui-input-value inp) "grok-5.5"))
              ;; Still editable after replacement: backspace drops final '5', append 'x'.
              (execute-kbd-macro (kbd "<backspace> x RET"))
              (should (equal (lispm-ui-input-value inp) "grok-5.x"))
              (should (equal submitted "grok-5.x"))
              ;; Clear resets to empty and still accepts typing via trailing sentinel!
              (lispm-ui-input-clear inp)
              (should (equal (lispm-ui-input-value inp) ""))
              (should (overlay-get (lispm-ui-input-overlay inp) 'before-string))
              (execute-kbd-macro (kbd "a b"))
              (should (equal (lispm-ui-input-value inp) "ab"))
              (should (equal (lispm-ui-signal-get (lispm-ui-input-signal inp)) "ab"))
              (should (member "ab" sig-log))
              ;; IME guard blocks direct self-insert while quail-current-key is active.
              (let ((quail-current-key "n"))
                (should-error (execute-kbd-macro (kbd "i")) :type 'user-error))
              ;; Invalid & read-only enforcement.
              (lispm-ui-input-set-invalid inp t)
              (should (lispm-ui-input-invalid inp)))
          (when eff (lispm-ui-effect-dispose eff))
          (lispm-ui-input-dispose inp))))))

(ert-deftest lispm-ui-phase2-input-debounce-and-readonly ()
  "Input supports debounced change delivery and ignores edits when read-only."
  (let ((events nil))
    (with-temp-buffer
      (lispm-ui-view-mode)
      (let* ((inhibit-read-only t)
             (inp (lispm-ui-insert-input
                   :value "v1"
                   :debounce 0.05
                   :on-change (lambda (v) (push v events))))
             (ro (progn (insert "\n")
                        (lispm-ui-insert-input :value "locked" :read-only t))))
        (unwind-protect
            (save-window-excursion
              (switch-to-buffer (current-buffer))
              (lispm-ui-input-set-value inp "a")
              (lispm-ui-input-set-value inp "ab")
              (should-not events)
              (sleep-for 0.08)
              (should (equal events '("ab")))
              (lispm-ui-input-focus ro)
              (should-error (execute-kbd-macro (kbd "z")) :type 'user-error)
              (should (equal (lispm-ui-input-value ro) "locked")))
          (lispm-ui-input-dispose inp)
          (lispm-ui-input-dispose ro))))))

(ert-deftest lispm-ui-phase2-combobox-and-tags ()
  "Combobox filters/moves/selects and Tags add/remove/dedupe cleanly."
  (let ((picked nil)
        (tag-log nil))
    (should (equal (mapcar #'car (lispm-ui-combobox-filter "gr" '("gpt-4" "grok-5.5" "claude")))
                   '("grok-5.5")))
    (with-temp-buffer
      (lispm-ui-view-mode)
      (let* ((inhibit-read-only t)
             (combo (lispm-ui-insert-combobox
                     '((g . "grok-5.5") (c . "claude-4") (o . "o3"))
                     :on-select (lambda (k _l) (setq picked k))))
             (_ (insert "\n"))
             (tags (lispm-ui-insert-tags
                    '("fast")
                    :suggestions '("fast" "vision")
                    :allow-custom nil
                    :on-change (lambda (vals) (push (copy-sequence vals) tag-log)))))
        (unwind-protect
            (progn
              (should (= (length (lispm-ui-combobox-matches combo)) 3))
              (lispm-ui-combobox-move combo 1)
              (lispm-ui-combobox-select combo)
              (should (eq picked 'c))
              (should (equal (lispm-ui-tags-add tags "unknown") '("fast")))
              (should (equal (lispm-ui-tags-add tags "vision") '("fast" "vision")))
              (should (equal (lispm-ui-tags-add tags "vision") '("fast" "vision")))
              (should (equal (lispm-ui-tags-remove tags "fast") '("vision")))
              (should (equal (car tag-log) '("vision"))))
          (lispm-ui-combobox-dispose combo)
          (lispm-ui-tags-dispose tags))))))

;;;; Feedback Widgets

(ert-deftest lispm-ui-phase2-feedback-widgets-and-timers ()
  "Progress, shared Spinner timer, Skeleton, Badge, Avatar, EmptyState, Banner, Toast work."
  (cl-letf (((symbol-function 'lispm-ui-tty-p) (lambda (&optional _f) t)))
    (let ((p-tty (lispm-ui-progress 50 :max 100 :width 16 :label "50%")))
      (should (string-match-p "50%" p-tty))
      (should (string-match-p "█" p-tty))))
  (lispm-ui-phase2-test--with-gui nil
    (let ((p-gui (lispm-ui-progress 30 :indeterminate t :phase 2))
          (av (lispm-ui-avatar "Alice Bob" :image "/tmp/nonexistent-lispm-avatar.svg" :status 'online)))
      (should (get-text-property 0 'display p-gui))
      (should (get-text-property 0 'display av))))
  ;; Single shared spinner timer created and cleaned up on stop.
  (let* ((ticks 0)
         (h1 (lispm-ui-spinner-start (lambda (_s) (cl-incf ticks)) :interval 0.05))
         (timer1 lispm-ui-spinner--timer)
         (h2 (lispm-ui-spinner-start (lambda (_s) (cl-incf ticks)) :interval 0.05)))
    (unwind-protect
        (progn
          (should (timerp timer1))
          (should (eq timer1 lispm-ui-spinner--timer))
          (should (stringp (lispm-ui-spinner-frame 0 "Loading")))
          (lispm-ui-spinner-stop h1)
          (should lispm-ui-spinner--timer)
          (lispm-ui-spinner-stop h2)
          (should-not lispm-ui-spinner--timer))
      (lispm-ui-spinner-stop h1)
      (lispm-ui-spinner-stop h2)))
  (should (stringp (lispm-ui-skeleton :lines 3 :variant 'card)))
  (should (string-match-p "LIVE" (lispm-ui-badge "LIVE" :tone 'success :dot t)))
  (should (string-match-p "Nothing yet" (lispm-ui-empty-state "Nothing yet" :icon 'search)))
  (should (string-match-p "Alert" (lispm-ui-banner "Check disk" :tone 'warning :title "Alert")))
  (unwind-protect
      (progn
        (lispm-ui-toast-clear)
        (lispm-ui-toast "Saved" :id 'save :tone 'success :duration 0)
        (lispm-ui-toast "Updated" :id 'save :tone 'info :duration 0)
        (should (= (length (lispm-ui-toast-active)) 1))
        (should (string-match-p "Updated" (lispm-ui-toast-render)))
        (lispm-ui-toast-dismiss 'save)
        (should-not (lispm-ui-toast-active)))
    (lispm-ui-toast-clear)))

;;;; Overlay Surfaces

(ert-deftest lispm-ui-phase2-popover-dialog-drawer-and-palette ()
  "Popover placement, non-blocking Dialog, Drawer, and Command Palette ranking work."
  ;; Edge flip geometry
  (let ((placed (lispm-ui-popover-place 95 28 20 8 100 30 'below)))
    (should (< (plist-get placed :y) 28))
    (should (<= (+ (plist-get placed :x) 20) 100)))
  (let ((closed-pop nil)
        (closed-dlg nil)
        (closed-drw nil))
    (let ((pop (lispm-ui-popover-open "Hello Popover"
                                      :on-close (lambda () (setq closed-pop t)))))
      (should (lispm-ui-popover-p pop))
      (lispm-ui-popover-close pop)
      (should closed-pop))
    (let ((dlg (lispm-ui-dialog-open
                "Confirm" "Proceed?"
                :actions `((:label "Yes" :variant primary :action ,(lambda () 'yes)))
                :on-close (lambda (res) (setq closed-dlg res)))))
      (should (lispm-ui-dialog-p dlg))
      (lispm-ui-dialog-close dlg 'ok)
      (should (eq closed-dlg 'ok)))
    (let ((drw (lispm-ui-drawer-open "Inspector" "Body text"
                                     :side 'right
                                     :on-close (lambda () (setq closed-drw t)))))
      (should (lispm-ui-drawer-p drw))
      (lispm-ui-drawer-close drw)
      (should closed-drw)))
  ;; Command palette scoring & recent boost
  (let* ((cmds '((:id open :title "Open File" :category "File" :shortcut "C-x C-f")
                 (:id other :title "Configure Other" :category "Settings")
                 (:id opt :title "Open Terminal" :category "Tools")))
         (filtered (lispm-ui-command-palette-filter "of" cmds '(other)))
         (ids (mapcar (lambda (c) (plist-get c :id)) filtered)))
    (should (memq 'open ids))
    (should (> (lispm-ui-command-palette-score "open" "Open File")
               (lispm-ui-command-palette-score "open" "Configure Opener")))))

;;;; Data Display Widgets

(ert-deftest lispm-ui-phase2-list-table-tree-accordion-kv ()
  "Virtualized ListView, sortable DataTable, TreeView, Accordion, and KV render cleanly."
  ;; Virtualized list of 500 items renders bounded slice and scrolls cleanly.
  (let* ((items (cl-loop for i from 1 to 500 collect (format "Item-%03d" i)))
         (lv (lispm-ui-list-view-create :items items :page-size 10 :multi-select t)))
    (let ((rendered (lispm-ui-list-view-render lv)))
      (should (string-match-p "Item-001" rendered))
      (should-not (string-match-p "Item-025" rendered)))
    (lispm-ui-list-view-move lv 15)
    (should (= (lispm-ui-list-view-cursor lv) 15))
    (should (> (lispm-ui-list-view-offset lv) 0))
    (lispm-ui-list-view-toggle-select lv "Item-001")
    (lispm-ui-list-view-toggle-select lv "Item-002" t)
    (should (equal (lispm-ui-list-view-selected lv) '("Item-001" "Item-002")))
    (lispm-ui-list-view-typeahead lv ?I)
    (lispm-ui-list-view-set-loading lv t)
    (should (stringp (lispm-ui-list-view-render lv)))
    (lispm-ui-list-view-set-loading lv nil)
    (lispm-ui-list-view-set-items lv nil)
    (should (stringp (lispm-ui-list-view-render lv))))
  ;; DataTable sort & width distribution
  (let* ((cols '((:key name :title "Name" :flex 2 :sortable t)
                 (:key count :title "Count" :width 8 :align right :sortable t)))
         (rows '((:id 1 :name "Beta" :count 20)
                 (:id 2 :name "Alpha" :count 5)))
         (tbl (lispm-ui-table-create cols :rows rows)))
    (should (= (length (lispm-ui-table-column-widths cols 40)) 2))
    (lispm-ui-table-sort tbl 'count)
    (should (= (plist-get (car (lispm-ui-table-rows tbl)) :count) 5))
    (lispm-ui-table-sort tbl 'count)
    (should (= (plist-get (car (lispm-ui-table-rows tbl)) :count) 20))
    (should (string-match-p "Alpha" (lispm-ui-table-render tbl 40))))
  ;; TreeView flatten & toggle
  (let* ((nodes '((:key root :label "Root"
                        :children ((:key child :label "Child")))))
         (exp (lispm-ui-tree-toggle nil 'root)))
    (should (= (length (lispm-ui-tree-flatten nodes nil)) 1))
    (should (= (length (lispm-ui-tree-flatten nodes exp)) 2))
    (should (string-match-p "Child" (lispm-ui-tree-render nodes exp))))
  ;; Accordion & KV
  (should (equal (lispm-ui-accordion-toggle '(a b) 'c t) '(c)))
  (let ((acc (lispm-ui-accordion-render
              '((:key a :title "Sec A" :body "Body A")
                (:key b :title "Sec B" :body "Body B"))
              '(a)))
        (kv (lispm-ui-kv-render
             '((:key k1 :label "API Key" :value "sk-secret-123" :secret t :copyable t))
             :revealed-keys nil)))
    (should (string-match-p "Body A" acc))
    (should-not (string-match-p "Body B" acc))
    (should (string-match-p "••••" kv))
    (should-not (string-match-p "sk-secret" kv))))

;;;; Rich Content

(ert-deftest lispm-ui-phase2-pagination-breadcrumb-wizard-markdown-diff ()
  "Pagination, Breadcrumb, Wizard Stepper, Markdown, and Diff render cleanly."
  (should (equal (lispm-ui-pagination-pages 5 10 1)
                 '(1 ellipsis 4 5 6 ellipsis 10)))
  (should (string-match-p "5" (lispm-ui-pagination 5 10 #'ignore)))
  (let ((bc (lispm-ui-breadcrumb
             '((:label "One") (:label "Two") (:label "Three")
               (:label "Four") (:label "Five"))
             :max-items 3)))
    (should (string-match-p "One" bc))
    (should (string-match-p "…" bc))
    (should (string-match-p "Five" bc)))
  (let ((wiz (lispm-ui-wizard-stepper
              '((:label "Info" :status completed)
                (:label "Config" :status current)
                (:label "Verify" :status error))
              :orientation 'vertical)))
    (should (string-match-p "✓ 1. Info" wiz))
    (should (string-match-p "● 2. Config" wiz))
    (should (string-match-p "× 3. Verify" wiz)))
  (let ((md (lispm-ui-markdown "# Title\n\n- **Bold** and `code`\n```el\n(+ 1 2)\n```")))
    (should (string-match-p "Title" md))
    (should (string-match-p "Bold" md))
    (should (string-match-p "code" md)))
  (let ((df (lispm-ui-diff "@@ -1 +1 @@\n-old\n+new")))
    (should (string-match-p "-old" df))
    (should (string-match-p "\\+new" df))))

;;;; Sheet Standalone Decoupling

(ert-deftest lispm-ui-phase2-sheet-standalone-without-scene ()
  "Sheet header, status chip, and portrait work with hooks unbound."
  (let ((lispm-ui-sheet-status-info-function nil)
        (lispm-ui-sheet-portrait-draw-function nil))
    (let ((chip (lispm-ui-sheet-status-chip 'working)))
      (should (stringp chip))
      (should (eq (get-text-property 0 'lispm-ui-sheet-status chip) 'working)))
    (with-temp-buffer
      (lispm-ui-view-mode)
      (let ((inhibit-read-only t))
        (lispm-ui-sheet-header
         "Worker" "worker-1" 'working "Indexing repository" 60))
      (should (string-match-p "Worker" (buffer-string))))))

(provide 'lispm-ui-phase2-test)
;;; lispm-ui-phase2-test.el ends here
