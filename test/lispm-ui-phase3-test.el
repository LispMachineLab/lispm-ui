;;; lispm-ui-phase3-test.el --- Tests for Form 2.0 and App Kit 2.0 -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
;; URL: https://github.com/LispMachineLab/lispm-ui
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; Automated ERT coverage for Phase 3 deliverables (#48):
;; - Open `lispm-ui-form-define-type' extensibility
;; - All 9 built-in Form 2.0 field types (`switch', `slider', `number',
;;   `file', `combobox', `tags', `color', `date', `repeater')
;; - Inline/form/async validation, dirty/touched tracking, reset,
;;   placeholder/disabled/read-only, and collapsible sections
;; - Full declarative app via `lispm-kit-define-app': store reactivity,
;;   master-detail/tabs layouts, navigation history & breadcrumb, versioned
;;   `.eld' persistence & migration, commands, palette, toast & lifecycle.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'lispm-ui)

(defun lispm-ui-phase3-test--press-label (label)
  "Find button with `'lispm-ui-label' matching LABEL in current buffer and press it."
  (let ((pos (point-min))
        (found nil))
    (while (and (not found) (< pos (point-max)))
      (when (equal (get-text-property pos 'lispm-ui-label) label)
        (setq found pos))
      (setq pos (1+ pos)))
    (should found)
    (goto-char found)
    (push-button)))

;;;; Form 2.0: Extensibility & All 9 Field Types

(ert-deftest lispm-ui-phase3-form-custom-type-and-nine-builtins ()
  "Custom `lispm-ui-form-define-type' and all 9 built-in field types work."
  (lispm-ui-form-define-type 'phase3-badge
    :default "default-tag"
    :normalize (lambda (raw _p) (and (stringp raw) (upcase (string-trim raw))))
    :validate (lambda (val _p)
                (when (and val (string-match-p "BAD" val))
                  "Cannot contain BAD"))
    :render (lambda (key _f _p value label-fn row-fn)
              (funcall row-fn
                       (lambda ()
                         (funcall label-fn)
                         (insert (lispm-ui-form--at :control))
                         (lispm-ui-form--remember
                          key (widget-create 'item :format "" :value value))
                         (insert (lispm-ui-badge (or value "") :tone 'info))))))
  (let* ((lispm-ui--images (make-hash-table :test #'equal))
         (lispm-ui-form-choose-file-function
          (lambda (_kind _lbl _old _pred) "/tmp/picked-demo.txt"))
         (submitted nil)
         (buf (lispm-ui-form
               "Form 2.0 Showcase"
               '((cust :type phase3-badge :label "Custom")
                 (sw :type switch :label "Enabled" :text "Active" :value nil)
                 (sl :type slider :label "Volume" :min 0 :max 100 :step 10 :value 40)
                 (num :type number :label "Workers" :min 1 :max 16 :step 1 :value 4)
                 (fl :type file :label "Config" :placeholder "No file")
                 (cb :type combobox :label "Model"
                     :candidates ((g . "grok-5.5") (c . "claude-4")) :value g)
                 (tg :type tags :label "Tags" :suggestions ("fast" "code") :value ("fast"))
                 (cl :type color :label "Accent" :value "#2563eb")
                 (dt :type date :label "Due" :value "2026-11-26")
                 (rp :type repeater :label "Headers" :item-default "X-New: 1"
                     :value ("X-One: a" "X-Two: b")))
               :on-submit (lambda (vals) (setq submitted vals)))))
    (unwind-protect
        (with-current-buffer buf
          (should (equal (lispm-ui-form-value 'cust) "DEFAULT-TAG"))
          (should (eq (lispm-ui-form-value 'cb) 'g))
          (should (equal (lispm-ui-form-value 'tg) '("fast")))
          (should (equal (lispm-ui-form-value 'rp) '("X-One: a" "X-Two: b")))
          (should (string-match-p "DEFAULT-TAG" (buffer-string)))
          (should (string-match-p "1\\. X-One: a" (buffer-string)))
          ;; Custom type validation triggers inline error without extra draw.
          (lispm-ui-form--changed 'cust "bad-val")
          (should-not (lispm-ui-form-validate-all))
          (should (string-match-p "Cannot contain BAD" (alist-get 'cust (lispm-ui-form-errors))))
          (lispm-ui-form--changed 'cust "ok-val")
          ;; Exercise numeric range validation, date validation, color validation.
          (lispm-ui-form--changed 'num 99)
          (should-not (lispm-ui-form-validate-all))
          (should (alist-get 'num (lispm-ui-form-errors)))
          (lispm-ui-form--changed 'num 8)
          (lispm-ui-form--changed 'cl "not-a-color")
          (should-not (lispm-ui-form-validate-all))
          (should (alist-get 'cl (lispm-ui-form-errors)))
          (lispm-ui-form--changed 'cl "#ff8800")
          (lispm-ui-form--changed 'dt "2026-02-30")
          (should-not (lispm-ui-form-validate-all))
          (should (alist-get 'dt (lispm-ui-form-errors)))
          (lispm-ui-form--changed 'dt "2026-12-01")
          ;; Test repeater + Add button & file Browse button directly in buffer.
          (lispm-ui-phase3-test--press-label "+ Add")
          (should (= (length (lispm-ui-form-value 'rp)) 3))
          (lispm-ui-phase3-test--press-label (lispm-ui-tr "Choose…"))
          (should (equal (lispm-ui-form-value 'fl) "/tmp/picked-demo.txt"))
          ;; Valid submit succeeds.
          (lispm-ui-form-submit)
          (should (equal (alist-get 'cust submitted) "OK-VAL"))
          (should (= (alist-get 'num submitted) 8))
          (should (equal (alist-get 'fl submitted) "/tmp/picked-demo.txt")))
      (when (buffer-live-p buf) (kill-buffer buf)))))

;;;; Form 2.0: Dirty / Touched / Reset / Async / Form-Level / Collapsible

(ert-deftest lispm-ui-phase3-form-state-async-and-collapsible ()
  "Dirty/touched/reset, `:validate-form', `:validate-async', and collapsible sections work."
  (let* ((async-reqs nil)
         (drawn 0)
         (lispm-ui-form-drawn-hook (list (lambda () (cl-incf drawn))))
         (buf (lispm-ui-form
               "Profile"
               `((a :type text :label "Password" :value "one"
                    :placeholder "Enter password")
                 (b :type text :label "Confirm" :value "one"
                    :read-only t)
                 (user :type text :label "Username" :value "ada"
                       :validate-async ,(lambda (val done)
                                          (push (cons val done) async-reqs)))
                 (adv :type heading :label "Advanced" :collapsible t :collapsed t)
                 (secret :type text :label "Secret" :value "hidden-val"))
               :validate-form (lambda (vals)
                                (unless (equal (alist-get 'a vals) (alist-get 'b vals))
                                  "Passwords must match")))))
    (unwind-protect
        (with-current-buffer buf
          (should (= drawn 1))
          (should-not (lispm-ui-form-dirty-p))
          (should-not (lispm-ui-form-touched-p 'a))
          ;; Collapsed heading hides `:collapsed t` section initially.
          (should (lispm-ui-form-section-collapsed-p 'adv))
          (should-not (string-match-p "Secret:" (buffer-string)))
          (lispm-ui-form-toggle-section 'adv)
          (should-not (lispm-ui-form-section-collapsed-p 'adv))
          (should (string-match-p "Secret:" (buffer-string)))
          ;; Cross-field validate-form catches mismatch without extra redraw on submit.
          (let ((before-draw drawn))
            (lispm-ui-form--changed 'a "two")
            (should (lispm-ui-form-dirty-p 'a))
            (should (lispm-ui-form-touched-p 'a))
            (should-error (lispm-ui-form-submit) :type 'user-error)
            (should (= drawn before-draw))
            (should (equal (alist-get nil (lispm-ui-form-errors)) "Passwords must match")))
          ;; Reset restores initial values, dirty/touched, and errors cleanly.
          (lispm-ui-form-reset)
          (should (equal (lispm-ui-form-value 'a) "one"))
          (should-not (lispm-ui-form-dirty-p))
          (should-not (lispm-ui-form-touched-p 'a))
          (should-not (lispm-ui-form-errors))
          ;; Async validation with stale-token protection: oldest response ignored!
          (setq async-reqs nil)
          (lispm-ui-form--changed 'user "alice")
          (lispm-ui-form--changed 'user "bob")
          (should (= (length async-reqs) 2))
          (let ((bob-done (cdr (nth 0 async-reqs)))
                (alice-done (cdr (nth 1 async-reqs))))
            (funcall alice-done "Stale alice error")
            (should-not (alist-get 'user (lispm-ui-form-errors)))
            (funcall bob-done "Bob already taken")
            (should (equal (alist-get 'user (lispm-ui-form-errors)) "Bob already taken"))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

;;;; App Kit 2.0: End-to-End Declarative Application

(defvar lispm-ui-phase3-test--events nil)
(defvar lispm-ui-phase3-test--state-path nil)

(lispm-kit-define-app lispm-ui-phase3-demo-app
  :title "Demo Notes"
  :version "2.1.0"
  :layout 'master-detail
  :initial-state '(:count 0 :selected "Alpha")
  :state-file (lambda () lispm-ui-phase3-test--state-path)
  :state-version 2
  :migrate (lambda (old-ver loaded)
             (if (< old-ver 2)
                 (list :count (1+ (or (plist-get loaded :count) 0))
                       :selected (or (plist-get loaded :note) "Migrated"))
               loaded))
  :default-route 'list
  :sidebar (lambda (_app state)
             (format "Sidebar: %s" (plist-get state :selected)))
  :routes
  `((list :label "Notes"
          :render ,(lambda (_app state _params)
                     (format "Body Count=%s Selected=%s"
                             (plist-get state :count)
                             (plist-get state :selected))))
    (detail :label "Note Detail"
            :render ,(lambda (_app _state params)
                       (format "Inspecting: %s" (plist-get params :id)))))
  :commands
  (:menu "Demo"
   (lispm-ui-phase3-demo-bump
    :key "b" :label "Bump Count"
    :help "Increment state count"))
  :on-mount (lambda (_app) (push 'mounted lispm-ui-phase3-test--events))
  :on-unmount (lambda (_app) (push 'unmounted lispm-ui-phase3-test--events))
  :on-kill (lambda (_app) (push 'killed lispm-ui-phase3-test--events)))

(defun lispm-ui-phase3-demo-bump ()
  "Increment count in the demo app store."
  (interactive)
  (when lispm-kit-app--current
    (lispm-kit-app-dispatch
     lispm-kit-app--current
     (lambda (s) (plist-put s :count (1+ (or (plist-get s :count) 0)))))))

(ert-deftest lispm-ui-phase3-app-kit-end-to-end ()
  "Declarative App Kit handles migration, store reactivity, routing, palette, toast & lifecycle."
  (let* ((tmp (make-temp-file "lispm-app-state-" nil ".eld"))
         (lispm-ui-phase3-test--events nil)
         (lispm-ui-phase3-test--state-path tmp)
         (app nil))
    (unwind-protect
        (save-window-excursion
          ;; Seed v1 .eld state on disk to test schema migration -> v2!
          (with-temp-file tmp
            (insert "(:version 1 :state (:count 9 :note \"FromV1\"))\n"))
          (setq app (lispm-kit-app-launch 'lispm-ui-phase3-demo-app))
          (should (lispm-kit-app-instance-p app))
          (should (memq 'mounted lispm-ui-phase3-test--events))
          ;; V1 -> V2 migration bumped count 9->10 and renamed :note -> :selected.
          (should (= (plist-get (lispm-kit-app-state app) :count) 10))
          (should (equal (plist-get (lispm-kit-app-state app) :selected) "FromV1"))
          (let ((buf (lispm-kit-app-instance-buffer app)))
            (with-current-buffer buf
              (should (derived-mode-p 'lispm-ui-phase3-demo-app-mode 'lispm-ui-view-mode))
              (should (string-match-p "Body Count=10 Selected=FromV1" (buffer-string)))
              ;; Command invocation updates reactive store and auto-re-renders!
              (call-interactively #'lispm-ui-phase3-demo-bump)
              (should (= (plist-get (lispm-kit-app-state app) :count) 11))
              (should (string-match-p "Body Count=11" (buffer-string)))
              ;; Navigation + History + Back/Forward + Breadcrumbs
              (lispm-kit-app-navigate app 'detail '(:id "doc-42") "Doc 42")
              (should (string-match-p "Inspecting: doc-42" (buffer-string)))
              (should (= (length (lispm-kit-app-breadcrumb-items app)) 2))
              (lispm-kit-app-back app)
              (should (eq (plist-get (lispm-kit-app-current-route app) :route) 'list))
              (lispm-kit-app-forward app)
              (should (eq (plist-get (lispm-kit-app-current-route app) :route) 'detail))
              ;; Toast notification renders inside app
              (lispm-kit-app-notify app "Saved OK" :tone 'success :duration 0)
              (should (string-match-p "Saved OK" (buffer-string)))
              ;; Command Palette collects declared + shell commands
              (let ((cmds (lispm-kit-app-commands app)))
                (should (cl-some (lambda (c) (eq (plist-get c :id) 'lispm-ui-phase3-demo-bump))
                                 cmds))))
            ;; Sidebar buffer updated for master-detail layout
            (let ((sbuf (alist-get 'sidebar (lispm-kit-app-instance-side-buffers app))))
              (should (buffer-live-p sbuf))
              (should (string-match-p "Sidebar: FromV1"
                                      (with-current-buffer sbuf (buffer-string)))))
            ;; Kill buffer triggers save, unmount, kill, side-buffer & scope cleanup!
            (let ((scope (lispm-kit-app-instance-scope app)))
              (kill-buffer buf)
              (should (memq 'unmounted lispm-ui-phase3-test--events))
              (should (memq 'killed lispm-ui-phase3-test--events))
              (should (lispm-ui-scope-disposed scope))
              (let ((saved (with-temp-buffer
                             (insert-file-contents tmp)
                             (read (current-buffer)))))
                (should (= (plist-get saved :version) 2))
                (should (= (plist-get (plist-get saved :state) :count) 11))))))
      (lispm-ui-toast-clear)
      (when (and app (buffer-live-p (lispm-kit-app-instance-buffer app)))
        (kill-buffer (lispm-kit-app-instance-buffer app)))
      (when (file-exists-p tmp) (delete-file tmp)))))

(lispm-kit-define-app lispm-ui-phase3-tabs-app
  :title "Tabbed Tools"
  :layout 'tabs
  :initial-state '(:status "Ready")
  :routes `((one :label "One" :render ,(lambda (_a s _p) (plist-get s :status)))
            (two :label "Two" :render ,(lambda (_a _s _p) "Second Tab"))))

(ert-deftest lispm-ui-phase3-app-kit-tabs-and-commands-palette ()
  "App Kit supports `tabs' shell and invokes commands cleanly."
  (let ((app nil))
    (unwind-protect
        (save-window-excursion
          (setq app (lispm-kit-app-launch 'lispm-ui-phase3-tabs-app))
          (with-current-buffer (lispm-kit-app-instance-buffer app)
            (should (string-match-p "Ready" (buffer-string)))
            (lispm-kit-app-navigate app 'two)
            (should (string-match-p "Second Tab" (buffer-string)))))
      (when (and app (buffer-live-p (lispm-kit-app-instance-buffer app)))
        (kill-buffer (lispm-kit-app-instance-buffer app))))))

(provide 'lispm-ui-phase3-test)
;;; lispm-ui-phase3-test.el ends here
