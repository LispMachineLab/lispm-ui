;;; lispm-ui-phase1-test.el --- Tests for Design Token 2.0, Icons, View, Layout & FRP 2.0 -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; Comprehensive ERT tests covering Phase 1 (#46):
;; - Design Token 2.0 (`lispm-ui-tokens')
;; - Icon System 2.0 (`lispm-ui-icons', `lispm-ui-icons-catalog')
;; - UI Buffer Hygiene & Scroll Preservation (`lispm-ui-view')
;; - Compositional Layout Primitives & SplitPane (`lispm-ui-layout')
;; - FRP 2.0 Pruning, Resources, Rate-Limiting & Store (`lispm-ui-frp', `lispm-ui-frp-async')

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'lispm-ui)

;;;; Design Token 2.0

(ert-deftest lispm-ui-phase1-test-wcag-and-tokens ()
  "Verify sRGB linearized luminance, WCAG AA enforcement, dark elevation, and scales."
  ;; 1. WCAG 2.1 relative luminance & contrast ratio:
  (should (< (abs (lispm-ui-relative-luminance "#000000")) 1e-6))
  (should (< (abs (- (lispm-ui-relative-luminance "#ffffff") 1.0)) 1e-6))
  (let ((mid (lispm-ui-relative-luminance "#808080")))
    ;; Linearized #808080 (~0.502 sRGB) is ~0.215, NOT ~0.50.
    (should (> mid 0.20))
    (should (< mid 0.23)))
  (should (< (abs (- (lispm-ui-contrast-ratio "#ffffff" "#000000") 21.0)) 1e-3))
  ;; 2. Contrast enforcement meets AA >= 4.5 on both light and dark surfaces:
  (let ((on-white (lispm-ui-ensure-contrast "#f59e0b" "#ffffff" 4.5))
        (on-dark (lispm-ui-ensure-contrast "#1d4ed8" "#18181b" 4.5)))
    (should (>= (lispm-ui-contrast-ratio on-white "#ffffff") 4.5))
    (should (>= (lispm-ui-contrast-ratio on-dark "#18181b") 4.5)))
  ;; 3. Dark theme surface elevation steps progressively lighter:
  (let* ((dark-pal (lispm-ui-derive-palette "#18181b" "#f4f4f5" "#a1a1aa" "#3b82f6"))
         (dark-tok (lispm-ui-derive-tokens dark-pal)))
    (should (lispm-ui-dark-p dark-pal))
    (should (< (lispm-ui-relative-luminance (plist-get dark-tok :surface-0))
               (lispm-ui-relative-luminance (plist-get dark-tok :surface-1))))
    (should (< (lispm-ui-relative-luminance (plist-get dark-tok :surface-1))
               (lispm-ui-relative-luminance (plist-get dark-tok :surface-2))))
    (should (< (lispm-ui-relative-luminance (plist-get dark-tok :surface-2))
               (lispm-ui-relative-luminance (plist-get dark-tok :surface-3))))
    (dolist (role '(success warning info danger))
      (let ((fg (plist-get dark-tok (intern (format ":%s-fg" role))))
            (bg (plist-get dark-tok (intern (format ":%s-bg" role))))
            (on-bg (plist-get dark-tok (intern (format ":%s-on-bg" role)))))
        (should (>= (lispm-ui-contrast-ratio fg "#18181b") 4.45))
        (should (>= (lispm-ui-contrast-ratio on-bg bg) 4.45)))))
  ;; 4. Token fallback to classic 33 slots + scales + faces:
  (let ((lispm-ui-current-palette (lispm-ui-derive-palette "#ffffff" "#000000" "#666666" "#2222cc")))
    (lispm-ui-tokens--sync)
    (should (equal (lispm-ui-token :content) "#ffffff"))
    (should (stringp (lispm-ui-token :hover)))
    (should (stringp (lispm-ui-token :danger-fg)))
    (should (= (lispm-ui-space 'md) 12))
    (should (= (lispm-ui-radius 'lg) 8))
    (should (facep 'lispm-ui-h1))
    (should (facep 'lispm-ui-danger))))

;;;; Icon System 2.0

(ert-deftest lispm-ui-phase1-test-icons-catalog-and-tint ()
  "Verify 50+ built-in icons, declarative registry, currentColor tinting, and TTY glyphs."
  (should (>= (length lispm-ui-icons-catalog) 50))
  (let ((names (lispm-ui-icon-names)))
    (dolist (req '("search" "close" "plus" "minus" "check" "edit" "trash" "copy"
                   "settings" "refresh" "filter" "sort-asc" "sort-desc"
                   "chevron-up" "chevron-down" "arrow-left" "arrow-right"
                   "lock" "unlock" "eye" "eye-off" "bell" "user" "folder"
                   "file" "code" "terminal" "chat" "play" "pause"
                   "download" "upload" "external-link" "info" "warning"
                   "error" "success" "star" "pin" "menu" "more" "sidebar"))
      (should (member req names))
      (should (lispm-ui-icon-p req))))
  ;; Registry + SVG tinting:
  (let ((lispm-ui--icons (make-hash-table :test #'equal))
        (lispm-ui-current-palette (lispm-ui-derive-palette "#ffffff" "#111111" "#666666" "#2563eb")))
    (lispm-ui-tokens--sync)
    (unwind-protect
        (progn
          (lispm-ui-icon-register 'p1-custom
                                  :svg "<svg viewBox=\"0 0 16 16\"><path stroke=\"currentColor\" d=\"M1 1\"/></svg>"
                                  :tty "★")
          (should (lispm-ui-icon-p 'p1-custom))
          (let ((svg (lispm-ui-icon-svg 'search :size 24 :tint "#12ab34")))
            (should (string-match-p "width=\"24\"" svg))
            (should (string-match-p "#12ab34" svg))
            (should-not (string-match-p "currentColor" svg)))
          (let ((custom-svg (lispm-ui-icon-svg 'p1-custom :tint :danger-fg)))
            (should-not (string-match-p "currentColor" custom-svg)))
          (cl-letf (((symbol-function 'lispm-ui-tty-p) (lambda (&optional _) t)))
            (let ((tty-str (lispm-ui-icon 'p1-custom :tint "#aa00aa")))
              (should (equal tty-str "★"))
              (should (equal (plist-get (get-text-property 0 'face tty-str) :foreground)
                             "#aa00aa"))))
          (cl-letf (((symbol-function 'lispm-ui-tty-p) (lambda (&optional _) nil))
                    ((symbol-function 'display-images-p) (lambda (&rest _) t)))
            (let ((img-str (lispm-ui-icon 'search :size 20 :tint "#334455")))
              (should (eq (car (get-text-property 0 'display img-str)) 'image))
              (should (= (plist-get (cdr (get-text-property 0 'display img-str)) :width) 20)))))
      (lispm-ui-icon-unregister 'p1-custom)
      (should-not (lispm-ui-icon-p 'p1-custom)))))

;;;; UI Buffer Hygiene & Scroll Preservation

(ert-deftest lispm-ui-phase1-test-view-mode-and-preserved-scroll ()
  "Verify `lispm-ui-view-mode' hygiene, focus nav, scope kill cleanup, and scroll anchoring."
  (let* ((buf (generate-new-buffer " *p1-view*"))
         (src (lispm-ui-signal 0))
         (hits 0)
         (refreshed 0))
    (unwind-protect
        (save-window-excursion
          (set-frame-size (selected-frame) 100 20)
          (delete-other-windows)
          (switch-to-buffer buf)
          (lispm-ui-view-mode)
          (should truncate-lines)
          (should bidi-inhibit-bpa)
          (should-not global-hl-line-mode)
          (should (= (glyph-char (display-table-slot buffer-display-table 'truncation)) ?…))
          (setq lispm-ui-view-render-function (lambda () (cl-incf refreshed)))
          (lispm-ui-view-refresh)
          (should (= refreshed 1))
          ;; Scope cleans up automatically when buffer is killed:
          (lispm-ui-view-with-scope
            (lispm-ui-effect src (lambda (_v) (cl-incf hits)) t))
          (lispm-ui-signal-set src 1)
          (should (= hits 2))
          ;; Two interactive buttons traversed cleanly with TAB / S-TAB:
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert-text-button "First" 'lispm-ui-focusable t)
            (insert "   ")
            (insert-text-button "Second" 'lispm-ui-focusable t))
          (goto-char (point-min))
          (should (= (lispm-ui-view-next-control)
                     (save-excursion (goto-char (point-min))
                                     (re-search-forward "Second")
                                     (match-beginning 0))))
          (should (= (lispm-ui-view-prev-control) (point-min)))
          ;; Scroll preservation keeps point & row offset locked onto `row-6':
          (let ((win (selected-window))
                (render-rows
                 (lambda (count prefix)
                   (let ((inhibit-read-only t))
                     (erase-buffer)
                     (dotimes (i count)
                       (let ((key (intern (format "row-%d" (1+ i)))))
                         (insert (propertize (format "%s%d\n" prefix (1+ i))
                                             'lispm-ui-item-key key))))))))
            (funcall render-rows 20 "item-")
            (goto-char (lispm-ui-view--find-key-pos 'row-8 'lispm-ui-item-key))
            (set-window-start win (lispm-ui-view--find-key-pos 'row-5 'lispm-ui-item-key))
            (set-window-point win (point))
            (lispm-ui-with-preserved-scroll (:key-prop 'lispm-ui-item-key)
              (let ((inhibit-read-only t))
                (erase-buffer)
                (dotimes (i 5)
                  (insert (propertize (format "prep-%d\n" i)
                                      'lispm-ui-item-key (intern (format "prep-%d" i)))))
                (dotimes (i 20)
                  (insert (propertize (format "item-%d\n" (1+ i))
                                      'lispm-ui-item-key (intern (format "row-%d" (1+ i))))))))
            (should (eq (get-text-property (point) 'lispm-ui-item-key) 'row-8))
            (should (eq (get-text-property (window-point win) 'lispm-ui-item-key) 'row-8))
            (should (eq (get-text-property (window-start win) 'lispm-ui-item-key) 'row-5))))
      (when (buffer-live-p buf)
        (kill-buffer buf)))
    ;; Scope disposed on kill -> signal update does not trigger effect:
    (lispm-ui-signal-set src 2)
    (should (= hits 2))))

;;;; Compositional Layout & SplitPane

(ert-deftest lispm-ui-phase1-test-layout-and-split ()
  "Verify Stack, Divider, Card, Grid, Toolbar/FooterBar, and SplitPane."
  (let ((lispm-ui-current-palette (lispm-ui-derive-palette "#ffffff" "#000000" "#666666" "#2563eb")))
    (lispm-ui-tokens--sync)
    (cl-letf (((symbol-function 'lispm-ui-tty-p) (lambda (&optional _) t)))
      ;; Stacks & divider in TTY:
      (let ((hs (lispm-ui-hstack '("Left" nil flex "Right") :gap 'xs :width 20)))
        (should (string-prefix-p "Left" hs))
        (should (string-suffix-p "Right" hs))
        (should (= (string-width hs) 20)))
      (let ((vs (lispm-ui-vstack '("A" nil "B") :gap 'sm :indent 16)))
        (should (string-match-p "  A\n\n  B" vs)))
      (should (string-match-p "Section" (lispm-ui-divider :width 20 :label "Section")))
      (should (equal (lispm-ui-divider :orientation 'vertical) "│"))
      ;; Card in TTY:
      (let ((card (lispm-ui-card "Hello world" :title "Card" :footer "Done" :width 24)))
        (should (string-match-p "╭─" card))
        (should (string-match-p "Card" card))
        (should (string-match-p "Hello world" card))
        (should (string-match-p "Done" card))
        (should (string-match-p "╯" card)))
      ;; Grid calculations & render:
      (should (= (lispm-ui-grid-columns 300 90 10) 3))
      (should (= (lispm-ui-grid-move 0 'down 7 3) 3))
      (should (= (lispm-ui-grid-move 3 'left 7 3) 2))
      (let (activated)
        (let ((grid (lispm-ui-grid-render '("one" "two" "three")
                                          :width 24 :cell-width 8 :gap 1
                                          :on-activate (lambda (it _) (setq activated it)))))
          (should (string-match-p "one" grid))
          (with-temp-buffer
            (insert grid)
            (goto-char (point-min))
            (let ((match (text-property-search-forward 'lispm-ui-item-key "two" #'equal)))
              (should match)
              (goto-char (prop-match-beginning match))
              (call-interactively (lookup-key (get-text-property (point) 'keymap) (kbd "RET")))
              (should (equal activated "two"))))))
      ;; Toolbar & FooterBar (TTY):
      (let (clicked)
        (let ((bar (lispm-ui-toolbar
                    :left (list (list :label "Run" :icon 'play
                                      :action (lambda () (setq clicked t))))
                    :right "OK"
                    :width 40)))
          (should (string-match-p "Run" bar))
          (should (string-match-p "OK" bar))
          (with-temp-buffer
            (insert bar)
            (let ((pos (text-property-any (point-min) (point-max) 'lispm-ui-focusable t)))
              (should pos)
              (goto-char pos)
              (call-interactively (lookup-key (get-text-property pos 'keymap) (kbd "RET")))
              (should clicked)))))))
  ;; Multi-pane SplitPane:
  (let ((b-main (generate-new-buffer " *p1-split-main*"))
        (b-left (generate-new-buffer " *p1-split-left*"))
        (b-right (generate-new-buffer " *p1-split-right*"))
        (b-bot (generate-new-buffer " *p1-split-bot*"))
        split)
    (unwind-protect
        (save-window-excursion
          (set-frame-size (selected-frame) 160 50)
          (delete-other-windows)
          (set-window-buffer (selected-window) b-main)
          (setq split (lispm-ui-split-create :main-window (selected-window)
                                             :left-width 28 :right-width 36 :bottom-height 10))
          (lispm-ui-split-open split 'left b-left)
          (lispm-ui-split-open split 'right b-right)
          (lispm-ui-split-open split 'bottom b-bot)
          (should (lispm-ui-split-pane-visible-p split 'left))
          (should (lispm-ui-split-pane-visible-p split 'right))
          (should (lispm-ui-split-pane-visible-p split 'bottom))
          (lispm-ui-split-toggle split 'left)
          (should-not (lispm-ui-split-pane-visible-p split 'left))
          (lispm-ui-split-toggle split 'left)
          (should (lispm-ui-split-pane-visible-p split 'left))
          (lispm-ui-split-resize split 'right 44)
          (should (= (lispm-ui-split-right-width split) 44))
          (lispm-ui-split-dispose split)
          (should-not (lispm-ui-split-pane-visible-p split 'right)))
      (when split (lispm-ui-split-dispose split))
      (mapc #'kill-buffer (list b-main b-left b-right b-bot)))))

;;;; FRP 2.0: Pruning, Resource, Debounce/Throttle & Store

(ert-deftest lispm-ui-phase1-test-frp2-pruning-resource-stream-store ()
  "Verify unchanged-value pruning, `lispm-ui-resource', rate-limiting, and `lispm-ui-store'."
  ;; 1. Unchanged intermediate derived value skips downstream `compute':
  (let* ((down-evals 0)
         (src (lispm-ui-signal 2))
         (parity (lispm-ui-signal-map #'cl-evenp src))
         (label (lispm-ui-signal-map
                 (lambda (evenp) (cl-incf down-evals) (if evenp "even" "odd"))
                 parity)))
    (should (equal (lispm-ui-signal-get label) "even"))
    (should (= down-evals 1))
    ;; Changing 2 -> 4 preserves `evenp = t', so `label' does NOT re-evaluate:
    (lispm-ui-signal-set src 4)
    (should (equal (lispm-ui-signal-get label) "even"))
    (should (= down-evals 1))
    ;; Changing 4 -> 5 flips `evenp', triggering `label' evaluation:
    (lispm-ui-signal-set src 5)
    (should (equal (lispm-ui-signal-get label) "odd"))
    (should (= down-evals 2)))
  ;; 2. `lispm-ui-resource' cancels stale requests and tracks lifecycle:
  (let* ((q (lispm-ui-signal "first"))
         (resolvers nil)
         (aborted 0)
         (res (lispm-ui-resource
               (lambda (input resolve _reject)
                 (push (cons input resolve) resolvers)
                 (lambda () (cl-incf aborted)))
               :source q)))
    (should (eq (lispm-ui-resource-status res) 'loading))
    (let ((r1 (cdr (pop resolvers))))
      (lispm-ui-signal-set q "second")
      (should (= aborted 1))
      (let ((r2 (cdr (pop resolvers))))
        ;; Stale resolver from "first" is ignored:
        (funcall r1 "stale-data")
        (should (eq (lispm-ui-resource-status res) 'loading))
        ;; Active resolver transitions cleanly to `ok':
        (funcall r2 "fresh-data")
        (should (eq (lispm-ui-resource-status res) 'ok))
        (should (equal (lispm-ui-resource-data res) "fresh-data"))
        (lispm-ui-resource-mutate res "optimistic")
        (should (equal (lispm-ui-resource-data res) "optimistic"))))
    (lispm-ui-resource-dispose res))
  ;; 3. Debounce & throttle emit properly and clean up timers on scope dispose:
  (let* ((scope (lispm-ui-scope-create))
         (raw (lispm-ui-stream))
         (now 10.0)
         (debounced nil)
         (throttled nil))
    (lispm-ui-with-scope scope
      (let ((d (lispm-ui-stream-debounce 0.03 raw))
            (th (lispm-ui-stream-throttle 0.1 raw (lambda () now))))
        (lispm-ui-stream-subscribe d (lambda (v) (push v debounced)))
        (lispm-ui-stream-subscribe th (lambda (v) (push v throttled)))))
    (lispm-ui-stream-emit raw 'a)
    (lispm-ui-stream-emit raw 'b)
    (should (equal throttled '(a)))
    (should (null debounced))
    (sleep-for 0.06)
    (should (equal debounced '(b)))
    (lispm-ui-scope-dispose scope))
  ;; 4. `lispm-ui-store' batches multi-slice updates & supports select/snapshot/reset:
  (let* ((store (lispm-ui-store '(:count 1 :name "ada")))
         (summary (lispm-ui-store-select
                   store '(:count :name)
                   (lambda (c n) (format "%s:%d" n c))))
         (runs 0)
         (eff (lispm-ui-effect summary (lambda (_) (cl-incf runs)))))
    (unwind-protect
        (progn
          (should (equal (lispm-ui-signal-get summary) "ada:1"))
          (lispm-ui-store-set store :count 2 :name "grace")
          ;; Batched update fires combined effect exactly once:
          (should (= runs 1))
          (should (equal (lispm-ui-signal-get summary) "grace:2"))
          (lispm-ui-store-update store :count #'1+)
          (should (= (lispm-ui-store-get store :count) 3))
          (let ((snap (lispm-ui-store-snapshot store)))
            (should (= (plist-get snap :count) 3)))
          (lispm-ui-store-reset store)
          (should (equal (lispm-ui-signal-get summary) "ada:1")))
      (lispm-ui-effect-dispose eff)
      (lispm-ui-store-dispose store))))

(provide 'lispm-ui-phase1-test)
;;; lispm-ui-phase1-test.el ends here
