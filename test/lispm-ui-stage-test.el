;;; lispm-ui-stage-test.el --- Tests for FRP stage & spotlight components  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; Witnesses pure derivation laws, glitch-free reactive batching, subject
;; switching, idle auto-close, zoom/unzoom/idle-pinning, and multi-actor
;; spotlight interval composition in `lispm-ui-stage' and `lispm-ui-spotlight'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'lispm-ui)

(ert-deftest lispm-ui-stage-test-pure-derivation-laws ()
  "Derived stage specs obey mode, busy/idle, subject, and zoom invariants."
  (let ((b1 (generate-new-buffer " *ui-stage-t1*"))
        (b2 (generate-new-buffer " *ui-stage-t2*")))
    (unwind-protect
        (let* ((tiles (list (lispm-ui-stage-tile-create :buffer b2 :kind 'shell :seq 2)
                            (lispm-ui-stage-tile-create :buffer b1 :kind 'code :seq 1)))
               (busy-entry (lispm-ui-stage-entry-create :busy-p t :tiles tiles))
               (idle-entry (lispm-ui-stage-entry-create :busy-p nil :tiles tiles))
               (subjects `(("busy-a" . ,busy-entry)
                           ("idle-b" . ,idle-entry))))
          ;; Disabled mode short-circuits regardless of busy subjects.
          (should (equal (lispm-ui-stage-derive-spec nil "busy-a" subjects nil 4)
                         '(:mode disabled)))
          ;; Busy subject projects oldest-to-newest live tiles.
          (should (equal (lispm-ui-stage-derive-spec t "busy-a" subjects nil 4)
                         `(:mode tiled :subject "busy-a"
                                 :tiles ((,b1 . code) (,b2 . shell)))))
          ;; Idle subject without zoom closes stage panes.
          (should (equal (lispm-ui-stage-derive-spec t "idle-b" subjects nil 4)
                         '(:mode closed)))
          ;; Zoom on A does not leak when switching to B.
          (let ((zoom-a (lispm-ui-stage-zoom-create
                         :subject "busy-a" :buffer b1 :kind 'code)))
            (should (equal (lispm-ui-stage-derive-spec t "busy-a" subjects zoom-a 4)
                           `(:mode zoomed :subject "busy-a" :buffer ,b1 :kind code)))
            (should (equal (lispm-ui-stage-derive-spec t "idle-b" subjects zoom-a 4)
                           '(:mode closed)))))
      (kill-buffer b1)
      (kill-buffer b2))))

(ert-deftest lispm-ui-stage-test-subject-switch-and-idle-close ()
  "Switching subjects updates stage windows, and idle subjects close stage."
  (let ((chat-a (generate-new-buffer " *ui-chat-a*"))
        (chat-b (generate-new-buffer " *ui-chat-b*"))
        (file-a (generate-new-buffer " *ui-file-a*"))
        (shell-a (generate-new-buffer " *ui-shell-a*"))
        (file-b (generate-new-buffer " *ui-file-b*"))
        (reconciliations 0)
        stage)
    (unwind-protect
        (save-window-excursion
          (set-frame-size (selected-frame) 220 50)
          (delete-other-windows)
          (set-window-buffer (selected-window) chat-a)
          (setq stage
                (lispm-ui-stage-create
                 :primary-window-p (lambda (w)
                                     (memq (window-buffer w) (list chat-a chat-b)))
                 :on-reconcile (lambda (&rest _) (cl-incf reconciliations))))
          (lispm-ui-stage-set-subject stage "a")
          (lispm-ui-stage-set-busy stage "a" t)
          (lispm-ui-stage-present stage "a" file-a 'code t)
          (lispm-ui-stage-present stage "a" shell-a 'shell t)
          (should (= (length (lispm-ui-stage-windows stage)) 2))
          ;; Streaming again into the same buffer short-circuits reconciliation.
          (let ((before reconciliations))
            (lispm-ui-stage-present stage "a" shell-a 'shell t)
            (should (= reconciliations before)))
          ;; Background subject B updates without touching A's stage windows.
          (lispm-ui-stage-set-busy stage "b" t)
          (lispm-ui-stage-present stage "b" file-b 'code t)
          (should (get-buffer-window file-a))
          (should-not (get-buffer-window file-b))
          ;; Switching to busy B replaces A's 2 tiles with B's 1 tile.
          (set-window-buffer (get-buffer-window chat-a) chat-b)
          (lispm-ui-stage-set-subject stage "b")
          (should (= (length (lispm-ui-stage-windows stage)) 1))
          (should (get-buffer-window file-b))
          (should-not (get-buffer-window file-a))
          ;; When B finishes working (idle), stage closes automatically.
          (lispm-ui-stage-set-busy stage "b" nil)
          (should (= (length (lispm-ui-stage-windows stage)) 0))
          ;; Disabling stage (e.g. Files/Shells tab) ignores background work.
          (lispm-ui-stage-set-enabled stage nil)
          (let ((before reconciliations))
            (lispm-ui-stage-set-busy stage "b" t)
            (lispm-ui-stage-present stage "b" file-b 'code t)
            (should (= reconciliations before))
            (should (= (length (lispm-ui-stage-windows stage)) 0))))
      (when stage (lispm-ui-stage-dispose stage))
      (mapc #'kill-buffer (list chat-a chat-b file-a shell-a file-b)))))

(ert-deftest lispm-ui-stage-test-zoom-and-idle-pin ()
  "Zoom freezes background splits; returning after stop keeps 1 focused pane."
  (let ((chat (generate-new-buffer " *ui-zoom-chat*"))
        (b1 (generate-new-buffer " *ui-zoom-1*"))
        (b2 (generate-new-buffer " *ui-zoom-2*"))
        (b3 (generate-new-buffer " *ui-zoom-3*"))
        stage)
    (unwind-protect
        (save-window-excursion
          (set-frame-size (selected-frame) 220 50)
          (delete-other-windows)
          (set-window-buffer (selected-window) chat)
          (setq stage
                (lispm-ui-stage-create
                 :primary-window-p (lambda (w) (eq (window-buffer w) chat))))
          (lispm-ui-stage-set-subject stage "a")
          (lispm-ui-stage-set-busy stage "a" t)
          (lispm-ui-stage-present stage "a" b1 'code t)
          (lispm-ui-stage-present stage "a" b2 'shell t)
          (should (= (length (lispm-ui-stage-windows stage)) 2))
          ;; Select b1 pane -> zooms to 1 stage pane and freezes b3.
          (select-window (get-buffer-window b1))
          (lispm-ui-stage-on-selection-change stage (selected-frame))
          (should (lispm-ui-stage-zoomed-p stage))
          (should (= (length (lispm-ui-stage-windows stage)) 1))
          (lispm-ui-stage-present stage "a" b3 'lisp t)
          (should (= (length (lispm-ui-stage-windows stage)) 1))
          (should (get-buffer-window b1))
          ;; Now subject stops while user was zoomed, then user clicks chat:
          ;; must NOT split back into multi-pane!
          (lispm-ui-stage-set-busy stage "a" nil)
          (select-window (get-buffer-window chat))
          (lispm-ui-stage-on-selection-change stage (selected-frame))
          (should (= (length (lispm-ui-stage-windows stage)) 1))
          (should (get-buffer-window b1)))
      (when stage (lispm-ui-stage-dispose stage))
      (mapc #'kill-buffer (list chat b1 b2 b3)))))

(ert-deftest lispm-ui-spotlight-test-reactive-store-and-intervals ()
  "Reactive spotlight store renders, blends overlapping actors, and fades."
  (let ((buf (generate-new-buffer " *ui-spotlight-buf*"))
        (store (lispm-ui-spotlight-make-store :fade-duration 1.0)))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (dotimes (i 12) (insert (format "line %d\n" (1+ i)))))
          (let ((alice (lispm-ui-spotlight-actor-create
                        :key "alice" :label "alice" :color "#ff5555"))
                (bob (lispm-ui-spotlight-actor-create
                      :key "bob" :label "bob" :color "#50fa7b")))
            (lispm-ui-spotlight-cast store alice buf 2 '((2 . 6))
                                     :note "reading" :now 10.0)
            (lispm-ui-spotlight-cast store bob buf 4 '((4 . 8))
                                     :fade-at 11.0 :now 10.0)
            (with-current-buffer buf
              (let ((ovs (seq-filter (lambda (o) (overlay-get o 'lispm-ui-spotlight))
                                     (overlays-in (point-min) (point-max)))))
                (should (>= (length ovs) 4))))
            ;; Advance past bob's fade duration; alice remains steady.
            (lispm-ui-spotlight-tick store 12.5)
            (should (= (length (lispm-ui-spotlight-beams store)) 1))
            (lispm-ui-spotlight-remove-where store #'identity 12.5)
            (should-not (lispm-ui-spotlight-buffer-active-p store buf))
            (with-current-buffer buf
              (should-not (seq-some (lambda (o) (overlay-get o 'lispm-ui-spotlight))
                                    (overlays-in (point-min) (point-max)))))))
      (lispm-ui-spotlight-dispose-store store)
      (kill-buffer buf))))

(ert-deftest lispm-ui-stage-test-instance-isolation-and-touch-zoom ()
  "Separate stages do not hijack windows, `touch-zoom' stays immutable, and timeout is public."
  (let ((p1 (generate-new-buffer " *p0-s1-p*"))
        (b1 (generate-new-buffer " *p0-s1-b*"))
        (p2 (generate-new-buffer " *p0-s2-p*"))
        (b2 (generate-new-buffer " *p0-s2-b*"))
        (reconciles 0)
        s1 s2)
    (unwind-protect
        (save-window-excursion
          (set-frame-size (selected-frame) 220 50)
          (delete-other-windows)
          (set-window-buffer (selected-window) p1)
          (setq s1 (lispm-ui-stage-create
                    :primary-window-p (lambda (w) (eq (window-buffer w) p1))
                    :zoom-timeout 1
                    :on-reconcile (lambda (&rest _) (cl-incf reconciles)))
                s2 (lispm-ui-stage-create
                    :primary-window-p (lambda (w) (eq (window-buffer w) p2))))
          (lispm-ui-stage-set-subject s1 "a")
          (lispm-ui-stage-present s1 "a" b1 'code t)
          (should (= (length (lispm-ui-stage-windows s1)) 1))
          (should (= (length (lispm-ui-stage-windows s2)) 0))
          ;; Foreign window is ignored by `s2' zoom:
          (lispm-ui-stage-zoom s2 (get-buffer-window b1))
          (should-not (lispm-ui-stage-zoomed-p s2))
          ;; `touch-zoom' replaces zoom struct without triggering `on-reconcile':
          (let ((win1 (get-buffer-window b1)))
            (lispm-ui-stage-zoom s1 win1)
            (let ((before-zoom (lispm-ui-signal-get (lispm-ui-stage-zoom-signal s1)))
                  (before-reconciles reconciles))
              (lispm-ui-stage-touch-zoom s1 win1)
              (let ((after-zoom (lispm-ui-signal-get (lispm-ui-stage-zoom-signal s1))))
                (should-not (eq before-zoom after-zoom))
                (should (= reconciles before-reconciles)))
              (setf (lispm-ui-stage-zoom-last-active
                     (lispm-ui-signal-get (lispm-ui-stage-zoom-signal s1)))
                    (- (float-time) 10.0))
              (lispm-ui-stage-check-timeout s1)
              (should-not (lispm-ui-stage-zoomed-p s1)))))
      (when s1 (lispm-ui-stage-dispose s1))
      (when s2 (lispm-ui-stage-dispose s2))
      (mapc #'kill-buffer (list p1 b1 p2 b2)))))

(provide 'lispm-ui-stage-test)
;;; lispm-ui-stage-test.el ends here
