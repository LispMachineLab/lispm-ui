;;; lispm-ui-scene-test.el --- Tests for spatial scenes and property sheets -*- lexical-binding: t; -*-

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

;; Regression tests for `lispm-ui-scene-layout', `lispm-ui-scene-sprites',
;; `lispm-ui-scene', and `lispm-ui-sheet'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dom)
(require 'seq)
(require 'subr-x)
(require 'lispm-ui)

(setq native-comp-enable-subr-trampolines nil)
(when (boundp 'comp-enable-subr-trampolines)
  (setq comp-enable-subr-trampolines nil))

;;;; Local fixtures

(defun lispm-ui-scene-test--record (id root &rest props)
  "Create a fixture record keyed by ID under ROOT with PROPS."
  (append (list :key (list 'native id)
                :id id
                :kind 'native
                :name (format "Agent-%s" id)
                :root root
                :parent (plist-get props :parent)
                :waits-on (plist-get props :waits-on)
                :status (or (plist-get props :status) '(:state idle))
                :mailbox (or (plist-get props :mailbox) '(:queued 0 :unread 0))
                :activity (plist-get props :activity)
                :avatar (plist-get props :avatar))))

(defun lispm-ui-scene-test--seat-map (state)
  "Return alist of (KEY . (ROOM . SLOT)) from STATE."
  (mapcar (lambda (entry)
            (cons (car entry)
                  (cons (plist-get (cdr entry) :room)
                        (plist-get (cdr entry) :slot))))
          (plist-get state :seats)))

(defun lispm-ui-scene-test--desk-map (plan)
  "Return alist of (KEY . (X Y ROOM SLOT)) from PLAN."
  (mapcar (lambda (node)
            (cons (plist-get node :key)
                  (list (plist-get node :x)
                        (plist-get node :y)
                        (plist-get node :room)
                        (plist-get node :slot))))
          (plist-get plan :nodes)))

(defun lispm-ui-scene-test--room-floor (room)
  "Return walkable floor rectangle (:x :y :width :height) inside ROOM."
  (let ((x (plist-get room :x))
        (y (plist-get room :y))
        (w (plist-get room :width))
        (h (plist-get room :height)))
    (list :x (+ x lispm-ui-scene-wall-thickness)
          :y (+ y lispm-ui-scene-wall-height)
          :width (- w (* 2 lispm-ui-scene-wall-thickness))
          :height (- h lispm-ui-scene-wall-height lispm-ui-scene-wall-thickness))))

(defun lispm-ui-scene-test--rect-inside-p (inner outer)
  "Return non-nil when rectangle INNER lies strictly inside OUTER."
  (and (>= (plist-get inner :x) (plist-get outer :x))
       (>= (plist-get inner :y) (plist-get outer :y))
       (<= (+ (plist-get inner :x) (plist-get inner :width))
           (+ (plist-get outer :x) (plist-get outer :width)))
       (<= (+ (plist-get inner :y) (plist-get inner :height))
           (+ (plist-get outer :y) (plist-get outer :height)))))

(defun lispm-ui-scene-test--plan-sample ()
  "Build a sample scene plan containing blocked parent, helper, and peer."
  (let* ((records
          (list (lispm-ui-scene-test--record
                 "lead" "/tmp/proj/"
                 :status '(:state blocked :kind permission :msg "Allow tool")
                 :mailbox '(:queued 2 :unread 1))
                (lispm-ui-scene-test--record
                 "helper" "/tmp/proj/"
                 :parent '(native "lead")
                 :waits-on '((native "lead"))
                 :status '(:state working :msg "Checking tests"))
                (lispm-ui-scene-test--record
                 "done1" "/tmp/proj/"
                 :status '(:state done))
                (lispm-ui-scene-test--record
                 "other" "/tmp/other/"
                 :status '(:state error :msg "Build failed"))))
         (state (lispm-ui-scene-reconcile nil records)))
    (lispm-ui-scene-plan state records)))

;;;; Layout allocation, pod packing, routing, and persistence

(ert-deftest lispm-ui-scene-layout-empty-and-unknown ()
  "Empty reconciliations produce empty plans; unknown parents fall back cleanly."
  (let* ((empty-state (lispm-ui-scene-reconcile nil nil))
         (empty-plan (lispm-ui-scene-plan empty-state nil)))
    (should (null (plist-get empty-state :rooms)))
    (should (null (plist-get empty-state :seats)))
    (should (null (plist-get empty-plan :rooms)))
    (should (null (plist-get empty-plan :nodes))))
  (let* ((orphan (lispm-ui-scene-test--record
                  "orphan" "/tmp/u/" :parent '(native "missing")))
         (state (lispm-ui-scene-reconcile nil (list orphan)))
         (plan (lispm-ui-scene-plan state (list orphan))))
    (should (equal (lispm-ui-scene-test--seat-map state)
                   '(((native "orphan") 0 . 0))))
    (should (null (lispm-ui-scene-nearest-node (plist-get plan :nodes) nil 1 0)))))

(ert-deftest lispm-ui-scene-layout-order-is-not-position ()
  "Reordering live records never moves already assigned desks."
  (let* ((a (lispm-ui-scene-test--record "a" "/tmp/alpha/"))
         (b (lispm-ui-scene-test--record "b" "/tmp/alpha/"))
         (c (lispm-ui-scene-test--record "c" "/tmp/alpha/"))
         (s1 (lispm-ui-scene-reconcile nil (list a b c)))
         (s2 (lispm-ui-scene-reconcile s1 (list c a b)))
         (p1 (lispm-ui-scene-plan s1 (list a b c)))
         (p2 (lispm-ui-scene-plan s2 (list c a b))))
    (should (equal (lispm-ui-scene-test--seat-map s1)
                   (lispm-ui-scene-test--seat-map s2)))
    (dolist (key '((native "a") (native "b") (native "c")))
      (should (equal (cdr (assoc key (lispm-ui-scene-test--desk-map p1)))
                     (cdr (assoc key (lispm-ui-scene-test--desk-map p2))))))))

(ert-deftest lispm-ui-scene-layout-growth-does-not-move-desks ()
  "Growing a room or adding projects preserves seats and non-sparse desk positions."
  (let* ((a (lispm-ui-scene-test--record "a" "/tmp/alpha/"))
         (b (lispm-ui-scene-test--record "b" "/tmp/alpha/"))
         (c (lispm-ui-scene-test--record "c" "/tmp/beta/"))
         (s1 (lispm-ui-scene-reconcile nil (list a)))
         (s2 (lispm-ui-scene-reconcile s1 (list a b)))
         (p2 (lispm-ui-scene-plan s2 (list a b)))
         (s3 (lispm-ui-scene-reconcile s2 (list a b c)))
         (p3 (lispm-ui-scene-plan s3 (list a b c))))
    (should (equal (cdr (assoc '(native "a") (lispm-ui-scene-test--seat-map s1)))
                   (cdr (assoc '(native "a") (lispm-ui-scene-test--seat-map s3)))))
    (should (equal (cdr (assoc '(native "a") (lispm-ui-scene-test--desk-map p2)))
                   (cdr (assoc '(native "a") (lispm-ui-scene-test--desk-map p3)))))
    (let* ((nine (cl-loop for i from 0 below 9
                          collect (lispm-ui-scene-test--record (format "g%d" i) "/tmp/dense/")))
           (ten (append nine (list (lispm-ui-scene-test--record "g9" "/tmp/dense/"))))
           (sd9 (lispm-ui-scene-reconcile nil nine))
           (pd9 (lispm-ui-scene-plan sd9 nine))
           (sd10 (lispm-ui-scene-reconcile sd9 ten))
           (pd10 (lispm-ui-scene-plan sd10 ten)))
      (should (equal (cdr (assoc '(native "g0") (lispm-ui-scene-test--desk-map pd9)))
                     (cdr (assoc '(native "g0") (lispm-ui-scene-test--desk-map pd10))))))))

(ert-deftest lispm-ui-scene-layout-absent-desks-reserved-until-compact ()
  "Absent agents keep seat reservations across restart until compact."
  (let* ((a (lispm-ui-scene-test--record "a" "/tmp/alpha/"))
         (b (lispm-ui-scene-test--record "b" "/tmp/alpha/"))
         (c (lispm-ui-scene-test--record "c" "/tmp/alpha/"))
         (s1 (lispm-ui-scene-reconcile nil (list a b)))
         (s2 (lispm-ui-scene-reconcile s1 (list b)))
         (p2 (lispm-ui-scene-plan s2 (list b)))
         (s3 (lispm-ui-scene-reconcile s2 (list a b)))
         (s4 (lispm-ui-scene-reconcile s2 (list b c)))
         (s5 (lispm-ui-scene-compact s2 (list b))))
    (should (= (length (plist-get p2 :nodes)) 1))
    (should (equal (cdr (assoc '(native "a") (lispm-ui-scene-test--seat-map s1)))
                   (cdr (assoc '(native "a") (lispm-ui-scene-test--seat-map s3)))))
    (should-not (equal (cdr (assoc '(native "a") (lispm-ui-scene-test--seat-map s1)))
                       (cdr (assoc '(native "c") (lispm-ui-scene-test--seat-map s4)))))
    (should (= (length (plist-get s5 :seats)) 1))
    (should (= (plist-get (cdr (assoc '(native "b") (plist-get s5 :seats))) :slot) 0))))

(ert-deftest lispm-ui-scene-layout-parent-before-child-and-siblings ()
  "Even when listed in reverse order, parents seat adjacent to children."
  (let* ((p (lispm-ui-scene-test--record "p" "/tmp/alpha/"))
         (c1 (lispm-ui-scene-test--record "c1" "/tmp/alpha/" :parent '(native "p")))
         (c2 (lispm-ui-scene-test--record "c2" "/tmp/alpha/" :parent '(native "p")))
         (state (lispm-ui-scene-reconcile nil (list c2 c1 p)))
         (seats (lispm-ui-scene-test--seat-map state))
         (sp (cddr (assoc '(native "p") seats)))
         (s1 (cddr (assoc '(native "c1") seats)))
         (s2 (cddr (assoc '(native "c2") seats))))
    (should (< sp s1))
    (should (< sp s2))
    (should (= (/ sp 4) (/ s1 4)))
    (should (= (/ sp 4) (/ s2 4)))))

(ert-deftest lispm-ui-scene-layout-child-prefers-parents-pod ()
  "Later subagents sit inside their parent's pod before an earlier open slot."
  (let* ((a (lispm-ui-scene-test--record "a" "/tmp/alpha/"))
         (b (lispm-ui-scene-test--record "b" "/tmp/alpha/"))
         (c (lispm-ui-scene-test--record "c" "/tmp/alpha/"))
         (d (lispm-ui-scene-test--record "d" "/tmp/alpha/"))
         (p (lispm-ui-scene-test--record "p" "/tmp/alpha/"))
         (s1 (lispm-ui-scene-reconcile nil (list a b c d p)))
         (s2 (lispm-ui-scene-reconcile s1 (list a c d p)))
         (child (lispm-ui-scene-test--record "child" "/tmp/alpha/" :parent '(native "p")))
         (s3 (lispm-ui-scene-reconcile s2 (list a c d p child)))
         (seats (lispm-ui-scene-test--seat-map s3)))
    (should (= (/ (cddr (assoc '(native "child") seats)) 4)
               (/ (cddr (assoc '(native "p") seats)) 4)))))

(ert-deftest lispm-ui-scene-layout-parent-cycle-and-long-chain ()
  "Parent cycles and deep chains allocate cleanly without infinite recursion."
  (let* ((a (lispm-ui-scene-test--record "a" "/tmp/alpha/" :parent '(native "b")))
         (b (lispm-ui-scene-test--record "b" "/tmp/alpha/" :parent '(native "a")))
         (s-cycle (lispm-ui-scene-reconcile nil (list a b)))
         (chain (cl-loop for i from 0 below 45
                         collect (lispm-ui-scene-test--record
                                  (format "n%d" i) "/tmp/chain/"
                                  :parent (when (> i 0) (list 'native (format "n%d" (1- i)))))))
         (s-chain (lispm-ui-scene-reconcile nil (reverse chain))))
    (should (= (length (plist-get s-cycle :seats)) 2))
    (should (= (length (plist-get s-chain :seats)) 45))))

(ert-deftest lispm-ui-scene-layout-project-relocation-is-local ()
  "Moving one agent between roots leaves bystanders in place."
  (let* ((a1 (lispm-ui-scene-test--record "a1" "/tmp/alpha/"))
         (m  (lispm-ui-scene-test--record "m"  "/tmp/alpha/"))
         (b1 (lispm-ui-scene-test--record "b1" "/tmp/beta/"))
         (s1 (lispm-ui-scene-reconcile nil (list a1 m b1)))
         (m2 (lispm-ui-scene-test--record "m"  "/tmp/beta/"))
         (s2 (lispm-ui-scene-reconcile s1 (list a1 m2 b1)))
         (map1 (lispm-ui-scene-test--seat-map s1))
         (map2 (lispm-ui-scene-test--seat-map s2)))
    (should (equal (cdr (assoc '(native "a1") map1))
                   (cdr (assoc '(native "a1") map2))))
    (should (equal (cdr (assoc '(native "b1") map1))
                   (cdr (assoc '(native "b1") map2))))
    (should (equal (cadr (assoc '(native "m") map2))
                   (cadr (assoc '(native "b1") map2))))))

(ert-deftest lispm-ui-scene-layout-root-normalization-and-home ()
  "Nil and home directory share one room without touching remote paths."
  (let* ((home-a (lispm-ui-scene-test--record "h1" nil))
         (home-b (lispm-ui-scene-test--record "h2" (expand-file-name "~")))
         (remote (lispm-ui-scene-test--record "r1" "/ssh:host:/srv/app"))
         (state (lispm-ui-scene-reconcile nil (list home-a home-b remote)))
         (seats (lispm-ui-scene-test--seat-map state)))
    (should (equal (cadr (assoc '(native "h1") seats))
                   (cadr (assoc '(native "h2") seats))))
    (should-not (equal (cadr (assoc '(native "h1") seats))
                       (cadr (assoc '(native "r1") seats))))
    (should (string-prefix-p "/ssh:host:/srv/app/"
                             (lispm-ui-scene-normalize-root "/ssh:host:/srv/app")))))

(ert-deftest lispm-ui-scene-layout-does-not-mutate-inputs ()
  "Reconcile, compact, fit-room, and plan leave callers' structures untouched."
  (let* ((rec (lispm-ui-scene-test--record "a" "/tmp/alpha/"))
         (records (list rec))
         (rec-copy (copy-tree records))
         (s1 (lispm-ui-scene-reconcile nil records))
         (s1-copy (copy-tree s1))
         (plan (lispm-ui-scene-plan s1 records)))
    (lispm-ui-scene-compact s1 records)
    (lispm-ui-scene-fit-room (car (plist-get plan :rooms)) 700 500)
    (should (equal records rec-copy))
    (should (equal s1 s1-copy))))

(ert-deftest lispm-ui-scene-layout-walls-stay-clear-in-sparse-and-dense-rooms ()
  "Desks never touch walls or overlap each other in rooms up to 16 seats."
  (dolist (count '(1 4 9 16))
    (let* ((records (cl-loop for i from 0 below count
                             collect (lispm-ui-scene-test--record
                                      (format "n%d" i) "/tmp/room/")))
           (state (lispm-ui-scene-reconcile nil records))
           (plan (lispm-ui-scene-plan state records))
           (room (car (plist-get plan :rooms)))
           (floor (lispm-ui-scene-test--room-floor room))
           (nodes (plist-get room :nodes)))
      (dolist (node nodes)
        (should (lispm-ui-scene-test--rect-inside-p node floor)))
      (cl-loop for (a . rest) on nodes do
               (dolist (b rest)
                 (should-not (lispm-ui-scene-overlap-p a b)))))))

(ert-deftest lispm-ui-scene-layout-compact-pods-and-bounded-service-area ()
  "Sixteen packed desks fit inside room bounds without crossing fixtures."
  (let* ((records (cl-loop for i from 0 below 16
                           collect (lispm-ui-scene-test--record
                                    (format "n%d" i) "/tmp/dense/")))
         (state (lispm-ui-scene-reconcile nil records))
         (plan (lispm-ui-scene-plan state records))
         (room (car (plist-get plan :rooms)))
         (floor (lispm-ui-scene-test--room-floor room))
         (nodes (plist-get room :nodes))
         (fixtures (plist-get room :fixtures)))
    (should (= (length nodes) 16))
    (dolist (fixture fixtures)
      (should (lispm-ui-scene-test--rect-inside-p fixture floor))
      (dolist (node nodes)
        (should-not (lispm-ui-scene-overlap-p fixture node))))))

(ert-deftest lispm-ui-scene-layout-walking-follows-clear-orthogonal-aisles ()
  "Walk paths between same-room teammates are purely orthogonal and clear."
  (let* ((records (cl-loop for i from 0 below 8
                           collect (lispm-ui-scene-test--record
                                    (format "w%d" i) "/tmp/walk/")))
         (state (lispm-ui-scene-reconcile nil records))
         (plan (lispm-ui-scene-plan state records))
         (path (lispm-ui-scene-walk-path plan '(native "w0") '(native "w7"))))
    (should path)
    (cl-loop for (a b) on path while b do
             (should (or (= (car a) (car b)) (= (cdr a) (cdr b)))))
    (should (null (lispm-ui-scene-walk-path plan '(native "w0") '(native "w0"))))))

(ert-deftest lispm-ui-scene-layout-cross-room-walking-uses-exterior-corridor ()
  "Cross-room walk paths route through both doorways along the bottom corridor."
  (let* ((a (lispm-ui-scene-test--record "a" "/tmp/room-a/"))
         (b (lispm-ui-scene-test--record "b" "/tmp/room-b/"))
         (state (lispm-ui-scene-reconcile nil (list a b)))
         (plan (lispm-ui-scene-plan state (list a b)))
         (max-y (apply #'max (mapcar (lambda (r) (+ (plist-get r :y) (plist-get r :height)))
                                     (plist-get plan :rooms))))
         (path (lispm-ui-scene-walk-path plan '(native "a") '(native "b"))))
    (should path)
    (should (seq-some (lambda (pt) (> (cdr pt) max-y)) path))))

(ert-deftest lispm-ui-scene-layout-sparse-overview-and-saved-slots ()
  "Sparse rooms center cleanly in overview without altering state seat slots."
  (let* ((a (lispm-ui-scene-test--record "a" "/tmp/one/"))
         (b (lispm-ui-scene-test--record "b" "/tmp/one/"))
         (s1 (lispm-ui-scene-reconcile nil (list a b)))
         (p1 (lispm-ui-scene-plan s1 (list a b)))
         (r1 (car (plist-get p1 :rooms)))
         (s2 (lispm-ui-scene-reconcile s1 (list b))))
    (should (= (plist-get (car (plist-get r1 :nodes)) :display-pod) 0))
    (should (= (plist-get (cdr (assoc '(native "b") (plist-get s2 :seats))) :slot) 1))
    (let* ((p2 (lispm-ui-scene-plan s2 (list b)))
           (n2 (car (plist-get p2 :nodes))))
      (should (= (plist-get n2 :display-seat) 0))
      (should (= (plist-get n2 :slot) 1)))))

(ert-deftest lispm-ui-scene-layout-duplicate-record-and-invalid-key ()
  "Duplicate and invalid scene keys both signal `lispm-ui-scene-error'."
  (should-not (lispm-ui-scene-key-p '(:kw "id")))
  (should-not (lispm-ui-scene-key-p '(native "")))
  (should-error (lispm-ui-scene-reconcile nil (list (list :key '(:bad "1"))))
                :type 'lispm-ui-scene-error)
  (let* ((a1 (lispm-ui-scene-test--record "a" "/tmp/dup/" :name "First"))
         (a2 (lispm-ui-scene-test--record "a" "/tmp/dup/" :name "Second")))
    (should-error (lispm-ui-scene-reconcile nil (list a1 a2))
                  :type 'lispm-ui-scene-error)))

(ert-deftest lispm-ui-scene-layout-persistence-safety ()
  "Writes validate state, reject corruption/collisions, and preserve old files."
  (let ((dir (make-temp-file "lispm-ui-scene-test-" t)))
    (unwind-protect
        (let* ((file (expand-file-name "seats.eld" dir))
               (a (lispm-ui-scene-test--record "a" "/tmp/persist/"))
               (state (lispm-ui-scene-reconcile nil (list a)))
               (corrupt (expand-file-name "corrupt.eld" dir)))
          (should (null (lispm-ui-scene-read file)))
          (lispm-ui-scene-write state file)
          (should (equal (lispm-ui-scene-read file) state))
          (with-temp-file corrupt (insert "(broken\n"))
          (should-error (lispm-ui-scene-write state corrupt)
                        :type 'lispm-ui-scene-error)
          (with-temp-buffer
            (insert-file-contents corrupt)
            (should (equal (buffer-string) "(broken\n")))
          (let ((collision '(:version 1
                             :rooms ((:id 0 :root "/tmp/" :label "tmp"
                                      :x 0 :y 0 :width 450 :height 454))
                             :seats (((native "a") :room 0 :slot 0)
                                     ((native "b") :room 0 :slot 0))))
                (dangling '(:version 1
                            :rooms ()
                            :seats (((native "a") :room 9 :slot 0)))))
            (should-error (lispm-ui-scene-write collision file)
                          :type 'lispm-ui-scene-error)
            (should-error (lispm-ui-scene-write dangling file)
                          :type 'lispm-ui-scene-error)
            (should (equal (lispm-ui-scene-read file) state)))
          (cl-letf (((symbol-function 'rename-file)
                     (lambda (_a _b &optional _c) (error "Disk full"))))
            (should-error (lispm-ui-scene-write state file)))
          (should (equal (lispm-ui-scene-read file) state)))
      (delete-directory dir t))))

;;;; Sprites, portraits, room interiors, and avatar cache

(ert-deftest lispm-ui-scene-sprites-catalogs-and-decks ()
  "Portrait/room decks assign cleanly and avoid immediate repeats across resets."
  (should (= (length lispm-ui-scene-portraits) 16))
  (should (= (length lispm-ui-scene-room-styles) 8))
  (lispm-ui-scene-reset-assignments)
  (let* ((seen (make-hash-table :test #'eql))
         (nodes (cl-loop for i from 0 below 16
                         collect (lispm-ui-scene-test--record (format "p%d" i) "/tmp/d/"))))
    (dolist (node nodes)
      (puthash (lispm-ui-scene-portrait-index node) t seen))
    (should (= (hash-table-count seen) 16))
    (let ((s1 (lispm-ui-scene-room-style (list :id 101 :label "Alpha")))
          (s2 (lispm-ui-scene-room-style (list :id 102 :label "Beta"))))
      (should (plist-get s1 :wall))
      (should (plist-get s2 :floor))))
  (lispm-ui-scene-reset-assignments 'room))

(ert-deftest lispm-ui-scene-sprites-wall-decor-above-floor ()
  "Every room interior style keeps all wall decor strictly above the walkable floor."
  (let* ((plan (lispm-ui-scene-test--plan-sample))
         (room (car (plist-get plan :rooms)))
         (ry (plist-get room :y))
         (floor-top (+ ry lispm-ui-scene-wall-height)))
    (should (> floor-top ry))
    (dotimes (index (length lispm-ui-scene-room-styles))
      (let* ((style (aref lispm-ui-scene-room-styles index))
             (svg (svg-create 600 600)))
        (lispm-ui-scene-draw-room svg room nil 'desk 1.0 style)
        (let ((wall-group (car (dom-by-class svg "office-wall-art"))))
          (when wall-group
            (dolist (rect (dom-by-tag wall-group 'rect))
              (let* ((y (string-to-number (or (dom-attr rect 'y) "0")))
                     (h (string-to-number (or (dom-attr rect 'height) "0"))))
                (should (<= (+ y h) floor-top))))))))))

(ert-deftest lispm-ui-scene-sprites-avatars-and-fallback ()
  "Valid SVG avatars cache until cleared; invalid paths fall back cleanly."
  (let ((dir (make-temp-file "lispm-ui-avatar-" t)))
    (unwind-protect
        (let ((avatar (expand-file-name "bot.svg" dir))
              (corrupt (expand-file-name "bad.svg" dir)))
          (with-temp-file avatar
            (insert "<svg viewBox=\"0 0 16 16\"><rect x=\"0\" y=\"0\" width=\"16\" height=\"16\"/></svg>"))
          (with-temp-file corrupt (insert "<not-an-svg"))
          (lispm-ui-scene-clear-avatar-cache)
          (should (lispm-ui-scene-read-avatar avatar))
          (should (lispm-ui-scene-draw-avatar (svg-create 32 32) avatar 0 0 24))
          (should-not (lispm-ui-scene-read-avatar corrupt))
          (should-not (lispm-ui-scene-draw-avatar (svg-create 32 32) "/no/such/file.svg" 0 0 24))
          (lispm-ui-scene-clear-avatar-cache))
      (delete-directory dir t))))

;;;; Immutable composition, levels, animations, hit-testing, and text

(ert-deftest lispm-ui-scene-responsive-fit-and-levels ()
  "Responsive room fitting adapts to portrait panes; levels reflect scale."
  (should (eq (lispm-ui-scene-level 1.0) 'desk))
  (should (eq (lispm-ui-scene-level 0.5) 'floor))
  (should (eq (lispm-ui-scene-level 0.25) 'map))
  (should-error (lispm-ui-scene-fit-room (car (plist-get (lispm-ui-scene-test--plan-sample) :rooms))
                                         0 400)
                :type 'wrong-type-argument)
  (let* ((records (cl-loop for i from 0 below 6
                           collect (lispm-ui-scene-test--record (format "r%d" i) "/tmp/resp/")))
         (state (lispm-ui-scene-reconcile nil records))
         (plan (lispm-ui-scene-plan state records))
         (room (car (plist-get plan :rooms)))
         (narrow (lispm-ui-scene-fit-room room 160 720))
         (wide (lispm-ui-scene-fit-room room 840 520))
         (n-nodes (plist-get (plist-get narrow :room) :nodes)))
    (should (>= (plist-get wide :scale) (plist-get narrow :scale)))
    (should (apply #'= (mapcar (lambda (n) (plist-get n :x))
                               (seq-take n-nodes 2))))))

(ert-deftest lispm-ui-scene-compose-hit-test-and-docket ()
  "Immutable scene frame couples SVG, actor hits, docket buttons, and desk hits."
  (let* ((plan (lispm-ui-scene-test--plan-sample))
         (visits (make-hash-table :test #'equal))
         (frame (lispm-ui-scene-compose
                 plan :selected '(native "lead") :scale 1.0 :phase 0
                 :room-id 0 :visits visits :now 100.0)))
    (should (lispm-ui-scene-frame-p frame))
    (should (eq (lispm-ui-scene-frame-level frame) 'desk))
    (should (dom-by-tag (lispm-ui-scene-frame-svg frame) 'svg))
    (let* ((proj (lispm-ui-scene-frame-projection frame))
           (scale (plist-get proj :scale))
           (ox (plist-get proj :x))
           (oy (plist-get proj :y))
           (approve (seq-find (lambda (b) (eq (plist-get b :action) 'approve))
                              (lispm-ui-scene-frame-docket-hits frame)))
           (helper-hit (car (lispm-ui-scene-frame-actor-hits frame)))
           (done-node (seq-find (lambda (n) (equal (plist-get n :key) '(native "done1")))
                                (plist-get plan :nodes))))
      (should approve)
      (should (equal (lispm-ui-scene-hit-test
                      frame
                      (* scale (- (+ (plist-get approve :x)
                                     (/ (plist-get approve :width) 2.0))
                                  ox))
                      (* scale (- (+ (plist-get approve :y)
                                     (/ (plist-get approve :height) 2.0))
                                  oy)))
                     '(:action approve :key (native "lead"))))
      (should helper-hit)
      (should (equal (lispm-ui-scene-hit-test
                      frame
                      (* scale (- (+ (plist-get helper-hit :x) 10) ox))
                      (* scale (- (+ (plist-get helper-hit :y) 10) oy)))
                     '(:key (native "helper"))))
      (should done-node)
      (should (equal (lispm-ui-scene-hit-test
                      plan
                      (* scale (- (+ (plist-get done-node :x) 15) ox))
                      (* scale (- (+ (plist-get done-node :y) 15) oy))
                      1.0 0)
                     '(:key (native "done1")))))
    (let* ((map-frame (lispm-ui-scene-compose plan :scale 0.25 :phase 0))
           (map-proj (lispm-ui-scene-frame-projection map-frame))
           (ms (plist-get map-proj :scale))
           (room0 (car (plist-get plan :rooms))))
      (should (eq (lispm-ui-scene-frame-level map-frame) 'map))
      (should (equal (lispm-ui-scene-hit-test
                      map-frame
                      (* ms (+ (plist-get room0 :x) 50))
                      (* ms (+ (plist-get room0 :y) 50)))
                     (list :room (plist-get room0 :id)))))))

(ert-deftest lispm-ui-scene-visitors-transition-and-cleanup ()
  "Explicit visit table transitions from walking to talking and drops stale pairs."
  (let* ((plan (lispm-ui-scene-test--plan-sample))
         (nodes (plist-get (car (plist-get plan :rooms)) :nodes))
         (visits (make-hash-table :test #'equal))
         (early (lispm-ui-scene-visitors plan nodes visits 10.0))
         (late (lispm-ui-scene-visitors plan nodes visits 12.0)))
    (should (eq (plist-get (car early) :pose) 'walking))
    (should (eq (plist-get (car late) :pose) 'talking))
    (should (= (hash-table-count visits) 1))
    (should (null (lispm-ui-scene-visitors
                   plan (list (car nodes)) visits 15.0)))
    (should (= (hash-table-count visits) 0))))

(ert-deftest lispm-ui-scene-states-and-accessible-text ()
  "Canonical status presentation and accessible text agree on every state."
  (dolist (spec '(((:state working) . "Working")
                  ((:state blocked :kind permission) . "Waiting for approval")
                  ((:state blocked :kind question) . "Needs an answer")
                  ((:state blocked :kind auth) . "Needs authentication")
                  ((:state blocked) . "Waiting")
                  ((:state done) . "Complete")
                  ((:state error) . "Error")
                  (nil . "Status unavailable")))
    (should (equal (lispm-ui-scene-state-label (car spec) t) (cdr spec))))
  (let ((plan (lispm-ui-scene-test--plan-sample)))
    (with-temp-buffer
      (lispm-ui-scene-render-text plan '(native "lead") nil 'test-key 'test-room)
      (goto-char (point-min))
      (let ((text (buffer-string)))
        (should (string-match-p "^> ★ Agent-lead" text))
        (should (string-match-p "Waits on: Agent-lead" text))
        (should (string-match-p "\\[done OK\\]" text))
        (should (string-match-p "\\[error ERR\\]" text))
        (should (equal (get-text-property (point-min) 'test-key) nil))
        (re-search-forward "^> ")
        (should (equal (get-text-property (point) 'test-key) '(native "lead")))
        (should (equal (get-text-property (point) 'test-room) 0))))))

;;;; Property sheet chrome

(ert-deftest lispm-ui-sheet-header-tabs-and-buttons ()
  "Property sheet renders headers, chips, owner-buffer tabs, and wrapping buttons."
  (cl-letf (((symbol-function 'lispm-ui-tty-p) (lambda () t)))
    (let ((chip (lispm-ui-sheet-status-chip '(:state blocked :kind permission))))
      (should (string-match-p "Waiting for approval" chip))
      (should (eq (get-text-property 0 'lispm-ui-sheet-status chip) 'blocked)))
    (should (imagep (lispm-ui-sheet-portrait-image (list :id "a" :avatar "/missing.svg") 32)))
    (with-temp-buffer
      (lispm-ui-sheet-header "Planner\nOne" (list :id "a")
                             '(:state working) "Step 1\nStep 2" 48)
      (lispm-ui-sheet-heading "Actions" 48)
      (lispm-ui-sheet-empty "None queued")
      (let* ((text (buffer-string))
             (selected-tab nil)
             (clicked nil)
             (completed nil)
             (flow (lispm-ui-sheet-flow-create :width 0)))
        (should (string-match-p "Planner One" text))
        (should (string-match-p "Working" text))
        (should (string-match-p "Actions" text))
        (should (string-match-p "None queued" text))
        (let ((tab-start (point)))
          (lispm-ui-sheet-tabs '((overview . "Overview") (details . "Details"))
                               'overview 48
                               (lambda (key) (setq selected-tab key)))
          (goto-char tab-start)
          (let ((pos (next-single-property-change tab-start 'action)))
            (when pos
              (funcall (get-text-property pos 'action) nil))))
        (should (memq selected-tab '(overview details)))
        (goto-char (point-max))
        (let ((row-start (point)))
          (lispm-ui-sheet-insert-button
           "First Action" (lambda (arg) (setq clicked arg)) '("ok") 20 flow
           (lambda () (setq completed t)))
          (lispm-ui-sheet-insert-button
           "Second Action" #'ignore nil 20 flow)
          (should (string-match-p "\n" (buffer-substring-no-properties row-start (point))))
          (let ((btn (button-at row-start)))
            (should btn)
            (push-button btn))
          (should (equal clicked "ok"))
          (should completed))))))

(provide 'lispm-ui-scene-test)
;;; lispm-ui-scene-test.el ends here
