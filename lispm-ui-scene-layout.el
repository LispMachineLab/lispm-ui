;;; lispm-ui-scene-layout.el --- Spatial rooms, seats, and aisle geometry -*- lexical-binding: t; -*-

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

;; Pure spatial layout, stable slot reservation, responsive pod packing,
;; orthogonal aisle routing, and validated atomic persistence for agent
;; rooms.  Saved files hold data only, never executable code.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-ui-base)

;;;; Constants & conditions

(defconst lispm-ui-scene-room-width 450
  "Default world-coordinate width of one room.")

(defconst lispm-ui-scene-room-height 454
  "Default world-coordinate height of one room.")

(defconst lispm-ui-scene-wall-height 88
  "World-coordinate height reserved for the room's back wall.")

(defconst lispm-ui-scene-wall-thickness 12
  "World-coordinate thickness of the side and front walls.")

(defconst lispm-ui-scene--file-limit (* 4 1024 1024)
  "Maximum number of bytes read from a saved seating file.")

(define-error 'lispm-ui-scene-error
  "LispM UI scene error" 'lispm-ui-error)

(defun lispm-ui-scene--fail (format-string &rest args)
  "Signal a structured `lispm-ui-scene-error' using FORMAT-STRING and ARGS."
  (signal 'lispm-ui-scene-error (list (apply #'format format-string args))))

;;;; Predicates & validation

;;; Post: Returns non-nil iff KEY is a non-keyword symbol + non-empty string pair.
(defun lispm-ui-scene-key-p (key)
  "Return non-nil when KEY is a valid (KIND SESSION-ID) identity."
  (and (proper-list-p key)
       (= (length key) 2)
       (symbolp (car key))
       (car key)
       (not (keywordp (car key)))
       (stringp (cadr key))
       (not (string-empty-p (cadr key)))))

(defun lispm-ui-scene-normalize-root (root)
  "Return normalized directory string for ROOT without touching the filesystem.
Nil denotes the user's home directory.  Remote TRAMP paths are preserved
verbatim without opening any connection."
  (unless (or (null root) (and (stringp root) (not (string-empty-p root))))
    (lispm-ui-scene--fail "Invalid scene root: %S" root))
  (file-name-as-directory (expand-file-name (or root "~"))))

(defun lispm-ui-scene-overlap-p (a b)
  "Return non-nil when rectangles A and B have overlapping interiors."
  (and (< (plist-get a :x) (+ (plist-get b :x) (plist-get b :width)))
       (< (plist-get b :x) (+ (plist-get a :x) (plist-get a :width)))
       (< (plist-get a :y) (+ (plist-get b :y) (plist-get b :height)))
       (< (plist-get b :y) (+ (plist-get a :y) (plist-get a :height)))))

(defun lispm-ui-scene--plist-p (value keys)
  "Return non-nil when VALUE has each property in KEYS exactly once."
  (and (proper-list-p value)
       (= (length value) (* 2 (length keys)))
       (let ((seen nil)
             (tail value))
         (while (and tail (memq (car tail) keys) (not (memq (car tail) seen)))
           (push (car tail) seen)
           (setq tail (cddr tail)))
         (null tail))))

(defun lispm-ui-scene--validate-room (room expected-id)
  "Signal if ROOM does not match the canonical room schema at EXPECTED-ID."
  (unless (and (lispm-ui-scene--plist-p room '(:id :root :label :x :y :width :height))
               (eql (plist-get room :id) expected-id)
               (stringp (plist-get room :root))
               (equal (plist-get room :root)
                      (lispm-ui-scene-normalize-root (plist-get room :root)))
               (stringp (plist-get room :label))
               (eql (plist-get room :x) (+ 16 (* (% expected-id 3) 474)))
               (eql (plist-get room :y) (+ 16 (* (/ expected-id 3) 478)))
               (eql (plist-get room :width) lispm-ui-scene-room-width)
               (eql (plist-get room :height) lispm-ui-scene-room-height))
    (lispm-ui-scene--fail "Invalid scene room %S" expected-id)))

(defun lispm-ui-scene--validate-seat (entry rooms keys occupied)
  "Validate seat ENTRY against ROOMS table, SEEN KEYS, and OCCUPIED cells."
  (unless (and (consp entry)
               (lispm-ui-scene-key-p (car entry))
               (not (gethash (car entry) keys))
               (lispm-ui-scene--plist-p (cdr entry) '(:room :slot))
               (integerp (plist-get (cdr entry) :room))
               (gethash (plist-get (cdr entry) :room) rooms)
               (integerp (plist-get (cdr entry) :slot))
               (<= 0 (plist-get (cdr entry) :slot))
               (< (plist-get (cdr entry) :slot) 16))
    (lispm-ui-scene--fail "Invalid or duplicate seat identity: %S" (car-safe entry)))
  (let ((position (list (plist-get (cdr entry) :room)
                        (plist-get (cdr entry) :slot))))
    (when (gethash position occupied)
      (lispm-ui-scene--fail "Several actors occupy seat %S" position))
    (puthash position t occupied))
  (puthash (car entry) t keys))

(defun lispm-ui-scene--validate (state)
  "Return STATE if it is a valid seating database, else signal."
  (unless (and (lispm-ui-scene--plist-p state '(:version :rooms :seats))
               (eql (plist-get state :version) 1)
               (proper-list-p (plist-get state :rooms))
               (proper-list-p (plist-get state :seats)))
    (lispm-ui-scene--fail "Invalid scene database or unsupported version"))
  (let ((rooms (make-hash-table :test #'eql))
        (keys (make-hash-table :test #'equal))
        (occupied (make-hash-table :test #'equal))
        (index 0))
    (dolist (room (plist-get state :rooms))
      (lispm-ui-scene--validate-room room index)
      (puthash index room rooms)
      (setq index (1+ index)))
    (dolist (entry (plist-get state :seats))
      (lispm-ui-scene--validate-seat entry rooms keys occupied)))
  state)

(defun lispm-ui-scene--records (records)
  "Index RECORDS by key with an `equal' table; omit keyless records."
  (unless (proper-list-p records)
    (lispm-ui-scene--fail "Invalid scene record list"))
  (let ((index (make-hash-table :test #'equal)))
    (dolist (record records index)
      (unless (and (proper-list-p record) (zerop (% (length record) 2)))
        (lispm-ui-scene--fail "Invalid scene record"))
      (when-let* ((key (plist-get record :key)))
        (unless (lispm-ui-scene-key-p key)
          (lispm-ui-scene--fail "Invalid scene key: %S" key))
        (when (gethash key index)
          (lispm-ui-scene--fail "Duplicate scene key: %S" key))
        (lispm-ui-scene-normalize-root (plist-get record :root))
        (puthash key record index)))))

;;;; Room & seat reconciliation

(cl-defstruct (lispm-ui-scene--alloc
               (:constructor lispm-ui-scene--alloc-create)
               (:copier nil))
  rooms room-index root-index seats occupied new-seats next-room)

(defun lispm-ui-scene--make-room (root id annex)
  "Return a new room plist with ID at normalized ROOT and ANNEX index."
  (let ((name (if (equal root (lispm-ui-scene-normalize-root nil))
                  "Home"
                (let ((leaf (file-name-nondirectory (directory-file-name root))))
                  (if (string-empty-p leaf) root leaf)))))
    (list :id id
          :root root
          :label (if (zerop annex) name (format "%s (%d)" name (1+ annex)))
          :x (+ 16 (* (% id 3) 474))
          :y (+ 16 (* (/ id 3) 478))
          :width lispm-ui-scene-room-width
          :height lispm-ui-scene-room-height)))

(defun lispm-ui-scene--free-slot (occupied room &optional pod)
  "Return first unoccupied slot index in OCCUPIED for ROOM and optional POD."
  (let ((used (gethash (plist-get room :id) occupied)))
    (seq-find (lambda (slot) (not (aref used slot)))
              (if pod
                  (number-sequence (* pod 4) (+ (* pod 4) 3))
                (number-sequence 0 15)))))

(defun lispm-ui-scene--same-root-parent (record records-by-key)
  "Return RECORD's parent key when present in RECORDS-BY-KEY at same root."
  (let* ((key (plist-get record :parent))
         (parent (and (lispm-ui-scene-key-p key)
                      (gethash key records-by-key))))
    (and parent
         (equal (lispm-ui-scene-normalize-root (plist-get parent :root))
                (lispm-ui-scene-normalize-root (plist-get record :root)))
         key)))

(defun lispm-ui-scene--seed-alloc (state records-by-key)
  "Build an initial `lispm-ui-scene--alloc' from STATE and RECORDS-BY-KEY."
  (let ((alloc (lispm-ui-scene--alloc-create
                :rooms (plist-get state :rooms)
                :room-index (make-hash-table :test #'eql)
                :root-index (make-hash-table :test #'equal)
                :seats (make-hash-table :test #'equal)
                :occupied (make-hash-table :test #'eql)
                :next-room (length (plist-get state :rooms)))))
    (dolist (room (lispm-ui-scene--alloc-rooms alloc))
      (puthash (plist-get room :id) room (lispm-ui-scene--alloc-room-index alloc))
      (puthash (plist-get room :id) (make-vector 16 nil)
               (lispm-ui-scene--alloc-occupied alloc))
      (push room (gethash (plist-get room :root)
                          (lispm-ui-scene--alloc-root-index alloc))))
    (maphash (lambda (root values)
               (puthash root (nreverse values)
                        (lispm-ui-scene--alloc-root-index alloc)))
             (lispm-ui-scene--alloc-root-index alloc))
    (dolist (entry (plist-get state :seats) alloc)
      (let* ((key (car entry))
             (seat (cdr entry))
             (record (gethash key records-by-key))
             (room (gethash (plist-get seat :room)
                            (lispm-ui-scene--alloc-room-index alloc))))
        (unless (and record
                     (not (equal (plist-get room :root)
                                 (lispm-ui-scene-normalize-root
                                  (plist-get record :root)))))
          (puthash key seat (lispm-ui-scene--alloc-seats alloc))
          (aset (gethash (plist-get seat :room)
                         (lispm-ui-scene--alloc-occupied alloc))
                (plist-get seat :slot) t)
          (push entry (lispm-ui-scene--alloc-new-seats alloc)))))))

(defun lispm-ui-scene--allocate-seat (key records-by-key alloc)
  "Reserve a stable seat for KEY in ALLOC using RECORDS-BY-KEY."
  (let* ((record (gethash key records-by-key))
         (root (lispm-ui-scene-normalize-root (plist-get record :root)))
         (parent-seat (gethash (plist-get record :parent)
                               (lispm-ui-scene--alloc-seats alloc)))
         (parent-room (and parent-seat
                           (gethash (plist-get parent-seat :room)
                                    (lispm-ui-scene--alloc-room-index alloc))))
         (occupied (lispm-ui-scene--alloc-occupied alloc))
         (root-index (lispm-ui-scene--alloc-root-index alloc))
         (room nil)
         (slot nil))
    (when (and parent-room (equal root (plist-get parent-room :root)))
      (setq slot (or (lispm-ui-scene--free-slot
                      occupied parent-room (/ (plist-get parent-seat :slot) 4))
                     (lispm-ui-scene--free-slot occupied parent-room)))
      (when slot (setq room parent-room)))
    (unless room
      (dolist (candidate (gethash root root-index))
        (unless room
          (when-let* ((free (lispm-ui-scene--free-slot occupied candidate)))
            (setq room candidate slot free)))))
    (unless room
      (setq room (lispm-ui-scene--make-room
                  root (lispm-ui-scene--alloc-next-room alloc)
                  (length (gethash root root-index)))
            slot 0)
      (cl-incf (lispm-ui-scene--alloc-next-room alloc))
      (puthash (plist-get room :id) room (lispm-ui-scene--alloc-room-index alloc))
      (puthash (plist-get room :id) (make-vector 16 nil) occupied)
      (puthash root (append (gethash root root-index) (list room)) root-index)
      (setf (lispm-ui-scene--alloc-rooms alloc)
            (append (lispm-ui-scene--alloc-rooms alloc) (list room))))
    (let ((seat (list :room (plist-get room :id) :slot slot)))
      (puthash key seat (lispm-ui-scene--alloc-seats alloc))
      (aset (gethash (plist-get room :id) occupied) slot t)
      (push (cons (copy-tree key) seat) (lispm-ui-scene--alloc-new-seats alloc)))))

(defun lispm-ui-scene-reconcile (state records)
  "Return STATE with stable seats allocated for RECORDS without mutating inputs.
Absent records keep reservations; relocation moves only that record;
new children prefer their parent's pod, then room and project."
  (let* ((copy (copy-tree (lispm-ui-scene--validate
                           (or state '(:version 1 :rooms nil :seats nil)))))
         (records-by-key (lispm-ui-scene--records records))
         (alloc (lispm-ui-scene--seed-alloc copy records-by-key))
         (seats (lispm-ui-scene--alloc-seats alloc))
         (visiting (make-hash-table :test #'equal))
         (ordered nil))
    (maphash (lambda (key _record) (push key ordered)) records-by-key)
    (setq ordered (sort ordered (lambda (a b)
                                  (string< (prin1-to-string a) (prin1-to-string b)))))
    (dolist (key ordered)
      (unless (gethash key seats)
        (let ((current key)
              (stack nil))
          (while (and current
                      (not (gethash current seats))
                      (not (gethash current visiting)))
            (puthash current t visiting)
            (push current stack)
            (setq current (lispm-ui-scene--same-root-parent
                           (gethash current records-by-key) records-by-key)))
          (dolist (pending stack)
            (lispm-ui-scene--allocate-seat pending records-by-key alloc)
            (remhash pending visiting)))))
    (list :version 1
          :rooms (lispm-ui-scene--alloc-rooms alloc)
          :seats (nreverse (lispm-ui-scene--alloc-new-seats alloc)))))

(defun lispm-ui-scene-compact (_state records)
  "Repack RECORDS from scratch, discarding prior empty reservations."
  (lispm-ui-scene-reconcile nil records))

;;;; Furnishing & responsive room packing

(defun lispm-ui-scene--decorate-pods (nodes)
  "Group copied NODES into sorted pod bounding boxes."
  (let ((groups (make-hash-table :test #'eql))
        (pods nil))
    (dolist (node nodes)
      (push node (gethash (or (plist-get node :display-pod)
                              (/ (or (plist-get node :slot) 0) 4))
                          groups)))
    (maphash
     (lambda (id members)
       (let ((left (apply #'min (mapcar (lambda (n) (plist-get n :x)) members)))
             (top (apply #'min (mapcar (lambda (n) (plist-get n :y)) members)))
             (right (apply #'max (mapcar (lambda (n)
                                           (+ (plist-get n :x) (plist-get n :width)))
                                         members)))
             (bottom (apply #'max (mapcar (lambda (n)
                                            (+ (plist-get n :y) (plist-get n :height)))
                                          members))))
         (push (list :id id
                     :x (- left 4) :y (- top 4)
                     :width (+ 8 (- right left)) :height (+ 8 (- bottom top))
                     :keys (mapcar (lambda (n) (plist-get n :key)) members))
               pods)))
     groups)
    (sort pods (lambda (a b) (< (plist-get a :id) (plist-get b :id))))))

(defun lispm-ui-scene--decorate-fixtures (room nodes floor-y)
  "Append non-overlapping corner plant fixtures to ROOM clear of NODES."
  (let* ((x (plist-get room :x))
         (y (plist-get room :y))
         (width (plist-get room :width))
         (height (plist-get room :height))
         (candidates (list (list :kind 'plant :x (+ x width -42) :y (+ floor-y 4)
                                 :width 18 :height 16)
                           (list :kind 'plant :x (+ x width -42) :y (+ y height -46)
                                 :width 18 :height 22)))
         (fixtures (plist-get room :fixtures)))
    (dolist (plant candidates fixtures)
      (when (and (>= (plist-get plant :x) (+ x lispm-ui-scene-wall-thickness))
                 (>= (plist-get plant :y) floor-y)
                 (not (seq-some (lambda (obstacle) (lispm-ui-scene-overlap-p plant obstacle))
                                (append nodes fixtures (plist-get room :aisles)))))
        (setq fixtures (append fixtures (list plant)))))))

(defun lispm-ui-scene--decorate-room (room &optional service)
  "Annotate copied ROOM with anchors, pods, aisles, and optional SERVICE fixture."
  (let* ((x (plist-get room :x))
         (y (plist-get room :y))
         (width (plist-get room :width))
         (height (plist-get room :height))
         (floor-y (+ y lispm-ui-scene-wall-height))
         (floor-bottom (- (+ y height) lispm-ui-scene-wall-thickness))
         (nodes (plist-get room :nodes))
         (spine (if nodes
                    (max (+ x lispm-ui-scene-wall-thickness 6)
                         (- (apply #'min (mapcar (lambda (n) (plist-get n :x)) nodes)) 20))
                  (+ x 20)))
         (walk-gap (if (plist-get room :responsive) 12 1))
         (coffee (or service
                     (list :kind 'coffee :x (+ x width -112) :y (+ floor-y 4)
                           :width 88 :height 18)))
         (aisles nil))
    (dolist (node nodes)
      (let* ((nx (plist-get node :x))
             (ny (plist-get node :y))
             (walk (cons (+ nx (/ (plist-get node :width) 2.0))
                         (+ ny (plist-get node :height) walk-gap)))
             (line (seq-find (lambda (a) (= (plist-get a :y) (- (cdr walk) 4))) aisles)))
        (plist-put node :desk-anchor (cons nx ny))
        (plist-put node :walk-anchor walk)
        (plist-put node :idle-anchor
                   (cons (+ (plist-get coffee :x) 12
                            (* (or (plist-get node :display-seat)
                                   (% (or (plist-get node :slot) 0) 4))
                               16))
                         (+ (plist-get coffee :y) (plist-get coffee :height) -4)))
        (if line
            (setf (plist-get line :width) (max (plist-get line :width) (- (car walk) spine)))
          (push (list :x spine :y (- (cdr walk) 4)
                      :width (- (car walk) spine)
                      :height (max 1 (min 8 (- floor-bottom (- (cdr walk) 4)))))
                aisles))))
    (setf (plist-get room :pods) (lispm-ui-scene--decorate-pods nodes)
          (plist-get room :circulation-x) spine
          (plist-get room :door-anchor)
          (cons (+ x lispm-ui-scene-wall-thickness) (+ y height -20))
          (plist-get room :aisles)
          (cons (list :x (- spine 6) :y (+ floor-y 4) :width 12
                      :height (max 1 (- floor-bottom floor-y 4)))
                aisles)
          (plist-get room :fixtures) (list coffee))
    (setf (plist-get room :fixtures)
          (lispm-ui-scene--decorate-fixtures room nodes floor-y))
    room))

(defun lispm-ui-scene--pod-chunks (nodes)
  "Sort NODES by saved slot and partition them into display chunks of four."
  (let ((sorted (cl-stable-sort (copy-sequence nodes)
                                (lambda (a b)
                                  (< (or (plist-get a :slot) 0)
                                     (or (plist-get b :slot) 0)))))
        (groups nil))
    (cl-loop for node in sorted
             for index from 0
             do (let* ((pod (/ index 4))
                       (group (assq pod groups)))
                  (plist-put node :display-pod pod)
                  (plist-put node :display-seat (% index 4))
                  (if group
                      (setcdr group (append (cdr group) (list node)))
                    (push (list pod node) groups))))
    (cons sorted (or (nreverse groups) '((0))))))

(defun lispm-ui-scene--best-fit-spec (groups dw dh pixels-wide height)
  "Select optimal pod grid spec for GROUPS in PIXELS-WIDE by HEIGHT pixels."
  (let ((largest (apply #'max 1 (mapcar (lambda (g) (length (cdr g))) groups)))
        (best nil)
        (score -1.0))
    (dotimes (seats (if (= (length groups) 1) largest (min largest 2)))
      (dotimes (columns (length groups))
        (dolist (service '(right bottom))
          (let* ((seat-cols (1+ seats))
                 (pod-cols (1+ columns))
                 (pod-rows (ceiling (length groups) pod-cols))
                 (pw (+ (* seat-cols dw) (* (1- seat-cols) 8)))
                 (ph (+ (* (ceiling largest seat-cols) dh)
                        (* (1- (ceiling largest seat-cols)) 40)))
                 (workw (+ (* pod-cols pw) (* (1- pod-cols) 40)))
                 (workh (+ (* pod-rows ph) (* (1- pod-rows) 40)))
                 (minw (+ 64 workw (if (eq service 'right) 124 0)))
                 (minh (+ lispm-ui-scene-wall-height 56 workh
                          (if (eq service 'bottom) 92 0)))
                 (fit (min (/ pixels-wide minw) (/ (float height) minh)))
                 (quality (+ (min 1.5 fit)
                             (if (= seat-cols 2) 0.006 0)
                             (if (eq service 'right) 0.003 0))))
            (when (and (<= (* minw 0.72) (+ pixels-wide 0.001))
                       (> quality score))
              (setq score quality
                    best (list seat-cols pod-cols pw ph workw workh
                               minw minh service fit)))))))
    best))

(defun lispm-ui-scene-fit-room (room width height)
  "Pack a copy of ROOM into WIDTH by HEIGHT pixels without stretching desks.
Returns (:room ROOM :scale SCALE) with scale clamped to [0.72, 1.5]."
  (dolist (dimension (list width height))
    (unless (and (numberp dimension) (> dimension 0) (< dimension 1.0e6))
      (signal 'wrong-type-argument (list 'positive-finite-number dimension))))
  (let* ((room (copy-tree room))
         (chunked (lispm-ui-scene--pod-chunks (plist-get room :nodes)))
         (nodes (car chunked))
         (groups (cdr chunked))
         (dw (apply #'max 90 (mapcar (lambda (n) (plist-get n :width)) nodes)))
         (dh (apply #'max 80 (mapcar (lambda (n) (plist-get n :height)) nodes)))
         (pixels-wide (max (float width) (* 0.72 (+ dw 64))))
         (best (lispm-ui-scene--best-fit-spec groups dw dh pixels-wide height)))
    (pcase-let* ((`(,seat-cols ,pod-cols ,pw ,ph ,workw ,workh
                    ,minw ,minh ,service ,fit) best)
                 (scale (max 0.72 (min 1.5 fit)))
                 (worldw (max minw (/ pixels-wide scale)))
                 (worldh (max minh (/ (float height) scale)))
                 (x (plist-get room :x))
                 (y (plist-get room :y))
                 (left (+ x 40 (/ (- worldw 64 workw
                                      (if (eq service 'right) 124 0))
                                   2)))
                 (top (+ y lispm-ui-scene-wall-height 24
                         (/ (- worldh lispm-ui-scene-wall-height 56 workh
                               (if (eq service 'bottom) 92 0))
                            2)))
                 (coffee (if (eq service 'right)
                             (list :kind 'coffee :x (+ x worldw -124)
                                   :y (+ y lispm-ui-scene-wall-height 24)
                                   :width 100
                                   :height (min 100 (- worldh lispm-ui-scene-wall-height 56)))
                           (list :kind 'coffee :x (+ x 40) :y (+ y worldh -100)
                                 :width (min 120 (- worldw 64)) :height 68))))
      (cl-loop for group in groups for index from 0 do
               (cl-loop for node in (cdr group) for seat from 0 do
                        (setf (plist-get node :x)
                              (+ left (* (% index pod-cols) (+ pw 40))
                                 (* (% seat seat-cols) (+ dw 8)))
                              (plist-get node :y)
                              (+ top (* (/ index pod-cols) (+ ph 40))
                                 (* (/ seat seat-cols) (+ dh 40))))))
      (setf (plist-get room :width) worldw
            (plist-get room :height) worldh
            (plist-get room :nodes) nodes
            (plist-get room :responsive) t)
      (list :room (lispm-ui-scene--decorate-room room coffee) :scale scale))))

;;;; Aisle pathfinding

(defun lispm-ui-scene--visit-points (room peer)
  "Return safe standing feetpoints beside PEER in ROOM, nearest first."
  (let* ((nodes (plist-get room :nodes))
         (obstacles (append nodes (plist-get room :fixtures)))
         (px (plist-get peer :x))
         (py (plist-get peer :y))
         (right (+ px (plist-get peer :width)))
         (bottom (+ py (plist-get peer :height)))
         (walk (plist-get peer :walk-anchor))
         (left-edge (+ (plist-get room :x) lispm-ui-scene-wall-thickness))
         (top-edge (+ (plist-get room :y) lispm-ui-scene-wall-height))
         (right-edge (- (+ (plist-get room :x) (plist-get room :width))
                        lispm-ui-scene-wall-thickness))
         (bottom-edge (- (+ (plist-get room :y) (plist-get room :height))
                         lispm-ui-scene-wall-thickness))
         (xs (list (- px 41) (+ right 22)))
         (points nil))
    (dolist (node nodes)
      (push (- (plist-get node :x) 41) xs)
      (push (+ (plist-get node :x) (plist-get node :width) 22) xs))
    (dolist (x (delete-dups xs))
      (dolist (y (list (cdr walk) (+ py 62) (+ bottom 66)))
        (let ((body (list :x (- x 18) :y (- y 62) :width 55 :height 65)))
          (when (and (or (<= (+ x 37) (- px 4)) (>= (- x 18) (+ right 4)))
                     (>= (- x 18) left-edge)
                     (<= (+ x 37) right-edge)
                     (>= (- y 62) top-edge)
                     (<= (+ y 3) bottom-edge)
                     (not (seq-some (lambda (obs) (lispm-ui-scene-overlap-p body obs))
                                    obstacles)))
            (push (cons x y) points)))))
    (sort points
          (lambda (a b)
            (< (+ (expt (- (car a) (car walk)) 2) (expt (- (cdr a) (cdr walk)) 2))
               (+ (expt (- (car b) (car walk)) 2) (expt (- (cdr b) (cdr walk)) 2)))))))

(defun lispm-ui-scene--clear-walk-p (path nodes)
  "Return non-nil when orthogonal PATH avoids desk rectangles in NODES."
  (not
   (cl-loop for (p q) on path while q thereis
            (seq-some
             (lambda (node)
               (let ((left (plist-get node :x))
                     (top (plist-get node :y))
                     (right (+ (plist-get node :x) (plist-get node :width)))
                     (bottom (+ (plist-get node :y) (plist-get node :height))))
                 (if (= (car p) (car q))
                     (and (< left (car p) right)
                          (< (min (cdr p) (cdr q)) bottom)
                          (> (max (cdr p) (cdr q)) top))
                   (and (< top (cdr p) bottom)
                        (< (min (car p) (car q)) right)
                        (> (max (car p) (car q)) left)))))
             nodes))))

(defun lispm-ui-scene-walk-path (plan from-key to-key)
  "Return orthogonal aisle waypoints between FROM-KEY and TO-KEY in PLAN.
Returns nil if endpoints are missing, identical, or lack a clear path."
  (let* ((nodes (plist-get plan :nodes))
         (from (seq-find (lambda (n) (equal from-key (plist-get n :key))) nodes))
         (to (seq-find (lambda (n) (equal to-key (plist-get n :key))) nodes))
         (rooms (plist-get plan :rooms))
         (a (seq-find (lambda (r) (eql (plist-get from :room) (plist-get r :id))) rooms))
         (b (seq-find (lambda (r) (eql (plist-get to :room) (plist-get r :id))) rooms))
         (start (plist-get from :walk-anchor))
         (approach (plist-get to :walk-anchor)))
    (when (and from to a b start approach (not (equal from-key to-key)))
      (let* ((ax (plist-get a :circulation-x))
             (bx (plist-get b :circulation-x))
             (ad (plist-get a :door-anchor))
             (bd (plist-get b :door-anchor))
             (outside (+ 12 (apply #'max
                                   (mapcar (lambda (r)
                                             (+ (plist-get r :y) (plist-get r :height)))
                                           rooms))))
             (base (if (eq a b)
                       (list start (cons ax (cdr start)) (cons ax (cdr approach)))
                     (list start (cons ax (cdr start)) (cons ax (cdr ad)) ad
                           (cons (- (plist-get a :x) 12) (cdr ad))
                           (cons (- (plist-get a :x) 12) outside)
                           (cons (- (plist-get b :x) 12) outside)
                           (cons (- (plist-get b :x) 12) (cdr bd)) bd
                           (cons bx (cdr bd)) (cons bx (cdr approach))))))
        (seq-some
         (lambda (end)
           (let ((clean nil))
             (dolist (point (append base (list (cons (car end) (cdr approach)) end)))
               (unless (equal point (car clean))
                 (push point clean)))
             (setq clean (nreverse clean))
             (when (lispm-ui-scene--clear-walk-p clean nodes) clean)))
         (lispm-ui-scene--visit-points b to))))))

;;;; Plan assembly & navigation

(defun lispm-ui-scene--place-grid-nodes (state records-by-key)
  "Build room index, grouped node lists, and flat node list from STATE."
  (let ((room-index (make-hash-table :test #'eql))
        (nodes-by-room (make-hash-table :test #'eql))
        (nodes nil))
    (dolist (room (plist-get state :rooms))
      (puthash (plist-get room :id) room room-index))
    (dolist (entry (plist-get state :seats))
      (when-let* ((record (gethash (car entry) records-by-key)))
        (let* ((seat (cdr entry))
               (slot (plist-get seat :slot))
               (room (gethash (plist-get seat :room) room-index))
               (pod (/ slot 4))
               (local (% slot 4))
               (node (copy-tree record)))
          (plist-put node :room (plist-get room :id))
          (plist-put node :slot slot)
          (plist-put node :x (+ (plist-get room :x) 24 (* (% pod 2) 204) (* (% local 2) 94)))
          (plist-put node :y (+ (plist-get room :y) lispm-ui-scene-wall-height 24
                                (* (/ pod 2) 164) (* (/ local 2) 82)))
          (plist-put node :width 90)
          (plist-put node :height 80)
          (push node nodes)
          (push node (gethash (plist-get room :id) nodes-by-room)))))
    (cons (nreverse nodes) nodes-by-room)))

(defun lispm-ui-scene-plan (state records)
  "Return immutable scene plan for STATE and RECORDS.
Returns (:rooms :nodes :width :height); empty rooms stay reserved in STATE."
  (lispm-ui-scene--validate state)
  (let* ((records-by-key (lispm-ui-scene--records records))
         (placed (lispm-ui-scene--place-grid-nodes state records-by-key))
         (nodes (car placed))
         (nodes-by-room (cdr placed))
         (display-nodes (make-hash-table :test #'equal))
         (visible nil)
         (width 32)
         (height 32))
    (dolist (room (plist-get state :rooms))
      (when-let* ((members (gethash (plist-get room :id) nodes-by-room)))
        (let ((copy (lispm-ui-scene--decorate-room
                     (plist-put (copy-tree room) :nodes (nreverse members)))))
          (when (<= (length members) 8)
            (let ((fit (lispm-ui-scene-fit-room
                        copy (* 1.5 (plist-get room :width))
                        (* 1.5 (plist-get room :height)))))
              (when (= (plist-get fit :scale) 1.5)
                (setq copy (plist-get fit :room)))))
          (dolist (node (plist-get copy :nodes))
            (puthash (plist-get node :key) node display-nodes))
          (push copy visible)
          (setq width (max width (+ (plist-get room :x) (plist-get room :width) 16))
                height (max height (+ (plist-get room :y) (plist-get room :height) 16))))))
    (list :rooms (nreverse visible)
          :nodes (mapcar (lambda (node)
                           (gethash (plist-get node :key) display-nodes))
                         nodes)
          :width width
          :height height)))

(defun lispm-ui-scene-nearest-node (nodes current dx dy)
  "Return the node in NODES nearest CURRENT in unit direction (DX, DY)."
  (when current
    (let ((best nil)
          (best-distance nil))
      (dolist (node nodes best)
        (let* ((ox (- (plist-get node :x) (plist-get current :x)))
               (oy (- (plist-get node :y) (plist-get current :y)))
               (forward (+ (* dx ox) (* dy oy)))
               (side (abs (+ (* dy ox) (* dx oy)))))
          (when (> forward 0)
            (let ((distance (+ forward (* 3 side))))
              (when (or (null best-distance) (< distance best-distance))
                (setq best node best-distance distance)))))))))

;;;; Validated atomic persistence

(defun lispm-ui-scene-read (file)
  "Read validated seating data from FILE, or return nil if FILE does not exist.
Signals `lispm-ui-scene-error' on malformed or circular data without
evaluating any forms."
  (when (file-exists-p file)
    (condition-case err
        (progn
          (when (> (file-attribute-size (file-attributes file))
                   lispm-ui-scene--file-limit)
            (lispm-ui-scene--fail "Scene file exceeds the size limit"))
          (with-temp-buffer
            (insert-file-contents file)
            (let* ((read-circle nil)
                   (state (read (current-buffer))))
              (skip-chars-forward " \t\r\n")
              (while (eq (char-after) ?\;)
                (forward-line 1)
                (skip-chars-forward " \t\r\n"))
              (unless (eobp)
                (lispm-ui-scene--fail "Trailing data in scene file"))
              (lispm-ui-scene--validate state))))
      (error
       (lispm-ui-scene--fail "Cannot read scene from %s: %s"
                             file (error-message-string err))))))

(defun lispm-ui-scene-write (state file)
  "Atomically write validated STATE to FILE, preserving corrupt existing files."
  (lispm-ui-scene--validate state)
  (lispm-ui-scene-read file)
  (let* ((file (expand-file-name file))
         (directory (file-name-directory file))
         (temporary nil))
    (make-directory directory t)
    (unwind-protect
        (progn
          (setq temporary (make-temp-file
                           (expand-file-name ".office-seats-" directory)))
          (let ((coding-system-for-write 'utf-8-unix)
                (print-length nil)
                (print-level nil)
                (print-circle nil))
            (with-temp-file temporary
              (insert ";; LispM Office seats.  Data only; versioned and validated.\n")
              (prin1 state (current-buffer))
              (insert "\n")
              (when (> (string-bytes (buffer-string)) lispm-ui-scene--file-limit)
                (lispm-ui-scene--fail "Scene data exceeds the size limit"))))
          (rename-file temporary file t)
          (setq temporary nil))
      (when (and temporary (file-exists-p temporary))
        (delete-file temporary))))
  state)

(provide 'lispm-ui-scene-layout)
;;; lispm-ui-scene-layout.el ends here
