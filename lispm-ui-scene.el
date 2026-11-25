;;; lispm-ui-scene.el --- Immutable SVG scene composition, hit-testing, and text view -*- lexical-binding: t; -*-

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

;; High-level scene composition returning immutable `lispm-ui-scene-frame'
;; values that carry their own SVG DOM, projection, and hit-test regions.
;; Also provides canonical status presentation metadata and an accessible
;; text renderer sharing the exact same records and identities.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'dom)
(require 'svg)
(require 'xml)
(require 'lispm-ui-draw)
(require 'lispm-ui-palette)
(require 'lispm-ui-scene-layout)
(require 'lispm-ui-scene-sprites)

;;;; Constants & immutable frame value

(defconst lispm-ui-scene-max-pixels 4096
  "Largest width or height of a rendered scene image in pixels.")

(defvar lispm-ui-scene--default-visits (make-hash-table :test #'equal)
  "Fallback collaboration visit start times when callers omit a table.")

(cl-defstruct (lispm-ui-scene-frame
               (:constructor lispm-ui-scene-frame--create)
               (:copier nil))
  "Immutable rendered scene with coupled projection and hit-test geometry."
  svg plan projection level room-id scale actor-hits docket-hits)

;;;; Canonical status presentation

(defun lispm-ui-scene-node-state (node)
  "Return NODE's state symbol, preserving unknown data as `unknown'."
  (or (plist-get (plist-get node :status) :state) 'unknown))

;;; Basis: Canonical visual + textual state presentation shared by scene and sheet.
(defun lispm-ui-scene-state-info (status)
  "Return canonical presentation plist for STATUS.
Properties are :state, :label, :chip-label, :badge, :dot, and
:accent-key or :accent-color."
  (let* ((state (or (plist-get status :state) 'unknown))
         (kind (plist-get status :kind)))
    (pcase state
      ('working '(:state working :label "Working" :chip-label "Working"
                  :badge nil :dot "#389b6c" :accent-key :focus))
      ('idle '(:state idle :label "Idle" :chip-label "Idle"
               :badge nil :dot "#8e8375" :accent-key :muted))
      ('done '(:state done :label "Complete" :chip-label "Complete"
               :badge "OK" :dot "#4b7da8" :accent-color "#4b8663"))
      ('error '(:state error :label "Error" :chip-label "Error"
                :badge "ERR" :dot "#c94a4a" :accent-key :alert-dark))
      ('blocked
       (pcase kind
         ('permission '(:state blocked :label "Approval needed"
                        :chip-label "Waiting for approval" :badge "!"
                        :dot "#d98b26" :accent-color "#b88432"))
         ('question '(:state blocked :label "Needs answer"
                      :chip-label "Needs an answer" :badge "?"
                      :dot "#d98b26" :accent-color "#b88432"))
         ('auth '(:state blocked :label "Sign-in needed"
                  :chip-label "Needs authentication" :badge "AUTH"
                  :dot "#d98b26" :accent-color "#b88432"))
         (_ '(:state blocked :label "Paused"
              :chip-label "Waiting" :badge "WAIT"
              :dot "#d98b26" :accent-color "#b88432"))))
      (_ '(:state unknown :label "Active" :chip-label "Status unavailable"
           :badge nil :dot "#8e8375" :accent-key :muted)))))

(defun lispm-ui-scene-state-label (status &optional chip-p)
  "Return concise human label for STATUS; CHIP-P selects detailed sheet phrasing."
  (plist-get (lispm-ui-scene-state-info status)
             (if chip-p :chip-label :label)))

(defun lispm-ui-scene-state-badge (node)
  "Return short text-mode badge string for NODE, or nil."
  (plist-get (lispm-ui-scene-state-info (plist-get node :status)) :badge))

(defun lispm-ui-scene-state-dot-color (state)
  "Return status indicator dot color for STATE symbol."
  (plist-get (lispm-ui-scene-state-info (list :state state)) :dot))

;;;; Projections & room lookup

(defun lispm-ui-scene--find-room (plan room-id)
  "Return room with ROOM-ID in PLAN, or nil for the whole overview."
  (and room-id
       (seq-find (lambda (r) (equal room-id (plist-get r :id)))
                 (plist-get plan :rooms))))

(defun lispm-ui-scene-level (scale)
  "Return semantic level (`desk', `floor', or `map') for SCALE."
  (cond ((>= scale 0.72) 'desk)
        ((>= scale 0.45) 'floor)
        (t 'map)))

(defun lispm-ui-scene-projection (plan scale &optional room-id)
  "Return viewport projection plist for PLAN at SCALE and optional ROOM-ID."
  (unless (and (numberp scale) (> scale 0) (< scale 1.0e6))
    (signal 'wrong-type-argument (list 'positive-finite-number scale)))
  (let* ((room (lispm-ui-scene--find-room plan room-id))
         (x (if room (plist-get room :x) 0))
         (y (if room (plist-get room :y) 0))
         (width (max 1 (or (plist-get (or room plan) :width)
                           lispm-ui-scene-room-width)))
         (height (max 1 (or (plist-get (or room plan) :height)
                            lispm-ui-scene-room-height)))
         (actual (min (float scale)
                      (/ (float lispm-ui-scene-max-pixels) width)
                      (/ (float lispm-ui-scene-max-pixels) height))))
    (list :x x :y y :width width :height height :scale actual
          :pixel-width (* width actual)
          :pixel-height (* height actual))))

(defun lispm-ui-scene-fit-plan (plan room-id width height)
  "Return responsive copy of PLAN focused on ROOM-ID in WIDTH by HEIGHT pixels."
  (when-let* ((room (lispm-ui-scene--find-room plan room-id)))
    (let* ((packed (lispm-ui-scene-fit-room room width height))
           (display-room (plist-get packed :room))
           (display (copy-tree plan))
           (by-key (make-hash-table :test #'equal)))
      (dolist (node (plist-get display-room :nodes))
        (puthash (plist-get node :key) node by-key))
      (setf (plist-get display :rooms)
            (mapcar (lambda (item)
                      (if (equal (plist-get item :id) room-id)
                          display-room
                        item))
                    (plist-get display :rooms))
            (plist-get display :nodes)
            (mapcar (lambda (node)
                      (or (gethash (plist-get node :key) by-key) node))
                    (plist-get display :nodes))
            (plist-get display :width)
            (max (or (plist-get display :width) 0)
                 (+ (plist-get room :x) (plist-get display-room :width) 16))
            (plist-get display :height)
            (max (or (plist-get display :height) 0)
                 (+ (plist-get room :y) (plist-get display-room :height) 16)))
      (list :plan display :scale (plist-get packed :scale)))))

;;;; Endpoints, labels, events, and visitors

(defun lispm-ui-scene-endpoint (endpoint nodes)
  "Locate ENDPOINT by stable key or unambiguous name inside NODES."
  (when endpoint
    (if (consp endpoint)
        (seq-find (lambda (node) (equal endpoint (plist-get node :key))) nodes)
      (let ((matches (seq-filter
                      (lambda (node)
                        (equal (format "%s" endpoint) (plist-get node :name)))
                      nodes)))
        (and (= (length matches) 1) (car matches))))))

(defun lispm-ui-scene-visible-events (plan scale room-id events)
  "Return EVENTS whose endpoints are both visible in PLAN at SCALE and ROOM-ID."
  (let* ((projection (lispm-ui-scene-projection plan scale room-id))
         (room (lispm-ui-scene--find-room plan room-id))
         (nodes (if room (plist-get room :nodes) (plist-get plan :nodes))))
    (unless (eq (lispm-ui-scene-level (plist-get projection :scale)) 'map)
      (seq-filter
       (lambda (event)
         (and (memq (plist-get event :type)
                    '(message-queued message-taken message-delivered))
              (lispm-ui-scene-endpoint (plist-get event :from) nodes)
              (lispm-ui-scene-endpoint (plist-get event :to) nodes)))
       events))))

(defun lispm-ui-scene-desk-label (node &optional lead-p)
  "Return compact desk label for NODE, keeping suffixes; LEAD-P marks coordinator."
  (let ((name (or (plist-get node :name) "Agent"))
        (sub-p (plist-get node :parent)))
    (cond
     ((string-match "\\`\\([^@]+\\)@.*\\(<[0-9]+>\\)\\'" name)
      (let ((role (match-string 1 name))
            (suffix (match-string 2 name))
            (prefix (if sub-p "↳ " "")))
        (concat prefix
                (truncate-string-to-width
                 role (max 1 (- 13 (string-width prefix) (string-width suffix)))
                 nil nil "…")
                " " suffix)))
     ((string-match "\\`\\([^@]+\\)@[^<]+\\'" name)
      (let ((role (match-string 1 name)))
        (cond (lead-p (concat "★ " role))
              (sub-p (concat "↳ " role))
              ((> (string-width name) 14) role)
              (t name))))
     (lead-p (concat "★ " name))
     (sub-p (concat "↳ " name))
     (t name))))

(defun lispm-ui-scene--path-position (path progress)
  "Interpolate rectilinear point along PATH at normalized PROGRESS."
  (let* ((lengths (cl-loop for (a b) on path while b
                           collect (+ (abs (- (car b) (car a)))
                                      (abs (- (cdr b) (cdr a))))))
         (remaining (* progress (apply #'+ lengths)))
         (points path)
         (sizes lengths))
    (while (and (cdr sizes) (> remaining (car sizes)))
      (setq remaining (- remaining (car sizes))
            points (cdr points)
            sizes (cdr sizes)))
    (if (or (null (cdr points)) (zerop (or (car sizes) 0)))
        (car (last path))
      (let* ((a (car points))
             (b (cadr points))
             (part (/ remaining (float (car sizes)))))
        (cons (+ (car a) (* part (- (car b) (car a))))
              (+ (cdr a) (* part (- (cdr b) (cdr a)))))))))

(defun lispm-ui-scene-visitors (plan nodes &optional visits now)
  "Compute animated collaborator poses in PLAN for NODES at timestamp NOW.
VISITS maps active collaborator pairs to walk start times; omitted keys
use an internal session table."
  (let ((table (or visits lispm-ui-scene--default-visits))
        (active (make-hash-table :test #'equal))
        (clock (or now (float-time)))
        (visitors nil))
    (dolist (node nodes)
      (when-let* ((target (seq-some (lambda (k) (lispm-ui-scene-endpoint k nodes))
                                    (plist-get node :waits-on)))
                  (path (lispm-ui-scene-walk-path
                         plan (plist-get node :key) (plist-get target :key))))
        (let* ((key (list (plist-get node :key) (plist-get target :key)))
               (start (or (gethash key table) clock))
               (progress (min 1.0 (max 0.0 (/ (- clock start) 1.4))))
               (position (lispm-ui-scene--path-position path progress)))
          (puthash key t active)
          (puthash key start table)
          (push (list :key (plist-get node :key) :node node :feet position
                      :pose (if (< progress 1) 'walking 'talking))
                visitors))))
    (maphash (lambda (key _v)
               (unless (gethash key active) (remhash key table)))
             table)
    visitors))

;;;; Desks, links, and master-detail docket

(defun lispm-ui-scene--draw-desk
    (svg node selected level phase _scale &optional away lead-p)
  "Draw NODE's desk on SVG with SELECTION, LEVEL, PHASE, AWAY, and LEAD-P."
  (let* ((group (lispm-ui-scene-svg-group svg "office-desk"))
         (x (plist-get node :x))
         (y (plist-get node :y))
         (width (plist-get node :width))
         (height (plist-get node :height))
         (state (lispm-ui-scene-node-state node))
         (selected-p (equal selected (plist-get node :key)))
         (sub-p (and (plist-get node :parent) t))
         (ink "#3d3630")
         (dim "#7a6e60")
         (paper "#f6ecdc")
         (accent (lispm-ui-scene-palette-color :accent))
         (focus (lispm-ui-scene-palette-color :focus))
         (dot-color (lispm-ui-scene-state-dot-color state))
         (queued (plist-get (plist-get node :mailbox) :queued)))
    (dom-set-attribute group 'data-state (symbol-name state))
    (when (or lead-p sub-p)
      (dom-set-attribute group 'data-role (if lead-p "main" "sub")))
    (when selected-p
      (svg-rectangle group (+ x 2) (+ y 2) (- width 4) (- height 2)
                     :fill (lispm-ui-blend paper focus 0.14)
                     :stroke focus :stroke-width 1.6)
      (lispm-ui-scene-svg-rects
       group `((,(+ x 4) ,(+ y 4) 8 2 ,focus) (,(+ x 4) ,(+ y 4) 2 8 ,focus)
               (,(+ x width -12) ,(+ y 4) 8 2 ,focus)
               (,(+ x width -6) ,(+ y 4) 2 8 ,focus))))
    (let ((worker (lispm-ui-scene-svg-group group "office-worker")))
      (dom-set-attribute worker 'transform (format "translate(%s %s)" x y))
      (lispm-ui-scene-draw-worker worker node level phase away))
    (when lead-p
      (svg-rectangle group (+ x 12) (+ y 53) 66 2 :fill accent))
    (when (and (eq level 'desk) (or lead-p sub-p))
      (svg-rectangle group (+ x 5) (+ y 4) (if lead-p 27 23) 11
                     :fill (if lead-p accent paper)
                     :stroke (if lead-p accent dim) :stroke-width 0.9)
      (lispm-ui-scene-svg-text group (if lead-p "MAIN" "SUB")
                               (+ x (if lead-p 18.5 16.5)) (+ y 12) 7
                               (if lead-p "#ffffff" dim) "middle" 4 "bold"))
    (when (and (numberp queued) (> queued 0))
      (svg-rectangle group (+ x 68) (+ y 33) 18 12
                     :fill "#fff8e8" :stroke dim :stroke-width 0.9)
      (svg-rectangle group (+ x 69) (+ y 34) 16 2 :fill accent)
      (lispm-ui-scene-svg-text group (format "%s" queued)
                               (+ x 77) (+ y 43) 8 ink "middle" 3 "bold"))
    (when (eq level 'desk)
      (svg-rectangle group (+ x 6) (+ y height -15) (- width 12) 14
                     :fill (if selected-p "#fffaf0" paper)
                     :stroke (if selected-p focus "#b5a692")
                     :stroke-width (if selected-p 1.2 0.8))
      (svg-rectangle group (+ x 10) (+ y height -10) 4 4 :fill dot-color)
      (lispm-ui-scene-svg-text
       group (lispm-ui-scene-desk-label node lead-p)
       (+ x (/ width 2) 3) (+ y height -5) 9.5
       (if lead-p accent ink) "middle" 14
       (if (or selected-p lead-p) "bold" "normal")))))

(defun lispm-ui-scene--draw-relations (svg nodes events)
  "Draw parent delegation lines and flying message EVENTS between NODES on SVG."
  (let ((accent (lispm-ui-scene-palette-color :accent)))
    (dolist (node nodes)
      (when-let* ((parent-key (plist-get node :parent))
                  (parent (lispm-ui-scene-endpoint parent-key nodes))
                  ((eql (plist-get node :room) (plist-get parent :room))))
        (let ((px (+ (plist-get parent :x) 45))
              (py (+ (plist-get parent :y) 56))
              (cx (+ (plist-get node :x) 45))
              (cy (+ (plist-get node :y) 56))
              (link (lispm-ui-scene-svg-group svg "office-delegation")))
          (svg-polyline link (list (cons px py) (cons px cy) (cons cx cy))
                        :fill "none" :stroke accent :stroke-width 1.2
                        :stroke-dasharray "3,3" :opacity 0.45))))
    (dolist (event events)
      (when (memq (plist-get event :type)
                  '(message-queued message-taken message-delivered))
        (let ((from (lispm-ui-scene-endpoint (plist-get event :from) nodes))
              (to (lispm-ui-scene-endpoint (plist-get event :to) nodes))
              (progress (plist-get event :progress)))
          (when (and from to)
            (let* ((p (if (numberp progress) (max 0 (min 1 progress)) 0.5))
                   (fx (+ 45 (plist-get from :x)))
                   (fy (+ 28 (plist-get from :y)))
                   (tx (+ 45 (plist-get to :x)))
                   (ty (+ 28 (plist-get to :y)))
                   (x (+ (* (- 1 p) fx) (* p tx)))
                   (y (+ (* (- 1 p) fy) (* p ty)))
                   (tp (max 0.0 (- p 0.12)))
                   (trx (+ (* (- 1 tp) fx) (* tp tx)))
                   (try (+ (* (- 1 tp) fy) (* tp ty)))
                   (group (lispm-ui-scene-svg-group svg "office-envelope")))
              (svg-rectangle group (- trx 2) (- try 2) 4 4
                             :fill accent :opacity 0.35)
              (svg-rectangle group (- x 8) (- y 4) 18 11
                             :fill "#3a332c" :opacity 0.22)
              (svg-rectangle group (- x 9) (- y 6) 18 11
                             :fill "#fff9eb" :stroke accent :stroke-width 1.2)
              (svg-polyline group (list (cons (- x 9) (- y 6))
                                        (cons x y)
                                        (cons (+ x 9) (- y 6)))
                            :fill "none" :stroke accent :stroke-width 1.1)
              (svg-rectangle group (- x 2) (- y 2) 4 3 :fill "#d95b43"))))))))

(defun lispm-ui-scene--docket-bounds (room nodes)
  "Return non-overlapping master-detail docket bounds in ROOM clear of NODES."
  (let* ((rx (plist-get room :x))
         (ry (plist-get room :y))
         (rw (plist-get room :width))
         (rh (plist-get room :height))
         (bottom (- (+ ry rh) lispm-ui-scene-wall-thickness))
         (right (- (+ rx rw) lispm-ui-scene-wall-thickness))
         (max-y (if nodes
                    (apply #'max (mapcar (lambda (n)
                                           (+ (plist-get n :y) (plist-get n :height)))
                                         nodes))
                  (+ ry lispm-ui-scene-wall-height)))
         (max-x (if nodes
                    (apply #'max (mapcar (lambda (n)
                                           (+ (plist-get n :x) (plist-get n :width)))
                                         nodes))
                  rx))
         (avail-y (- bottom max-y))
         (avail-x (- right max-x)))
    (cond
     ((>= avail-y 44)
      (list :x (+ rx 16) :y (- bottom 40) :width (- rw 32) :height 36 :layout 'full))
     ((and (>= avail-x 180)
           (>= (- bottom (+ ry lispm-ui-scene-wall-height 36)) 40))
      (list :x (+ max-x 12) :y (- bottom 40) :width (- avail-x 16)
            :height 36 :layout 'side))
     ((>= avail-y 16)
      (list :x (+ rx 14) :y (- bottom 16) :width (- rw 28)
            :height 14 :layout 'compact)))))

(defun lispm-ui-scene--docket-buttons (node status)
  "Return ordered action button alist for NODE with STATUS."
  (append (when (and (eq (lispm-ui-scene-node-state node) 'blocked)
                     (eq (plist-get status :kind) 'permission))
            '((approve . "Approve")))
          '((open-chat . "Chat") (send . "Message") (inspect . "Details"))))

(defun lispm-ui-scene--draw-docket-actions (group node status bounds dx accent focus)
  "Draw action buttons for NODE and STATUS in GROUP; return (BX . HITS)."
  (let* ((dy (plist-get bounds :y))
         (dw (plist-get bounds :width))
         (dh (plist-get bounds :height))
         (compact (eq (plist-get bounds :layout) 'compact))
         (selected (plist-get node :key))
         (bx (- (+ dx dw) 6))
         (hits nil))
    (dolist (btn (reverse (lispm-ui-scene--docket-buttons node status)))
      (let* ((label (cdr btn))
             (bw (+ 12 (* (length label) 5.6)))
             (bh (if compact 11 18))
             (by (+ dy (/ (- dh bh) 2.0)))
             (primary (eq (car btn) 'approve)))
        (setq bx (- bx bw 5))
        (when (> bx (+ dx (if compact 90 150)))
          (svg-rectangle group (+ bx 5) by bw bh
                         :fill (if primary accent "#fffaf0")
                         :stroke (if primary accent focus) :stroke-width 0.9)
          (lispm-ui-scene-svg-text group label (+ bx 5 (/ bw 2.0))
                                   (+ by (if compact 8.5 12.5))
                                   (if compact 7 8)
                                   (if primary "#ffffff" "#38312b")
                                   "middle" nil "bold")
          (push (list :action (car btn) :key selected
                      :x (+ bx 5) :y by :width bw :height bh)
                hits))))
    (cons bx hits)))

(defun lispm-ui-scene--draw-docket (svg room nodes selected level phase)
  "Draw SELECTED's master-detail docket in ROOM on SVG and return hit rects."
  (when-let* ((_ (eq level 'desk))
              (node (and selected
                         (seq-find (lambda (n) (equal selected (plist-get n :key))) nodes)))
              (bounds (lispm-ui-scene--docket-bounds room nodes)))
    (let* ((group (lispm-ui-scene-svg-group svg "office-docket"))
           (dx (plist-get bounds :x))
           (dy (plist-get bounds :y))
           (dw (plist-get bounds :width))
           (dh (plist-get bounds :height))
           (compact (eq (plist-get bounds :layout) 'compact))
           (status (plist-get node :status))
           (state (lispm-ui-scene-node-state node))
           (dot (lispm-ui-scene-state-dot-color state))
           (accent (lispm-ui-scene-palette-color :accent))
           (focus (lispm-ui-scene-palette-color :focus))
           (raw-id (and (consp selected) (stringp (cadr selected)) (cadr selected)))
           (short-id (and raw-id (string-match "-\\([0-9a-fA-F]\\{4\\}\\)\\'" raw-id)
                          (concat " #" (match-string 1 raw-id))))
           (name (concat (lispm-ui-scene-desk-label node) (or short-id "")))
           (state-text (lispm-ui-scene-state-label status))
           (msg (string-trim
                 (replace-regexp-in-string "[[:space:][:cntrl:]]+" " "
                                           (format "%s" (or (plist-get status :msg) "")))))
           (actions (lispm-ui-scene--draw-docket-actions
                     group node status bounds dx accent focus))
           (bx (car actions))
           (hits (cdr actions)))
      (dom-set-attribute group 'data-key (xml-escape-string (prin1-to-string selected)))
      (svg-rectangle group (+ dx 2) (+ dy 2) dw dh :fill "#3b342c" :opacity 0.22)
      (svg-rectangle group dx dy dw dh :fill "#f6eddc" :stroke focus :stroke-width 1.3)
      (lispm-ui-scene-svg-rects
       group `((,(1+ dx) ,(1+ dy) ,(- dw 2) 1 "#fffaf0") (,dx ,dy 5 ,dh ,dot)))
      (if compact
          (lispm-ui-scene-svg-text
           group (concat name " · " state-text
                         (if (string-empty-p msg) "" (concat " — " msg)))
           (+ dx 10) (+ dy 10) 8 "#38312b" "start"
           (max 12 (floor (/ (- bx dx 16) 5.2))) "bold")
        (svg-rectangle group (+ dx 9) (+ dy 5) 26 26
                       :fill "#e6d9c3" :stroke "#8c7d6b" :stroke-width 0.8)
        (unless (lispm-ui-scene-draw-avatar
                 group (plist-get node :avatar) (+ dx 11) (+ dy 7) 22)
          (lispm-ui-scene-draw-pixel-portrait
           group (lispm-ui-scene-portrait-for-node node) (+ dx 14) (+ dy 10) 1))
        (let* ((tx (+ dx 42))
               (cols (max 14 (floor (/ (- bx tx 8) 5.5))))
               (queued (plist-get (plist-get node :mailbox) :queued))
               (waits (plist-get node :waits-on))
               (role (if (plist-get node :parent) "Subagent · " ""))
               (meta (concat role state-text
                             (if (and (numberp queued) (> queued 0))
                                 (format " · Inbox: %d" queued) "")
                             (if waits (format " · Waiting (%d)" (length waits)) "")
                             (if (and (eq state 'working) (zerop (% phase 2))) " ●" "")))
               (activity (cond ((not (string-empty-p msg)) msg)
                               ((eq state 'done)
                                "Task completed — click desk again, Details, or Chat to view transcript")
                               (t "Ready at desk — click desk again, Details, or press RET to inspect"))))
          (lispm-ui-scene-svg-text group (concat name "  [" meta "]")
                                   tx (+ dy 15) 9 "#352e28" "start" cols "bold")
          (lispm-ui-scene-svg-text group activity
                                   tx (+ dy 28) 8.2 "#63584c" "start" cols)))
      hits)))

;;;; Scene composition & pure hit-testing

(defun lispm-ui-scene--draw-visitors (svg visitors level phase)
  "Draw walking and talking VISITORS on SVG at LEVEL and PHASE; return actor hits."
  (let ((hits nil))
    (dolist (actor visitors (nreverse hits))
      (let* ((feet (plist-get actor :feet))
             (group (lispm-ui-scene-svg-group svg "office-visitor")))
        (dom-set-attribute group 'data-key
                           (xml-escape-string (prin1-to-string (plist-get actor :key))))
        (dom-set-attribute group 'transform
                           (format "translate(%s %s)"
                                   (- (car feet) 45) (- (cdr feet) 64)))
        (svg-rectangle group 31 63 28 3 :fill "#514b43" :opacity 0.18)
        (lispm-ui-scene-draw-person group (plist-get actor :node)
                                    level phase (plist-get actor :pose))
        (push (list :key (plist-get actor :key)
                    :x (- (car feet) 18) :y (- (cdr feet) 62)
                    :width 55 :height 65)
              hits)))))

(cl-defun lispm-ui-scene-compose
    (plan &key selected (scale 1.0) (phase 0) room-id events visits now)
  "Compose an immutable `lispm-ui-scene-frame' for PLAN without side effects."
  (let* ((projection (lispm-ui-scene-projection plan scale room-id))
         (actual (plist-get projection :scale))
         (level (lispm-ui-scene-level actual))
         (room (lispm-ui-scene--find-room plan room-id))
         (rooms (if room (list room) (plist-get plan :rooms)))
         (nodes (if room (plist-get room :nodes) (plist-get plan :nodes)))
         (visitors (unless (eq level 'map)
                     (lispm-ui-scene-visitors plan nodes visits now)))
         (*lispm-ui-scene-palette* (lispm-ui-scene-palette))
         (svg (svg-create (plist-get projection :pixel-width)
                          (plist-get projection :pixel-height)))
         (actor-hits nil)
         (docket-hits nil))
    (dom-set-attribute svg 'viewBox
                       (format "%s %s %s %s"
                               (plist-get projection :x) (plist-get projection :y)
                               (plist-get projection :width) (plist-get projection :height)))
    (dom-set-attribute svg 'preserveAspectRatio "none")
    (dom-set-attribute svg 'data-view (symbol-name level))
    (lispm-ui-svg-fill svg (plist-get projection :x) (plist-get projection :y)
                       (plist-get projection :width) (plist-get projection :height)
                       "#aaa392")
    (dolist (item rooms)
      (lispm-ui-scene-draw-room svg item selected level actual))
    (unless (eq level 'map)
      (let ((parents (make-hash-table :test #'equal)))
        (dolist (node (plist-get plan :nodes))
          (when-let* ((parent (plist-get node :parent)))
            (puthash parent t parents)))
        (dolist (node nodes)
          (lispm-ui-scene--draw-desk
           svg node selected level phase actual
           (seq-find (lambda (actor)
                       (equal (plist-get actor :key) (plist-get node :key)))
                     visitors)
           (and (null (plist-get node :parent))
                (gethash (plist-get node :key) parents)))))
      (lispm-ui-scene--draw-relations svg nodes events)
      (setq actor-hits (lispm-ui-scene--draw-visitors svg visitors level phase))
      (let ((docket-room
             (or room
                 (and selected
                      (when-let* ((sel (seq-find (lambda (n)
                                                   (equal selected (plist-get n :key)))
                                                 nodes)))
                        (lispm-ui-scene--find-room plan (plist-get sel :room)))))))
        (when docket-room
          (setq docket-hits
                (lispm-ui-scene--draw-docket
                 svg docket-room (plist-get docket-room :nodes)
                 selected level phase)))))
    (lispm-ui-scene-frame--create
     :svg svg :plan plan :projection projection :level level
     :room-id room-id :scale scale
     :actor-hits actor-hits :docket-hits docket-hits)))

(defun lispm-ui-scene-render-svg
    (plan selected scale phase &optional room-id events visits now)
  "Draw PLAN as an SVG DOM for SELECTED at SCALE, PHASE, ROOM-ID, and EVENTS."
  (lispm-ui-scene-frame-svg
   (lispm-ui-scene-compose plan :selected selected :scale scale :phase phase
                           :room-id room-id :events events
                           :visits visits :now now)))

(defun lispm-ui-scene--inside-p (x y rect)
  "Return non-nil when world point (X, Y) lies inside RECT."
  (and (>= x (plist-get rect :x))
       (>= y (plist-get rect :y))
       (< x (+ (plist-get rect :x) (plist-get rect :width)))
       (< y (+ (plist-get rect :y) (plist-get rect :height)))))

(defun lispm-ui-scene-hit-test (target x y &optional scale room-id)
  "Identify pixel (X, Y) in TARGET (`lispm-ui-scene-frame' or raw plan).
When TARGET is a raw plan plist, SCALE and optional ROOM-ID specify its
projection."
  (let* ((frame-p (lispm-ui-scene-frame-p target))
         (plan (if frame-p (lispm-ui-scene-frame-plan target) target))
         (room-id (if frame-p (lispm-ui-scene-frame-room-id target) room-id))
         (projection (if frame-p
                         (lispm-ui-scene-frame-projection target)
                       (lispm-ui-scene-projection plan scale room-id)))
         (actual (plist-get projection :scale))
         (room (lispm-ui-scene--find-room plan room-id)))
    (when (and (numberp x) (numberp y) (>= x 0) (>= y 0)
               (< x (plist-get projection :pixel-width))
               (< y (plist-get projection :pixel-height)))
      (let* ((wx (+ (plist-get projection :x) (/ x actual)))
             (wy (+ (plist-get projection :y) (/ y actual)))
             (map-p (eq (lispm-ui-scene-level actual) 'map))
             (actor (and frame-p (not map-p)
                         (seq-find (lambda (item) (lispm-ui-scene--inside-p wx wy item))
                                   (lispm-ui-scene-frame-actor-hits target))))
             (desk (and (not map-p)
                        (seq-find (lambda (item)
                                    (and (or (null room)
                                             (equal (plist-get room :id)
                                                    (plist-get item :room)))
                                         (lispm-ui-scene--inside-p wx wy item)))
                                  (plist-get plan :nodes))))
             (button (and frame-p (not map-p) (not actor) (not desk)
                          (seq-find (lambda (item) (lispm-ui-scene--inside-p wx wy item))
                                    (lispm-ui-scene-frame-docket-hits target))))
             (tile (and map-p
                        (seq-find (lambda (item)
                                    (and (or (null room)
                                             (equal (plist-get room :id)
                                                    (plist-get item :id)))
                                         (lispm-ui-scene--inside-p wx wy item)))
                                  (plist-get plan :rooms)))))
        (cond
         (button (list :action (plist-get button :action)
                       :key (plist-get button :key)))
         ((or actor desk) (list :key (plist-get (or actor desk) :key)))
         (tile (list :room (plist-get tile :id))))))))

;;;; Accessible text representation

(defun lispm-ui-scene-render-text
    (plan selected &optional room-id key-prop room-prop)
  "Insert PLAN as accessible text with SELECTED marked and optional ROOM-ID.
Lines carry KEY-PROP (default `lispm-ui-scene-key') and ROOM-PROP
\(default `lispm-ui-scene-room')."
  (let* ((key-prop (or key-prop 'lispm-ui-scene-key))
         (room-prop (or room-prop 'lispm-ui-scene-room))
         (room (lispm-ui-scene--find-room plan room-id))
         (rooms (if room (list room) (plist-get plan :rooms)))
         (parents (let ((table (make-hash-table :test #'equal)))
                    (dolist (node (plist-get plan :nodes) table)
                      (when-let* ((parent (plist-get node :parent)))
                        (puthash parent t table))))))
    (dolist (item rooms)
      (insert (propertize (format "%s  (%d agents)\n"
                                  (lispm-ui-scene-sanitize-label (plist-get item :label))
                                  (length (plist-get item :nodes)))
                          'face 'bold room-prop (plist-get item :id)))
      (dolist (node (plist-get item :nodes))
        (let* ((key (plist-get node :key))
               (lead-p (and (null (plist-get node :parent)) (gethash key parents)))
               (sub-p (and (plist-get node :parent) t))
               (prefix (cond (lead-p "★ ") (sub-p "↳ ") (t "")))
               (badge (lispm-ui-scene-state-badge node))
               (queued (plist-get (plist-get node :mailbox) :queued))
               (state (lispm-ui-scene-node-state node))
               (status (plist-get node :status))
               (text (format "%s %-18s [%s%s] inbox:%s%s\n"
                             (if (equal key selected) ">" " ")
                             (lispm-ui-scene-sanitize-label
                              (concat prefix (or (plist-get node :name) "")) 18)
                             state
                             (if badge (concat " " badge) "")
                             (if (numberp queued) queued "?")
                             (if (plist-get status :msg)
                                 (concat "  "
                                         (lispm-ui-scene-sanitize-label
                                          (plist-get status :msg) 80))
                               ""))))
          (insert (propertize text key-prop key room-prop (plist-get item :id)
                              'mouse-face 'highlight
                              'help-echo "Inspect this agent"
                              'face (and (equal key selected) 'highlight)))
          (when (plist-get node :waits-on)
            (insert (propertize
                     (format "    Waits on: %s\n"
                             (mapconcat (lambda (target)
                                          (let ((other (lispm-ui-scene-endpoint
                                                        target (plist-get plan :nodes))))
                                            (lispm-ui-scene-sanitize-label
                                             (if other
                                                 (plist-get other :name)
                                               (format "%S" target)))))
                                        (plist-get node :waits-on) ", "))
                     key-prop key room-prop (plist-get item :id))))))
      (insert "\n"))))

(defun lispm-ui-scene--sheet-portrait-draw (svg node x y size)
  "Render NODE's avatar or pixel portrait on SVG at X, Y with diameter SIZE."
  (unless (lispm-ui-scene-draw-avatar svg (plist-get node :avatar) x y size)
    (lispm-ui-scene-draw-pixel-portrait
     svg (lispm-ui-scene-portrait-for-node node) x y (/ size 16.0)))
  t)

(with-eval-after-load 'lispm-ui-sheet
  (unless lispm-ui-sheet-status-info-function
    (setq lispm-ui-sheet-status-info-function #'lispm-ui-scene-state-info))
  (unless lispm-ui-sheet-portrait-draw-function
    (setq lispm-ui-sheet-portrait-draw-function #'lispm-ui-scene--sheet-portrait-draw)))

(provide 'lispm-ui-scene)
;;; lispm-ui-scene.el ends here
