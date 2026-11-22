;;; lispm-ui-spotlight.el --- Reactive multi-actor buffer spotlights  -*- lexical-binding: t; -*-

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

;; Functional reactive buffer spotlighting built on `lispm-ui-frp':
;;
;; - Pure interval algebra (`lispm-ui-spotlight-coalesce-lines',
;;   `lispm-ui-spotlight-compose-segments', `lispm-ui-spotlight-compose-tags')
;;   composes overlapping beams from multiple actors into elementary
;;   disjoint intervals with alpha-weighted colour blending, side-by-side
;;   margin stripes, and coalesced header badges.
;; - `lispm-ui-spotlight-store' holds reactive `beams' and `clock' signals
;;   whose single `lispm-ui-effect' reconciles buffer overlays after each
;;   `lispm-ui-frp-batch' transaction settles.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-ui-base)
(require 'lispm-ui-frp)
(require 'lispm-ui-palette)

;;;; Structured conditions

(define-error 'lispm-ui-spotlight-error
  "LispM UI spotlight error" 'lispm-ui-error)

;;;; Domain structures

(cl-defstruct (lispm-ui-spotlight-actor
               (:constructor lispm-ui-spotlight-actor-create)
               (:copier nil)
               (:predicate lispm-ui-spotlight-actor-p))
  "Identity and visual styling of an actor casting spotlight beams."
  (key "" :type string)
  (label "" :type string)
  (color "#89b4fa" :type string)
  (glyph "λ" :type string))

;;; Invariant: Every `(BEG . END)' in `ranges' holds markers in `buffer'
;;; until `lispm-ui-spotlight--release-beam' clears them.
(cl-defstruct (lispm-ui-spotlight-beam
               (:constructor lispm-ui-spotlight-beam--create)
               (:copier nil)
               (:predicate lispm-ui-spotlight-beam-p))
  "A spotlight beam cast by `actor' onto line ranges of `buffer'."
  (id 0 :type integer)
  actor
  (step 0 :type integer)
  buffer
  tag-marker
  (ranges nil :type list)
  note
  fade-at)

(cl-defstruct (lispm-ui-spotlight-store
               (:constructor lispm-ui-spotlight-store--create)
               (:copier nil)
               (:predicate lispm-ui-spotlight-store-p))
  "Reactive FRP store driving spotlight overlays across buffers."
  beams-signal
  clock-signal
  (tint 0.14 :type number)
  (fade-duration 0.9 :type number)
  (priority 5 :type integer)
  (next-id 0 :type integer)
  (rendered-buffers nil :type list)
  effect
  timer
  on-reap)

;;;; Pure interval & colour combinators

;;; Law: (= (lispm-ui-spotlight-beam-alpha nil now d) 1.0) for all NOW, D > 0.
;;; Post: (<= 0.0 result 1.0)
(defun lispm-ui-spotlight-beam-alpha (fade-at now fade-duration)
  "Return opacity in [0.0, 1.0] at NOW for a beam fading from FADE-AT.
FADE-DURATION is the fade length in seconds; nil FADE-AT means 1.0."
  (cond
   ((or (null fade-at) (<= now fade-at)) 1.0)
   ((<= fade-duration 0) 0.0)
   (t (max 0.0 (min 1.0 (- 1.0 (/ (- now fade-at) (float fade-duration))))))))

;;; Pre: LINES is a list of positive integers.
;;; Post: Sorted list of disjoint inclusive `(START . END)' pairs.
(defun lispm-ui-spotlight-coalesce-lines (lines)
  "Group positive integers LINES into sorted inclusive `(START . END)' pairs."
  (let ((sorted (delete-dups (sort (copy-sequence lines) #'<))))
    (nreverse
     (seq-reduce (lambda (acc line)
                   (if (and acc (= line (1+ (cdar acc))))
                       (cons (cons (caar acc) line) (cdr acc))
                     (cons (cons line line) acc)))
                 sorted
                 nil))))

(defun lispm-ui-spotlight--covers-p (spans beg end)
  "Return non-nil when any `(S-BEG . S-END)' in SPANS covers `[BEG, END)'."
  (seq-some (pcase-lambda (`(,sbeg . ,send))
              (and (<= sbeg beg) (<= end send)))
            spans))

(defun lispm-ui-spotlight--unique-actors (items)
  "Keep the last entry per `:actor-key' in ITEMS, preserving first order."
  (let ((latest (make-hash-table :test #'equal))
        order)
    (dolist (item items)
      (let ((key (or (plist-get item :actor-key)
                     (plist-get item :agent-key))))
        (unless (gethash key latest)
          (push key order))
        (puthash key item latest)))
    (mapcar (lambda (key) (gethash key latest)) (nreverse order))))

;;; Pre: Each item in ITEMS has `:tag-pos' and `:actor-key' (or `:agent-key').
;;; Post: Sorted list of `(:pos POS :items LIST)' with unique actors per line.
(defun lispm-ui-spotlight-compose-tags (items)
  "Group ITEMS by `:tag-pos' and deduplicate per actor on each line."
  (let ((groups (make-hash-table :test #'eql))
        positions)
    (dolist (item items)
      (let ((pos (plist-get item :tag-pos)))
        (when (integerp pos)
          (unless (gethash pos groups)
            (push pos positions))
          (puthash pos (append (gethash pos groups) (list item)) groups))))
    (mapcar (lambda (pos)
              (list :pos pos
                    :items (lispm-ui-spotlight--unique-actors
                            (gethash pos groups))))
            (sort positions #'<))))

;;; Pre: Each item in ITEMS carries `:spans' of `(BEG . END)'.
;;; Post: Disjoint adjacent segments `(:beg B :end E :items COVERING)', B < E.
(defun lispm-ui-spotlight-compose-segments (items)
  "Decompose overlapping `:spans' in ITEMS into elementary disjoint intervals."
  (let* ((coords (delete-dups
                  (sort (cl-loop for item in items
                                 append (cl-loop for (b . e)
                                                 in (plist-get item :spans)
                                                 when (< b e)
                                                 collect b and collect e))
                        #'<)))
         segments)
    (while (cdr coords)
      (let* ((beg (car coords))
             (end (cadr coords))
             (covering (lispm-ui-spotlight--unique-actors
                        (seq-filter (lambda (item)
                                      (lispm-ui-spotlight--covers-p
                                       (plist-get item :spans) beg end))
                                    items))))
        (when covering
          (push (list :beg beg :end end :items covering) segments)))
      (setq coords (cdr coords)))
    (nreverse segments)))

(defun lispm-ui-spotlight-weighted-color (items)
  "Return the alpha-weighted `#rrggbb' colour of ITEMS, or nil."
  (let ((sum-r 0.0) (sum-g 0.0) (sum-b 0.0) (sum-w 0.0))
    (dolist (item items)
      (let ((rgb (lispm-ui-rgb (plist-get item :color)))
            (w (max 0.01 (float (or (plist-get item :alpha) 1.0)))))
        (when rgb
          (setq sum-r (+ sum-r (* w (nth 0 rgb)))
                sum-g (+ sum-g (* w (nth 1 rgb)))
                sum-b (+ sum-b (* w (nth 2 rgb)))
                sum-w (+ sum-w w)))))
    (when (> sum-w 0.0)
      (format "#%02x%02x%02x"
              (round (* 255 (/ sum-r sum-w)))
              (round (* 255 (/ sum-g sum-w)))
              (round (* 255 (/ sum-b sum-w)))))))

;;;; Marker lifecycle & overlay boundary

(defun lispm-ui-spotlight--line-marker (buffer line)
  "Return a marker at the start of 1-based LINE in BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (forward-line (1- (max 1 line)))
        (copy-marker (point))))))

(defun lispm-ui-spotlight--line-markers (buffer line-ranges)
  "Convert 1-based LINE-RANGES in BUFFER into `(BEG . END)' marker pairs."
  (with-current-buffer buffer
    (save-excursion
      (save-restriction
        (widen)
        (mapcar (pcase-lambda (`(,lo . ,hi))
                  (goto-char (point-min))
                  (forward-line (1- lo))
                  (let ((beg (point)))
                    (forward-line (1+ (- hi lo)))
                    (cons (copy-marker beg)
                          (copy-marker (if (> (point) beg)
                                           (point)
                                         (point-max))))))
                line-ranges)))))

(defun lispm-ui-spotlight--release-beam (beam)
  "Detach all markers owned by BEAM."
  (when (markerp (lispm-ui-spotlight-beam-tag-marker beam))
    (set-marker (lispm-ui-spotlight-beam-tag-marker beam) nil))
  (pcase-dolist (`(,beg . ,end) (lispm-ui-spotlight-beam-ranges beam))
    (when (markerp beg) (set-marker beg nil))
    (when (markerp end) (set-marker end nil))))

(defun lispm-ui-spotlight-clear-buffer-overlays (buffer &optional store)
  "Delete spotlight overlays in BUFFER belonging to STORE (or any store)."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (dolist (ov (overlays-in (point-min) (point-max)))
          (when (and (overlay-get ov 'lispm-ui-spotlight)
                     (or (null store)
                         (eq (overlay-get ov 'lispm-ui-spotlight-store) store)))
            (delete-overlay ov)))))))

(defun lispm-ui-spotlight--badge (glyph label color note alpha bg fg)
  "Render a spotlight header badge with GLYPH, LABEL, COLOR, NOTE and ALPHA.
BG and FG are the buffer's background and foreground colours."
  (let* ((chip-bg (or (lispm-ui-blend bg color (* 0.85 alpha)) color))
         (bar-bg (or (lispm-ui-blend bg color alpha) color))
         (dark-fg (or (lispm-ui-legible chip-bg "#1e1e2e" "#ffffff") fg "black"))
         (chip-fg (or (lispm-ui-blend bg dark-fg alpha) dark-fg))
         (prefix (if (string-empty-p (or glyph "")) " " (concat " " glyph " "))))
    (concat (propertize " " 'face `(:background ,bar-bg))
            (propertize " " 'face 'default)
            (propertize (concat prefix label (if note (concat " · " note) "") " ")
                        'face `(:background ,chip-bg :foreground ,chip-fg
                                            :weight bold :height 0.85)))))

(defun lispm-ui-spotlight--put-overlay (store buffer beg end &rest props)
  "Create a spotlight overlay for STORE in BUFFER from BEG to END with PROPS."
  (let ((ov (make-overlay beg end buffer t nil)))
    (overlay-put ov 'lispm-ui-spotlight t)
    (overlay-put ov 'lispm-ui-spotlight-store store)
    (overlay-put ov 'lispm-spotlight t)
    (overlay-put ov 'priority (lispm-ui-spotlight-store-priority store))
    (while props
      (overlay-put ov (pop props) (pop props)))
    ov))

(defun lispm-ui-spotlight--beam-item (beam now fade-duration)
  "Project BEAM at time NOW with FADE-DURATION into a render plist, or nil."
  (let ((alpha (lispm-ui-spotlight-beam-alpha
                (lispm-ui-spotlight-beam-fade-at beam) now fade-duration)))
    (when (> alpha 0.01)
      (let ((actor (lispm-ui-spotlight-beam-actor beam)))
        (list :actor-key (lispm-ui-spotlight-actor-key actor)
              :label (lispm-ui-spotlight-actor-label actor)
              :glyph (lispm-ui-spotlight-actor-glyph actor)
              :color (lispm-ui-spotlight-actor-color actor)
              :alpha alpha
              :note (lispm-ui-spotlight-beam-note beam)
              :tag-pos (marker-position (lispm-ui-spotlight-beam-tag-marker beam))
              :spans (mapcar (pcase-lambda (`(,b . ,e))
                               (cons (marker-position b) (marker-position e)))
                             (lispm-ui-spotlight-beam-ranges beam)))))))

(defun lispm-ui-spotlight--render-buffer (store buffer beams now)
  "Render BEAMS for STORE onto BUFFER at time NOW."
  (when (buffer-live-p buffer)
    (lispm-ui-spotlight-clear-buffer-overlays buffer store)
    (let* ((fade-dur (lispm-ui-spotlight-store-fade-duration store))
           (tint-base (lispm-ui-spotlight-store-tint store))
           (bg (or (face-background 'default nil t) "#1e1e2e"))
           (fg (or (face-foreground 'default nil t) "#cdd6f4"))
           (items (seq-keep (lambda (b)
                              (lispm-ui-spotlight--beam-item b now fade-dur))
                            beams)))
      (when items
        (dolist (group (lispm-ui-spotlight-compose-tags items))
          (let ((pos (plist-get group :pos))
                (row (mapconcat
                      (lambda (it)
                        (lispm-ui-spotlight--badge
                         (plist-get it :glyph) (plist-get it :label)
                         (plist-get it :color) (plist-get it :note)
                         (plist-get it :alpha) bg fg))
                      (plist-get group :items)
                      (propertize "  " 'face 'default))))
            (lispm-ui-spotlight--put-overlay
             store buffer pos pos 'before-string (concat row "\n"))))
        (dolist (seg (lispm-ui-spotlight-compose-segments items))
          (let* ((covering (plist-get seg :items))
                 (max-a (apply #'max (mapcar (lambda (it) (plist-get it :alpha))
                                             covering)))
                 (mixed (lispm-ui-spotlight-weighted-color covering))
                 (strength (min 0.35 (* tint-base max-a
                                        (if (> (length covering) 1) 1.35 1.0))))
                 (tint (and mixed (lispm-ui-blend bg mixed strength)))
                 (stripe (mapconcat
                          (lambda (it)
                            (let ((sc (or (lispm-ui-blend
                                           bg (plist-get it :color)
                                           (* 0.9 (plist-get it :alpha)))
                                          (plist-get it :color))))
                              (propertize "▎" 'face `(:foreground ,sc))))
                          covering "")))
            (lispm-ui-spotlight--put-overlay
             store buffer (plist-get seg :beg) (plist-get seg :end)
             'face (if tint `(:background ,tint :extend t) 'highlight)
             'line-prefix stripe)))))))

;;;; Reactive FRP effect & animation clock

(defun lispm-ui-spotlight--sync-timer (store beams)
  "Start or stop STORE's fade timer depending on whether BEAMS has fades."
  (let ((fading (seq-some #'lispm-ui-spotlight-beam-fade-at beams))
        (timer (lispm-ui-spotlight-store-timer store)))
    (cond
     ((and fading (not (timerp timer)))
      (setf (lispm-ui-spotlight-store-timer store)
            (run-at-time 0.066 0.066 #'lispm-ui-spotlight--timer-step store)))
     ((and (not fading) (timerp timer))
      (cancel-timer timer)
      (setf (lispm-ui-spotlight-store-timer store) nil)))))

(defun lispm-ui-spotlight--reconcile (store beams now _palette-gen)
  "Reconcile overlays across all buffers touched by STORE's BEAMS at NOW."
  (let ((by-buf (make-hash-table :test #'eq))
        active-bufs)
    (dolist (beam beams)
      (let ((buf (lispm-ui-spotlight-beam-buffer beam)))
        (when (buffer-live-p buf)
          (unless (gethash buf by-buf)
            (push buf active-bufs))
          (puthash buf (append (gethash buf by-buf) (list beam)) by-buf))))
    (dolist (prev (lispm-ui-spotlight-store-rendered-buffers store))
      (unless (memq prev active-bufs)
        (lispm-ui-spotlight-clear-buffer-overlays prev store)))
    (dolist (buf active-bufs)
      (lispm-ui-spotlight--render-buffer store buf (gethash buf by-buf) now))
    (setf (lispm-ui-spotlight-store-rendered-buffers store) active-bufs)
    (lispm-ui-spotlight--sync-timer store beams)
    (when-let* ((reap (lispm-ui-spotlight-store-on-reap store)))
      (funcall reap))))

(defun lispm-ui-spotlight--timer-step (store)
  "Advance STORE's fade clock when no interactive user input is pending."
  (unless (input-pending-p)
    (lispm-ui-spotlight-tick store (float-time))))

;;;; Public store API

(cl-defun lispm-ui-spotlight-make-store
    (&key (tint 0.14) (fade-duration 0.9) (priority 5) on-reap)
  "Create a reactive `lispm-ui-spotlight-store'.
TINT sets fill strength, FADE-DURATION sets fade seconds, PRIORITY sets
overlay priority, and ON-REAP runs after each reconciliation pass."
  (let* ((beams-sig (lispm-ui-signal nil #'equal))
         (clock-sig (lispm-ui-signal (float-time) #'eql))
         (store (lispm-ui-spotlight-store--create
                 :beams-signal beams-sig
                 :clock-signal clock-sig
                 :tint tint
                 :fade-duration fade-duration
                 :priority priority
                 :on-reap on-reap)))
    (setf (lispm-ui-spotlight-store-effect store)
          (lispm-ui-effect
           (list beams-sig clock-sig lispm-ui-palette-generation-signal)
           (lambda (beams now gen)
             (lispm-ui-spotlight--reconcile store beams now gen))))
    store))

(defun lispm-ui-spotlight-dispose-store (store)
  "Dispose STORE's reactive effect, timer, markers, and overlays."
  (when (lispm-ui-spotlight-store-p store)
    (when (timerp (lispm-ui-spotlight-store-timer store))
      (cancel-timer (lispm-ui-spotlight-store-timer store))
      (setf (lispm-ui-spotlight-store-timer store) nil))
    (lispm-ui-effect-dispose (lispm-ui-spotlight-store-effect store))
    (dolist (beam (lispm-ui-signal-peek (lispm-ui-spotlight-store-beams-signal store)))
      (lispm-ui-spotlight--release-beam beam))
    (dolist (buf (lispm-ui-spotlight-store-rendered-buffers store))
      (lispm-ui-spotlight-clear-buffer-overlays buf store))
    (setf (lispm-ui-spotlight-store-rendered-buffers store) nil)))

(defun lispm-ui-spotlight-beams (store)
  "Return the current list of `lispm-ui-spotlight-beam' values in STORE."
  (lispm-ui-signal-get (lispm-ui-spotlight-store-beams-signal store)))

(defun lispm-ui-spotlight-buffer-active-p (store buffer)
  "Return non-nil when STORE holds any live beam on BUFFER."
  (and (lispm-ui-spotlight-store-p store)
       (seq-some (lambda (beam)
                   (eq (lispm-ui-spotlight-beam-buffer beam) buffer))
                 (lispm-ui-spotlight-beams store))))

(defun lispm-ui-spotlight-tick (store &optional now)
  "Advance STORE's reactive fade clock to NOW and prune expired beams."
  (let* ((now (or now (float-time)))
         (fade-dur (lispm-ui-spotlight-store-fade-duration store))
         (current (lispm-ui-spotlight-beams store))
         kept)
    (dolist (beam current)
      (let ((buf (lispm-ui-spotlight-beam-buffer beam))
            (fade-at (lispm-ui-spotlight-beam-fade-at beam)))
        (if (or (not (buffer-live-p buf))
                (and fade-at
                     (<= (lispm-ui-spotlight-beam-alpha fade-at now fade-dur)
                         0.0)))
            (lispm-ui-spotlight--release-beam beam)
          (push beam kept))))
    (lispm-ui-frp-batch
      (lispm-ui-signal-set (lispm-ui-spotlight-store-beams-signal store)
                           (nreverse kept))
      (lispm-ui-signal-set (lispm-ui-spotlight-store-clock-signal store) now))))

(defun lispm-ui-spotlight-fade-where (store predicate fade-at &optional now)
  "Schedule steady beams in STORE matching PREDICATE to fade at FADE-AT.
NOW defaults to `float-time'."
  (let* ((now (or now (float-time)))
         (current (lispm-ui-spotlight-beams store))
         (changed nil)
         (updated
          (mapcar (lambda (beam)
                    (if (and (null (lispm-ui-spotlight-beam-fade-at beam))
                             (funcall predicate beam))
                        (progn
                          (setq changed t)
                          (lispm-ui-spotlight-beam--create
                           :id (lispm-ui-spotlight-beam-id beam)
                           :actor (lispm-ui-spotlight-beam-actor beam)
                           :step (lispm-ui-spotlight-beam-step beam)
                           :buffer (lispm-ui-spotlight-beam-buffer beam)
                           :tag-marker (lispm-ui-spotlight-beam-tag-marker beam)
                           :ranges (lispm-ui-spotlight-beam-ranges beam)
                           :note (lispm-ui-spotlight-beam-note beam)
                           :fade-at fade-at))
                      beam))
                  current)))
    (when changed
      (lispm-ui-frp-batch
        (lispm-ui-signal-set (lispm-ui-spotlight-store-beams-signal store)
                             updated)
        (lispm-ui-signal-set (lispm-ui-spotlight-store-clock-signal store)
                             now)))))

(defun lispm-ui-spotlight-remove-where (store predicate &optional now)
  "Immediately remove beams matching PREDICATE from STORE at NOW."
  (let* ((now (or now (float-time)))
         (current (lispm-ui-spotlight-beams store))
         (removed nil)
         kept)
    (dolist (beam current)
      (if (funcall predicate beam)
          (progn
            (setq removed t)
            (lispm-ui-spotlight--release-beam beam))
        (push beam kept)))
    (when removed
      (lispm-ui-frp-batch
        (lispm-ui-signal-set (lispm-ui-spotlight-store-beams-signal store)
                             (nreverse kept))
        (lispm-ui-signal-set (lispm-ui-spotlight-store-clock-signal store)
                             now)))))

(cl-defun lispm-ui-spotlight-cast
    (store actor buffer anchor-line line-ranges
           &key (step 0) note fade-at retire-before-step now)
  "Cast ACTOR's beam onto BUFFER at ANCHOR-LINE and LINE-RANGES in STORE.
STEP tags the actor's turn step; when RETIRE-BEFORE-STEP is non-nil,
steady beams of ACTOR with step < STEP begin fading at NOW.  NOTE is
shown in the header badge; FADE-AT schedules automatic retirement."
  (unless (lispm-ui-spotlight-store-p store)
    (signal 'lispm-ui-spotlight-error (list "Not a spotlight store" store)))
  (unless (lispm-ui-spotlight-actor-p actor)
    (signal 'lispm-ui-spotlight-error (list "Not a spotlight actor" actor)))
  (when (buffer-live-p buffer)
    (let* ((now (or now (float-time)))
           (id (cl-incf (lispm-ui-spotlight-store-next-id store)))
           (actor-key (lispm-ui-spotlight-actor-key actor))
           (beam (lispm-ui-spotlight-beam--create
                  :id id
                  :actor actor
                  :step step
                  :buffer buffer
                  :tag-marker (lispm-ui-spotlight--line-marker buffer anchor-line)
                  :ranges (lispm-ui-spotlight--line-markers buffer line-ranges)
                  :note note
                  :fade-at fade-at)))
      (lispm-ui-frp-batch
        (when retire-before-step
          (lispm-ui-spotlight-fade-where
           store
           (lambda (b)
             (and (equal (lispm-ui-spotlight-actor-key
                          (lispm-ui-spotlight-beam-actor b))
                         actor-key)
                  (< (lispm-ui-spotlight-beam-step b) step)))
           now now))
        (lispm-ui-signal-set
         (lispm-ui-spotlight-store-beams-signal store)
         (append (lispm-ui-spotlight-beams store) (list beam)))
        (lispm-ui-signal-set (lispm-ui-spotlight-store-clock-signal store) now))
      beam)))

(provide 'lispm-ui-spotlight)
;;; lispm-ui-spotlight.el ends here
