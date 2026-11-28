;;; lispm-ui-modal.el --- What LispM UI's modal panels share  -*- lexical-binding: t; -*-

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

;; Menus, alerts, dialogs and the keyboard's walks through a row of
;; controls each read events in a loop of their own until something is
;; chosen.  What they share is here: reading an event without quitting,
;; what each key does, stepping a focus ring, blinking a control as it
;; is pressed, finding what a click is on, and telling a drag from a
;; click.
;;
;; Every key works from the HHKB, which has no arrow keys of its own: C-f,
;; C-b, C-n and C-p go where the arrows do, Tab and Shift-Tab ring the next
;; and previous control, Return chooses, Space presses the control ringed
;; and Escape or C-g gives up.  A loop takes from here only the roles it
;; has use for; a menu's Right, say, opens a submenu.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'lispm-ui-frp)

(defun lispm-ui-modal-read-event (&optional timeout)
  "Read the next event, as `read-event' does; with TIMEOUT, return nil after
that many seconds without one.  A text terminal sends arrows and the
mouse as sequences of characters, which are read there as the keys and
clicks they are, see `read-key'; Escape alone starts such a sequence, so
it is typed twice, as Emacs's own menus take it."
  (if (and (not noninteractive) (not (display-graphic-p)))
      (if timeout
          (with-timeout (timeout nil) (read-key))
        (read-key))
    (read-event nil nil timeout)))

(defun lispm-ui-modal-read (&optional timeout)
  "Read the next event, a quit included, without quitting.
With TIMEOUT, return nil after that many seconds without one.  The click
that opened a panel is not echoed as an unfinished key.  On a text
terminal Escape is typed twice, see `lispm-ui-modal-read-event'."
  (let* ((inhibit-quit t)
         (event (let ((echo-keystrokes 0)) (lispm-ui-modal-read-event timeout))))
    (when (eq event ?\C-g)
      (setq quit-flag nil))
    event))

(defconst lispm-ui-modal-keys
  '((next tab ?\t)
    (previous backtab S-tab S-iso-lefttab)
    (right ?\C-f right)
    (left ?\C-b left)
    (down ?\C-n down)
    (up ?\C-p up)
    (confirm return ?\r ?\n kp-enter)
    (press ?\s)
    (cancel escape ?\e ?\C-g ?\M-.)
    (back ?\d backspace))
  "What keys do in LispM UI's modal panels: (ROLE EVENT...).
`next' and `previous' ring the next or previous control, `right',
`left', `down' and `up' go that way, `confirm' chooses the default,
`press' presses the control ringed, `cancel' gives up and `back' goes
back.  Escape is ESC, C-g and Command-period, as on Mac OS 9.")

(defun lispm-ui-modal-role (event)
  "Return the role of EVENT in a modal panel, see `lispm-ui-modal-keys'."
  (car (seq-find (lambda (entry) (memql event (cdr entry))) lispm-ui-modal-keys)))

(defun lispm-ui-modal-step (event)
  "Return 1 when EVENT moves on in a focus ring, -1 when it moves back, else nil:
Tab, C-f and C-n move on, Shift-Tab, C-b and C-p back."
  (pcase (lispm-ui-modal-role event)
    ((or 'next 'right 'down) 1)
    ((or 'previous 'left 'up) -1)))

(defun lispm-ui-modal-cancel-p (event)
  "Return non-nil when EVENT gives up: Escape, C-g or Command-period."
  (eq (lispm-ui-modal-role event) 'cancel))

(defun lispm-ui-modal-confirm-p (event)
  "Return non-nil when EVENT chooses: Return or Enter."
  (eq (lispm-ui-modal-role event) 'confirm))

(defun lispm-ui-modal-mouse-down-p (event)
  "Return non-nil when EVENT presses the first mouse button down."
  (and (consp event) (eq (event-basic-type event) 'mouse-1)
       (memq 'down (event-modifiers event))))

(defun lispm-ui-modal-mouse-up-p (event)
  "Return non-nil when EVENT lets the first mouse button go."
  (and (consp event) (eq (event-basic-type event) 'mouse-1)
       (not (memq 'down (event-modifiers event)))))

(defun lispm-ui-modal-over (event frame)
  "Return EVENT's position when it is over FRAME, else nil."
  (let ((position (event-end event)))
    (when (and (windowp (posn-window position))
               (eq (window-frame (posn-window position)) frame))
      position)))

(defun lispm-ui-modal-cycle (index step count &optional ok-p)
  "Return the index STEP places from INDEX among COUNT, wrapping around.
From no INDEX, a step on goes to the first and a step back to the last.
With OK-P, indexes it refuses are passed over; nil when it takes none."
  (when (> count 0)
    (let ((at (or index (if (> step 0) -1 count))))
      (if (not ok-p)
          (mod (+ at step) count)
        (cl-loop repeat count
                 do (setq at (mod (+ at (cl-signum step)) count))
                 when (funcall ok-p at) return at)))))

(defconst lispm-ui-modal-flashes
  '((button . 0.133) (menu-item . 0.05))
  "How long a control chosen blinks, in seconds: a button pressed from the
keyboard stays pushed in for eight ticks, as Mac OS 9's did; a menu item
chosen goes off and on for three ticks each.")

(defun lispm-ui-modal-flash (kind draw &optional undraw)
  "Blink a control of KIND, a key of `lispm-ui-modal-flashes', as it is
chosen: call DRAW to draw it pressed, show it for the time KIND blinks,
then call UNDRAW, when given, and show that."
  (funcall draw)
  (redisplay t)
  (sit-for (alist-get kind lispm-ui-modal-flashes))
  (when undraw
    (funcall undraw)
    (redisplay t)))

(defun lispm-ui-modal-inside-p (rect x y)
  "Return non-nil when X, Y lies in RECT, (LEFT TOP WIDTH HEIGHT) in pixels.
The rectangle holds its left and top edges, not its right and bottom."
  (pcase-let ((`(,left ,top ,width ,height) rect))
    (and rect (<= left x) (< x (+ left width)) (<= top y) (< y (+ top height)))))

(defun lispm-ui-modal-hit (layout x y)
  "Return the value of what LAYOUT has at X, Y, or nil.
LAYOUT is a list of (VALUE LEFT TOP WIDTH HEIGHT); the first holding X, Y
wins."
  (car (seq-find (lambda (entry) (lispm-ui-modal-inside-p (cdr entry) x y)) layout)))

(defun lispm-ui-modal-hold-step (in event over-p)
  "Pure transition for `lispm-ui-modal-hold'.
IN is the current pressed boolean; EVENT is the input event; OVER-P tests
whether a pointer event is over the target.
Return `(NEW-IN ACTION)' where ACTION is `continue', `choose', `cancel',
or `(redraw . NEW-IN)'."
  (cond
   ((lispm-ui-modal-cancel-p event)
    (list in 'cancel))
   ((lispm-ui-modal-mouse-up-p event)
    (list in (if (funcall over-p event) 'choose 'cancel)))
   ((mouse-movement-p event)
    (let ((over (and (funcall over-p event) t)))
      (if (eq over in)
          (list in 'continue)
        (list over (cons 'redraw over)))))
   (t
    (list in 'continue))))

(defun lispm-ui-modal-hold (over-p draw)
  "Hold a control pressed with the mouse until the button rises, as Mac OS
9's controls were: DRAW is called with t to draw it pushed in while the
mouse is over it, and with nil to draw it out; OVER-P, called with a
mouse event, says whether the event is over it.  Return non-nil when the
button rose over the control, which is then chosen.  Escape or C-g gives
up."
  (let ((in t) (chosen nil))
    (funcall draw t)
    (unwind-protect
        (track-mouse
          (catch 'done
            (while t
              (pcase-let ((`(,next-in ,action)
                           (lispm-ui-modal-hold-step in (lispm-ui-modal-read) over-p)))
                (setq in next-in)
                (pcase action
                  ('cancel (throw 'done nil))
                  ('choose (setq chosen t) (throw 'done nil))
                  (`(redraw . ,over) (funcall draw over)))))))
      (when in
        (funcall draw nil)))
    chosen))

(defun lispm-ui-modal-coalesce-mouse-move (event)
  "Consume queued mouse-movement events and return the latest movement EVENT."
  (while (and (not noninteractive)
              (input-pending-p)
              (let ((next (read-event nil nil 0)))
                (if (mouse-movement-p next)
                    (progn (setq event next) t)
                  (when next
                    (push next unread-command-events))
                  nil))))
  event)

(defun lispm-ui-modal-coalesce-wheel (delta wheel-fn &optional ok-p)
  "Accumulate queued same-direction wheel events into DELTA using WHEEL-FN.
When OK-P is non-nil, each event must also satisfy OK-P."
  (when (not noninteractive)
    (let (next next-delta)
      (while (and (setq next (lispm-ui-modal-read 0))
                  (setq next-delta (funcall wheel-fn next))
                  (= (cl-signum next-delta) (cl-signum delta))
                  (or (null ok-p) (funcall ok-p next)))
        (setq delta (+ delta next-delta)
              next nil))
      (when next
        (push next unread-command-events))))
  delta)

(defun lispm-ui-modal-raise (frame)
  "Keep modal popup FRAME and its shadow raised above its parent frame."
  (when (and (not noninteractive) (framep frame) (frame-live-p frame))
    (let ((shadow (frame-parameter frame 'lispm-ui-popup-shadow)))
      (when (and (framep shadow) (frame-live-p shadow))
        (raise-frame shadow)))
    (raise-frame frame)))

(cl-defun lispm-ui-modal-run (&key initial step render effect frame)
  "Run a pure reactive modal loop starting from INITIAL state.
STEP is called as `(funcall STEP state event)' and returns
`(NEXT-STATE . ACTIONS)'.  Each action in ACTIONS is processed:
- `(:redraw)' calls `(funcall RENDER next-state)` when given;
- `(:flash KIND DRAW-FN [UNDRAW-FN])' flashes a control;
- `(:return VALUE)' exits the loop returning VALUE;
- any other action is passed to `(funcall EFFECT next-state action)'."
  (let* ((lispm-ui-frp--batch-depth 0)
         (lispm-ui-frp--pending-signals nil)
         (lispm-ui-frp--pending-effects nil)
         (state-sig (lispm-ui-signal initial #'eq))
         (render-eff (and render (lispm-ui-effect state-sig render))))
    (unwind-protect
        (track-mouse
          (catch 'lispm-ui-modal-done
            (while t
              (let* ((event (lispm-ui-modal-read))
                     (stepped (funcall step (lispm-ui-signal-get state-sig) event))
                     (next-state (car stepped))
                     (actions (cdr stepped)))
                (when (and frame
                           (or (eq (car-safe event) 'switch-frame)
                               (and (lispm-ui-modal-mouse-down-p event)
                                    (not (lispm-ui-modal-over event frame)))))
                  (lispm-ui-modal-raise frame))
                (lispm-ui-signal-set state-sig next-state)
                (dolist (act actions)
                  (pcase act
                    ('(:redraw)
                     (when render (funcall render (lispm-ui-signal-get state-sig))))
                    (`(:flash ,kind ,draw-fn . ,rest)
                     (lispm-ui-modal-flash kind draw-fn (car rest)))
                    (`(:return ,value)
                     (throw 'lispm-ui-modal-done value))
                    (_
                     (when effect
                       (funcall effect (lispm-ui-signal-get state-sig) act)))))))))
      (when render-eff
        (lispm-ui-effect-dispose render-eff)))))

(defun lispm-ui-modal-moved-p (delta &optional cells)
  "Return non-nil when DELTA, (DX . DY) in pixels, is a move rather than a
hand's jitter in a click: more than `double-click-fuzz' either way.
With CELLS, DELTA counts a text terminal's cells, of which Emacs reads
`double-click-fuzz' as eighths: there a move by a cell is one."
  (and delta
       (let ((fuzz (if (numberp double-click-fuzz) double-click-fuzz 3)))
         (> (max (abs (car delta)) (abs (cdr delta)))
            (if cells (/ fuzz 8) fuzz)))))

(defconst lispm-ui-modal-areas
  '(tab-bar tab-line header-line mode-line menu-bar tool-bar vertical-line
    vertical-scroll-bar horizontal-scroll-bar left-fringe right-fringe
    left-margin right-margin right-divider bottom-divider internal-border
    child-frame-border)
  "Symbols `read-event' returns before a mouse event over part of a frame
or window other than text: the event follows.")

(defun lispm-ui-modal-typed-p (event)
  "Return non-nil when EVENT was typed: a character or a function key, not
a mouse event, nor the area symbol before one, see `lispm-ui-modal-areas'.
A panel opened from the keyboard starts on its first control."
  (or (integerp event)
      (and (symbolp event) event (not (memq event lispm-ui-modal-areas)))))

(provide 'lispm-ui-modal)
;;; lispm-ui-modal.el ends here
