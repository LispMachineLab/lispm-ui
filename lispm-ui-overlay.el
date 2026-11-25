;;; lispm-ui-overlay.el --- Non-blocking popovers, dialogs, drawers & Command Palette -*- lexical-binding: t; -*-

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

;; Overlay & floating surface components for custom LispM UI apps:
;;
;; - Popover (`lispm-ui-popover-place', `-open', `-close') with anchor
;;   viewport clamping and vertical flip.
;; - Dialog (`lispm-ui-dialog-open', `-close') supporting both non-blocking
;;   and `:modal t` modes.
;; - Slide-over Drawer (`lispm-ui-drawer-open', `-close') anchored on any edge.
;; - Fuzzy Command Palette (`lispm-ui-command-palette-score', `-filter',
;;   and interactive `lispm-ui-command-palette').

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'lispm-ui-base)
(require 'lispm-ui-palette)
(require 'lispm-ui-tokens)
(require 'lispm-ui-popup)
(require 'lispm-ui-view)
(require 'lispm-ui-layout)
(require 'lispm-ui-controls-extra)
(require 'lispm-ui-input)
(require 'lispm-ui-feedback)

;;;; Anchored Popover

(defvar lispm-ui-overlay--active-popover nil
  "Currently open `lispm-ui-popover' instance, if any.")

(cl-defstruct (lispm-ui-popover
               (:constructor lispm-ui-popover--create)
               (:copier nil)
               (:predicate lispm-ui-popover-p))
  frame buffer prev-window on-close)

(defun lispm-ui-popover-place (anchor-x anchor-y pop-w pop-h frame-w frame-h &optional prefer)
  "Compute viewport-clamped `( :x X :y Y :side SIDE )' near anchor point.
Flips between `below' and `above' when space overflows FRAME-H."
  (let* ((pref (or prefer 'below))
         (x (max 0 (min anchor-x (max 0 (- frame-w pop-w)))))
         (fits-below (<= (+ anchor-y pop-h) frame-h))
         (fits-above (>= (- anchor-y pop-h) 0))
         (side (cond
                ((eq pref 'above) (if (or fits-above (not fits-below)) 'above 'below))
                (t (if (or fits-below (not fits-above)) 'below 'above))))
         (y (if (eq side 'above)
                (max 0 (- anchor-y pop-h))
              (max 0 (min anchor-y (max 0 (- frame-h pop-h)))))))
    (list :x x :y y :side side)))

(defun lispm-ui-popover-close (&optional popover)
  "Close POPOVER (or active popover) and invoke its `:on-close' callback."
  (interactive)
  (when-let* ((target (or popover lispm-ui-overlay--active-popover)))
    (when (eq target lispm-ui-overlay--active-popover)
      (setq lispm-ui-overlay--active-popover nil))
    (when (frame-live-p (lispm-ui-popover-frame target))
      (lispm-ui-popup-dispose (lispm-ui-popover-frame target)))
    (when (buffer-live-p (lispm-ui-popover-buffer target))
      (kill-buffer (lispm-ui-popover-buffer target)))
    (when (window-live-p (lispm-ui-popover-prev-window target))
      (select-window (lispm-ui-popover-prev-window target)))
    (when-let* ((cb (lispm-ui-popover-on-close target)))
      (funcall cb)))
  nil)

(cl-defun lispm-ui-popover-open (content &key anchor width height on-close)
  "Display CONTENT in an anchored popover buffer/frame; return `lispm-ui-popover'."
  (lispm-ui-popover-close)
  (let* ((prev-win (selected-window))
         (buf (generate-new-buffer " *lispm-popover*"))
         (w (or width 280))
         (h (or height 120))
         (pop (lispm-ui-popover--create :buffer buf :prev-window prev-win :on-close on-close)))
    (with-current-buffer buf
      (lispm-ui-view-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (if (functionp content) (or (funcall content) "") (or content "")))
        (goto-char (point-min)))
      (local-set-key (kbd "<escape>") #'lispm-ui-popover-close)
      (local-set-key (kbd "C-g") #'lispm-ui-popover-close)
      (local-set-key (kbd "q") #'lispm-ui-popover-close))
    (setq lispm-ui-overlay--active-popover pop)
    (if (and (not noninteractive) (lispm-ui-popup-available-p))
        (let* ((parent (selected-frame))
               (ax (or (car-safe anchor) 20))
               (ay (or (cdr-safe anchor) 40))
               (pos (lispm-ui-popover-place ax ay w h
                                            (frame-pixel-width parent)
                                            (frame-pixel-height parent)))
               (frame (lispm-ui-popup-make-frame parent)))
          (setf (lispm-ui-popover-frame pop) frame)
          (set-window-buffer (frame-root-window frame) buf)
          (lispm-ui-popup-set-size frame w h)
          (lispm-ui-popup-show frame (plist-get pos :x) (plist-get pos :y) 'plain))
      (display-buffer buf))
    pop))

;;;; Non-blocking & modal Dialog

(cl-defstruct (lispm-ui-dialog
               (:constructor lispm-ui-dialog--create)
               (:copier nil)
               (:predicate lispm-ui-dialog-p))
  buffer prev-window modal on-close closed result depth)

(defun lispm-ui-dialog-close (dialog &optional result)
  "Close DIALOG cleanly with optional RESULT."
  (interactive)
  (when (and (lispm-ui-dialog-p dialog)
             (not (lispm-ui-dialog-closed dialog)))
    (setf (lispm-ui-dialog-closed dialog) t
          (lispm-ui-dialog-result dialog) result)
    (when (window-live-p (lispm-ui-dialog-prev-window dialog))
      (select-window (lispm-ui-dialog-prev-window dialog)))
    (when (buffer-live-p (lispm-ui-dialog-buffer dialog))
      (kill-buffer (lispm-ui-dialog-buffer dialog)))
    (when-let* ((cb (lispm-ui-dialog-on-close dialog)))
      (funcall cb result))
    (when (and (lispm-ui-dialog-modal dialog)
               (lispm-ui-dialog-depth dialog)
               (= (recursion-depth) (1+ (lispm-ui-dialog-depth dialog))))
      (exit-recursive-edit)))
  result)

(cl-defun lispm-ui-dialog-open (title body &key actions width modal on-close)
  "Open a dialog titled TITLE showing BODY and semantic ACTIONS.
ACTIONS items are `(:label STR :variant SYM :action FN)'.  When MODAL is
non-nil, enter `recursive-edit' and return the selected action result."
  (let* ((buf (generate-new-buffer (format "*%s*" (or title "Dialog"))))
         (prev-win (selected-window))
         (dlg (lispm-ui-dialog--create
               :buffer buf
               :prev-window prev-win
               :modal (and modal t)
               :depth (when modal (recursion-depth))
               :on-close on-close))
         (w (or width 56)))
    (with-current-buffer buf
      (lispm-ui-view-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (when title
          (insert (propertize title 'face 'lispm-ui-h2) "\n")
          (insert (lispm-ui-divider :width w) "\n\n"))
        (if (functionp body)
            (funcall body)
          (insert (or body "") "\n"))
        (insert "\n")
        (dolist (spec (or actions `((:label "Close" :variant secondary :action ,(lambda () nil)))))
          (let ((fn (plist-get spec :action)))
            (lispm-ui-insert-button
             (plist-get spec :label)
             (lambda ()
               (let ((res (when (functionp fn) (funcall fn))))
                 (lispm-ui-dialog-close dlg res)))
             :variant (or (plist-get spec :variant) 'secondary))
            (insert "  ")))
        (goto-char (point-min)))
      (local-set-key (kbd "<escape>") (lambda () (interactive) (lispm-ui-dialog-close dlg nil)))
      (local-set-key (kbd "C-g") (lambda () (interactive) (lispm-ui-dialog-close dlg nil)))
      (local-set-key (kbd "q") (lambda () (interactive) (lispm-ui-dialog-close dlg nil))))
    (pop-to-buffer buf)
    (if modal
        (unwind-protect
            (progn (recursive-edit) (lispm-ui-dialog-result dlg))
          (unless (lispm-ui-dialog-closed dlg)
            (lispm-ui-dialog-close dlg nil)))
      dlg)))

;;;; Slide-over Drawer / Sheet

(cl-defstruct (lispm-ui-drawer
               (:constructor lispm-ui-drawer--create)
               (:copier nil)
               (:predicate lispm-ui-drawer-p))
  buffer window prev-window on-close closed)

(defun lispm-ui-drawer-close (drawer)
  "Close slide-over DRAWER window and restore prior window focus."
  (interactive)
  (when (and (lispm-ui-drawer-p drawer)
             (not (lispm-ui-drawer-closed drawer)))
    (setf (lispm-ui-drawer-closed drawer) t)
    (let ((win (lispm-ui-drawer-window drawer)))
      (when (and (window-live-p win)
                 (> (length (window-list (window-frame win) 'nomini)) 1))
        (ignore-errors (delete-window win))))
    (when (buffer-live-p (lispm-ui-drawer-buffer drawer))
      (kill-buffer (lispm-ui-drawer-buffer drawer)))
    (when (window-live-p (lispm-ui-drawer-prev-window drawer))
      (select-window (lispm-ui-drawer-prev-window drawer)))
    (when-let* ((cb (lispm-ui-drawer-on-close drawer)))
      (funcall cb)))
  nil)

(cl-defun lispm-ui-drawer-open (title body &key (side 'right) (size 44) on-close)
  "Open a slide-over side drawer titled TITLE showing BODY along SIDE."
  (let* ((buf (generate-new-buffer (format "*Drawer: %s*" (or title ""))))
         (prev-win (selected-window))
         (drawer (lispm-ui-drawer--create :buffer buf :prev-window prev-win :on-close on-close)))
    (with-current-buffer buf
      (lispm-ui-view-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (or title "") 'face 'lispm-ui-h2) "  ")
        (lispm-ui-insert-button "×" (lambda () (lispm-ui-drawer-close drawer)) :variant 'ghost)
        (insert "\n" (lispm-ui-divider :width (max 20 (- size 4))) "\n\n")
        (if (functionp body)
            (funcall body)
          (insert (or body "") "\n"))
        (goto-char (point-min)))
      (local-set-key (kbd "q") (lambda () (interactive) (lispm-ui-drawer-close drawer)))
      (local-set-key (kbd "<escape>") (lambda () (interactive) (lispm-ui-drawer-close drawer))))
    (let* ((param (if (memq side '(top bottom))
                      `(window-height . ,size)
                    `(window-width . ,size)))
           (win (display-buffer-in-side-window buf `((side . ,side) ,param))))
      (setf (lispm-ui-drawer-window drawer) win)
      (when (window-live-p win)
        (select-window win)))
    drawer))

;;;; Command Palette (fuzzy search + ranking + interactive buffer)

(defvar lispm-ui-command-palette--recent nil
  "Recently invoked command IDs ordered most-recent first.")

(defun lispm-ui-command-palette-score (query text)
  "Return fuzzy match score of QUERY within TEXT, or nil when no match."
  (let ((q (downcase (string-trim (or query ""))))
        (s (downcase (or text ""))))
    (cond
     ((string-empty-p q) 0)
     ((string-prefix-p q s) (+ 200 (length q)))
     (t
      (let ((qi 0)
            (qlen (length q))
            (si 0)
            (slen (length s))
            (score 0)
            (run 0))
        (while (and (< qi qlen) (< si slen))
          (if (= (aref q qi) (aref s si))
              (progn
                (setq run (1+ run)
                      score (+ score 10 (* 5 run)))
                (when (or (= si 0) (memq (aref s (1- si)) '(?\s ?- ?_ ?/ ?:)))
                  (setq score (+ score 15)))
                (setq qi (1+ qi)))
            (setq run 0))
          (setq si (1+ si)))
        (when (= qi qlen) score))))))

(defun lispm-ui-command-palette-filter (query commands &optional recent-ids)
  "Return enabled COMMANDS matching QUERY sorted by fuzzy score and RECENT-IDS."
  (let* ((q (string-trim (or query "")))
         (recents (or recent-ids lispm-ui-command-palette--recent))
         (scored nil))
    (dolist (cmd commands)
      (when (or (not (plist-member cmd :enabled))
                (plist-get cmd :enabled))
        (let* ((id (plist-get cmd :id))
               (title (or (plist-get cmd :title) (format "%s" id)))
               (cat (or (plist-get cmd :category) ""))
               (haystack (if (string-empty-p cat) title (concat cat ": " title)))
               (sc (lispm-ui-command-palette-score q haystack))
               (rec-idx (seq-position recents id #'equal)))
          (when sc
            (let ((bonus (if rec-idx (max 0 (- 30 (* 5 rec-idx))) 0)))
              (push (cons (+ sc bonus) cmd) scored))))))
    (mapcar #'cdr
            (sort scored (lambda (a b) (> (car a) (car b)))))))

(defun lispm-ui-command-palette--render-list (buf list-beg input commands index on-exec)
  "Render matched COMMANDS inside palette BUF starting at LIST-BEG."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let* ((q (or (lispm-ui-input-value input) ""))
             (matches (lispm-ui-command-palette-filter q commands))
             (idx (max 0 (min index (max 0 (1- (length matches))))))
             (inhibit-read-only t))
        (save-excursion
          (delete-region list-beg (point-max))
          (goto-char list-beg)
          (if (null matches)
              (insert (propertize "  No matching commands\n" 'face 'lispm-ui-muted))
            (cl-loop for cmd in (seq-take matches 15) for i from 0 do
                     (let* ((sel (= i idx))
                            (cat (plist-get cmd :category))
                            (title (or (plist-get cmd :title) ""))
                            (kb (plist-get cmd :keybinding))
                            (line (concat
                                   (if sel "▸ " "  ")
                                   (when cat (concat (lispm-ui-badge cat :tone 'info) " "))
                                   (propertize title 'face (if sel 'lispm-ui-h4 'lispm-ui-body))
                                   (when kb (concat "  " (propertize kb 'face 'lispm-ui-muted)))
                                   "\n")))
                       (insert (propertize line
                                           'lispm-ui-palette-cmd cmd
                                           'mouse-face 'highlight
                                           'keymap (let ((m (make-sparse-keymap)))
                                                     (keymap-set m "<down-mouse-1>"
                                                                 (lambda (_e) (interactive "e")
                                                                   (funcall on-exec cmd)))
                                                     m)
                                           'face (when sel
                                                   `(:background ,(or (lispm-ui-token :hover) "#eeeeee")
                                                     :extend t))))))))
        matches))))

(cl-defun lispm-ui-command-palette (commands &key prompt placeholder on-select)
  "Open an interactive fuzzy Command Palette over COMMANDS plist list."
  (let* ((buf (generate-new-buffer "*Command Palette*"))
         (prev-win (selected-window))
         (sel-idx 0)
         (list-beg nil)
         (input nil)
         (close (lambda ()
                  (when (buffer-live-p buf) (kill-buffer buf))
                  (when (window-live-p prev-win) (select-window prev-win))))
         (exec (lambda (cmd)
                 (let ((id (plist-get cmd :id))
                       (action (plist-get cmd :action)))
                   (when id
                     (setq lispm-ui-command-palette--recent
                           (seq-take (cons id (delete id lispm-ui-command-palette--recent)) 20)))
                   (funcall close)
                   (when (functionp on-select) (funcall on-select cmd))
                   (when (functionp action) (funcall action))))))
    (with-current-buffer buf
      (lispm-ui-view-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (or prompt "Commands") 'face 'lispm-ui-h3) "\n")
        (setq input (lispm-ui-insert-input
                     :placeholder (or placeholder "Type a command…")
                     :icon 'search
                     :clearable t
                     :on-change (lambda (_q)
                                  (setq sel-idx 0)
                                  (lispm-ui-command-palette--render-list
                                   buf list-beg input commands sel-idx exec))
                     :on-submit (lambda (q)
                                  (when-let* ((matches (lispm-ui-command-palette-filter q commands))
                                              (picked (nth sel-idx matches)))
                                    (funcall exec picked)))
                     :on-cancel close))
        (insert "\n\n")
        (setq list-beg (copy-marker (point) nil))
        (lispm-ui-command-palette--render-list buf list-beg input commands sel-idx exec)
        (local-set-key (kbd "<down>")
                       (lambda () (interactive)
                         (let ((n (length (lispm-ui-command-palette-filter
                                           (lispm-ui-input-value input) commands))))
                           (when (> n 0)
                             (setq sel-idx (mod (1+ sel-idx) n))
                             (lispm-ui-command-palette--render-list
                              buf list-beg input commands sel-idx exec)))))
        (local-set-key (kbd "<up>")
                       (lambda () (interactive)
                         (let ((n (length (lispm-ui-command-palette-filter
                                           (lispm-ui-input-value input) commands))))
                           (when (> n 0)
                             (setq sel-idx (mod (1- sel-idx) n))
                             (lispm-ui-command-palette--render-list
                              buf list-beg input commands sel-idx exec)))))
        (local-set-key (kbd "<escape>") (lambda () (interactive) (funcall close)))
        (lispm-ui-input-focus input)))
    (pop-to-buffer buf)
    buf))

(provide 'lispm-ui-overlay)
;;; lispm-ui-overlay.el ends here
