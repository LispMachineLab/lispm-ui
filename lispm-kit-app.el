;;; lispm-kit-app.el --- Declarative application shell framework -*- lexical-binding: t; -*-

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

;; Declarative App Kit 2.0 runtime for LispM applications:
;; - `lispm-kit-define-app' registers app metadata, layout shell
;;   (`single', `master-detail', `three-pane', `tabs'), routes,
;;   reactive `lispm-ui-store' state, `.eld' persistence with migration,
;;   `lispm-kit-define-commands', and lifecycle hooks.
;; - Built-in navigation history stack (`navigate', `back', `forward',
;;   breadcrumb), `lispm-ui-split' multi-pane shells, Command Palette,
;;   and non-blocking toast notifications.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'lispm-kit)
(require 'lispm-ui-base)
(require 'lispm-ui-frp)
(require 'lispm-ui-frp-async)
(require 'lispm-ui-tokens)
(require 'lispm-ui-icons)
(require 'lispm-ui-view)
(require 'lispm-ui-layout)
(require 'lispm-ui-controls-extra)
(require 'lispm-ui-feedback)
(require 'lispm-ui-overlay)
(require 'lispm-ui-rich)
(require 'lispm-ui-tabs)

;;;; Registry & Structures

(defvar lispm-kit-app--registry (make-hash-table :test #'eq)
  "Registered applications keyed by app symbol.")

(cl-defstruct (lispm-kit-app-spec
               (:constructor lispm-kit-app-spec--create)
               (:copier nil)
               (:predicate lispm-kit-app-spec-p))
  name mode title version icon
  (layout 'single)
  initial-state state-file
  (state-version 1 :type integer)
  migrate routes default-route
  toolbar sidebar inspector footer
  on-mount on-unmount on-activate on-deactivate on-resize on-kill)

(cl-defstruct (lispm-kit-app-instance
               (:constructor lispm-kit-app-instance--create)
               (:copier nil)
               (:predicate lispm-kit-app-instance-p))
  spec buffer store scope rev-signal
  history forward-stack
  split side-buffers
  size-hook select-hook
  dirty active rendering closed)

(defvar-local lispm-kit-app--current nil
  "Active `lispm-kit-app-instance' associated with this buffer.")

;;;; Registration & Macro

;;;###autoload
(cl-defun lispm-kit-app-register (name mode &rest plist)
  "Register application NAME using major MODE and specification PLIST."
  (let ((spec (lispm-kit-app-spec--create
               :name name
               :mode mode
               :title (or (plist-get plist :title) (symbol-name name))
               :version (or (plist-get plist :version) "0.1.0")
               :icon (plist-get plist :icon)
               :layout (or (plist-get plist :layout) 'single)
               :initial-state (plist-get plist :initial-state)
               :state-file (plist-get plist :state-file)
               :state-version (or (plist-get plist :state-version) 1)
               :migrate (plist-get plist :migrate)
               :routes (plist-get plist :routes)
               :default-route (plist-get plist :default-route)
               :toolbar (plist-get plist :toolbar)
               :sidebar (plist-get plist :sidebar)
               :inspector (plist-get plist :inspector)
               :footer (plist-get plist :footer)
               :on-mount (plist-get plist :on-mount)
               :on-unmount (plist-get plist :on-unmount)
               :on-activate (plist-get plist :on-activate)
               :on-deactivate (plist-get plist :on-deactivate)
               :on-resize (plist-get plist :on-resize)
               :on-kill (plist-get plist :on-kill))))
    (puthash name spec lispm-kit-app--registry)
    spec))

(defun lispm-kit-app--strip-commands (plist)
  "Return a copy of PLIST omitting the compile-time `:commands' pair."
  (let (out)
    (while plist
      (let ((k (pop plist))
            (v (pop plist)))
        (unless (eq k :commands)
          (push k out)
          (push v out))))
    (nreverse out)))

;;;###autoload
(defmacro lispm-kit-define-app (name &rest plist)
  "Define declarative LispM application NAME with specification PLIST.
Creates major mode `NAME-mode' derived from `lispm-ui-view-mode', optional
commands via `lispm-kit-define-commands', and interactive launcher NAME."
  (declare (indent 1))
  (let* ((mode-sym (intern (format "%s-mode" name)))
         (map-sym (intern (format "%s-mode-map" name)))
         (cmds (plist-get plist :commands))
         (runtime-plist (lispm-kit-app--strip-commands plist))
         (title (or (and (stringp (plist-get plist :title)) (plist-get plist :title))
                    (symbol-name name))))
    `(progn
       (defvar-keymap ,map-sym
         :doc ,(format "Keymap for `%s'." mode-sym)
         :parent lispm-ui-view-mode-map
         "[" #'lispm-kit-app-back-cmd
         "]" #'lispm-kit-app-forward-cmd
         "M-<left>" #'lispm-kit-app-back-cmd
         "M-<right>" #'lispm-kit-app-forward-cmd
         ":" #'lispm-kit-app-palette-cmd
         "g" #'lispm-kit-app-refresh-cmd
         "s" #'lispm-kit-app-save-cmd
         "q" #'quit-window)
       (define-derived-mode ,mode-sym lispm-ui-view-mode ,title
         ,(format "Interactive application mode for `%s'." name)
         (setq-local lispm-ui-view-render-function #'lispm-kit-app-refresh-cmd))
       ,@(when cmds
           (let ((forms (if (eq (car-safe cmds) 'quote) (cadr cmds) cmds)))
             `((lispm-kit-define-commands ,mode-sym ,@forms))))
       (lispm-kit-app-register ',name ',mode-sym ,@runtime-plist)
       (defun ,name ()
         ,(format "Launch the %s application." title)
         (interactive)
         (lispm-kit-app-launch ',name))
       ',name)))

;;;; Persistence (.eld safe I/O + schema migration)

(defun lispm-kit-app--resolve-state-file (spec)
  "Return expanded state file path for SPEC, or nil."
  (let ((raw (lispm-kit-app-spec-state-file spec)))
    (when raw
      (let ((path (if (functionp raw) (funcall raw) raw)))
        (when (and (stringp path) (not (string-empty-p path)))
          (expand-file-name path))))))

(defun lispm-kit-app--merge-plist (base over)
  "Return fresh plist starting from BASE with OVER keys overriding."
  (let ((res (copy-tree base)))
    (cl-loop for (k v) on over by #'cddr
             do (setq res (plist-put res k v)))
    res))

(defun lispm-kit-app-load-state (spec)
  "Load persisted `.eld' state for SPEC, running `:migrate' when needed."
  (let* ((base (copy-tree (or (lispm-kit-app-spec-initial-state spec) nil)))
         (file (lispm-kit-app--resolve-state-file spec))
         (target-ver (lispm-kit-app-spec-state-version spec))
         (migrate (lispm-kit-app-spec-migrate spec)))
    (if (not (and file (file-readable-p file)))
        base
      (condition-case err
          (let* ((data (with-temp-buffer
                         (insert-file-contents file)
                         (goto-char (point-min))
                         (read (current-buffer))))
                 (ver (or (plist-get data :version) 0))
                 (loaded (plist-get data :state))
                 (migrated (if (and (not (eql ver target-ver)) (functionp migrate))
                               (funcall migrate ver loaded)
                             loaded)))
            (if (listp migrated)
                (lispm-kit-app--merge-plist base migrated)
              base))
        (error
         (message "lispm-kit-app: ignoring unreadable state %s (%S)" file err)
         base)))))

(defun lispm-kit-app-save-state (app)
  "Atomically persist APP store snapshot and `:state-version' to `:state-file'."
  (when (lispm-kit-app-instance-p app)
    (when-let* ((file (lispm-kit-app--resolve-state-file
                       (lispm-kit-app-instance-spec app))))
      (let* ((dir (file-name-directory file))
             (payload (list :version (lispm-kit-app-spec-state-version
                                      (lispm-kit-app-instance-spec app))
                            :state (lispm-kit-app-state app)))
             (tmp nil))
        (when (and dir (not (file-directory-p dir)))
          (make-directory dir t))
        (setq tmp (make-temp-file (concat file ".tmp.")))
        (unwind-protect
            (progn
              (with-temp-file tmp
                (let ((print-level nil)
                      (print-length nil)
                      (print-circle nil))
                  (prin1 payload (current-buffer))
                  (insert "\n")))
              (rename-file tmp file t)
              (setf (lispm-kit-app-instance-dirty app) nil)
              file)
          (when (and tmp (file-exists-p tmp))
            (ignore-errors (delete-file tmp))))))))

;;;; Store State & Navigation

(defun lispm-kit-app-state (app)
  "Return current state plist snapshot stored in APP."
  (when (and (lispm-kit-app-instance-p app)
             (lispm-ui-store-p (lispm-kit-app-instance-store app)))
    (lispm-ui-store-snapshot (lispm-kit-app-instance-store app))))

(defun lispm-kit-app-dispatch (app updater)
  "Apply UPDATER to APP state snapshot, committing changed store slices."
  (when (and (lispm-kit-app-instance-p app)
             (not (lispm-kit-app-instance-closed app))
             (functionp updater))
    (let* ((prev (lispm-kit-app-state app))
           (next (funcall updater (copy-tree prev)))
           (store (lispm-kit-app-instance-store app)))
      (when (listp next)
        (setf (lispm-kit-app-instance-dirty app) t)
        (lispm-ui-frp-batch
          (cl-loop for (k v) on next by #'cddr
                   do (lispm-ui-store-set store k v))
          (lispm-ui-signal-set
           (lispm-kit-app-instance-rev-signal app)
           (1+ (lispm-ui-signal-get (lispm-kit-app-instance-rev-signal app)))))
        next))))

(defun lispm-kit-app--route-spec (spec route-id)
  "Return route entry plist for ROUTE-ID inside SPEC, or first route."
  (let ((routes (lispm-kit-app-spec-routes spec)))
    (or (assq route-id routes) (car routes))))

(defun lispm-kit-app-current-route (app)
  "Return current route entry `(:route ID :params P :label STR)' for APP."
  (and (lispm-kit-app-instance-p app)
       (car (lispm-kit-app-instance-history app))))

(defun lispm-kit-app-navigate (app route &optional params label)
  "Push ROUTE with optional PARAMS and LABEL onto APP navigation stack."
  (when (and (lispm-kit-app-instance-p app)
             (not (lispm-kit-app-instance-closed app)))
    (let* ((spec (lispm-kit-app-instance-spec app))
           (r-entry (lispm-kit-app--route-spec spec route))
           (lbl (or label (plist-get (cdr r-entry) :label) (format "%s" route)))
           (head (lispm-kit-app-current-route app)))
      (unless (and head (eq (plist-get head :route) route)
                   (equal (plist-get head :params) params))
        (push (list :route route :params params :label lbl)
              (lispm-kit-app-instance-history app))
        (setf (lispm-kit-app-instance-forward-stack app) nil)
        (lispm-kit-app-render app))))
  (lispm-kit-app-current-route app))

(defun lispm-kit-app-back (app)
  "Navigate back one entry in APP history stack."
  (when (and (lispm-kit-app-instance-p app)
             (cdr (lispm-kit-app-instance-history app)))
    (push (pop (lispm-kit-app-instance-history app))
          (lispm-kit-app-instance-forward-stack app))
    (lispm-kit-app-render app)
    (lispm-kit-app-current-route app)))

(defun lispm-kit-app-forward (app)
  "Navigate forward one entry from APP forward stack."
  (when (and (lispm-kit-app-instance-p app)
             (lispm-kit-app-instance-forward-stack app))
    (push (pop (lispm-kit-app-instance-forward-stack app))
          (lispm-kit-app-instance-history app))
    (lispm-kit-app-render app)
    (lispm-kit-app-current-route app)))

(defun lispm-kit-app-breadcrumb-items (app)
  "Return `lispm-ui-breadcrumb' item specs along APP history trail."
  (let* ((trail (reverse (lispm-kit-app-instance-history app)))
         (len (length trail)))
    (cl-loop for entry in trail for idx from 0
             collect
             (let ((pops (- len 1 idx)))
               (list :label (plist-get entry :label)
                     :action (when (> pops 0)
                               (lambda ()
                                 (dotimes (_ pops)
                                   (push (pop (lispm-kit-app-instance-history app))
                                         (lispm-kit-app-instance-forward-stack app)))
                                 (lispm-kit-app-render app))))))))

;;;; Commands, Palette, Toast & Lifecycle

(defun lispm-kit-app-commands (app)
  "Collect Palette command entries for APP (declared + shell navigation)."
  (let* ((spec (lispm-kit-app-instance-spec app))
         (mode (lispm-kit-app-spec-mode spec))
         (buf (lispm-kit-app-instance-buffer app))
         (declared (get mode 'lispm-kit-commands))
         (items nil))
    (dolist (c declared)
      (let* ((cmd (plist-get c :command))
             (en-fn (plist-get c :enabled))
             (lbl-fn (plist-get c :label))
             (lbl (cond ((functionp lbl-fn) (with-current-buffer buf (funcall lbl-fn)))
                        ((stringp lbl-fn) lbl-fn)
                        (t (symbol-name cmd))))
             (keys (plist-get c :key))
             (enabled (or (null en-fn) (with-current-buffer buf (funcall en-fn)))))
        (when enabled
          (push (list :id cmd
                      :title lbl
                      :category (or (plist-get c :help) (lispm-kit-app-spec-title spec))
                      :shortcut (if (consp keys) (car keys) keys)
                      :action (lambda ()
                                (with-current-buffer buf
                                  (call-interactively cmd))))
                items))))
    (nconc
     (nreverse items)
     (delq nil
           (list
            (when (cdr (lispm-kit-app-instance-history app))
              (list :id 'lispm-kit-app-back :title "Back" :category "Navigation"
                    :shortcut "[" :action (lambda () (lispm-kit-app-back app))))
            (when (lispm-kit-app-instance-forward-stack app)
              (list :id 'lispm-kit-app-forward :title "Forward" :category "Navigation"
                    :shortcut "]" :action (lambda () (lispm-kit-app-forward app))))
            (list :id 'lispm-kit-app-refresh :title "Refresh" :category "View"
                  :shortcut "g" :action (lambda () (lispm-kit-app-render app)))
            (when (lispm-kit-app-spec-state-file spec)
              (list :id 'lispm-kit-app-save :title "Save State" :category "State"
                    :shortcut "s" :action (lambda () (lispm-kit-app-save-state app)))))))))

(defun lispm-kit-app-open-palette (app)
  "Open Command Palette populated with APP commands."
  (when (lispm-kit-app-instance-p app)
    (lispm-ui-command-palette (lispm-kit-app-commands app)
                              :prompt (concat (lispm-kit-app-spec-title
                                               (lispm-kit-app-instance-spec app))
                                              " > "))))

(cl-defun lispm-kit-app-notify (app message &key (tone 'info) (duration 3.0) title id)
  "Enqueue non-blocking Toast MESSAGE for APP and refresh view cleanly."
  (let ((toast (lispm-ui-toast message :tone tone :duration duration :title title :id id)))
    (when (and (lispm-kit-app-instance-p app)
               (buffer-live-p (lispm-kit-app-instance-buffer app)))
      (lispm-kit-app-render app))
    toast))

(defun lispm-kit-app--teardown (app)
  "Run unmount/kill lifecycle hooks and dispose resources for APP."
  (when (and (lispm-kit-app-instance-p app)
             (not (lispm-kit-app-instance-closed app)))
    (setf (lispm-kit-app-instance-closed app) t)
    (when (lispm-kit-app-instance-size-hook app)
      (remove-hook 'window-size-change-functions (lispm-kit-app-instance-size-hook app)))
    (when (lispm-kit-app-instance-select-hook app)
      (remove-hook 'window-selection-change-functions (lispm-kit-app-instance-select-hook app)))
    (let ((spec (lispm-kit-app-instance-spec app)))
      (when (lispm-kit-app-spec-state-file spec)
        (ignore-errors (lispm-kit-app-save-state app)))
      (when (functionp (lispm-kit-app-spec-on-unmount spec))
        (ignore-errors (funcall (lispm-kit-app-spec-on-unmount spec) app)))
      (when (functionp (lispm-kit-app-spec-on-kill spec))
        (ignore-errors (funcall (lispm-kit-app-spec-on-kill spec) app))))
    (when (lispm-ui-split-p (lispm-kit-app-instance-split app))
      (lispm-ui-split-dispose (lispm-kit-app-instance-split app)))
    (pcase-dolist (`(,_ . ,sbuf) (lispm-kit-app-instance-side-buffers app))
      (when (buffer-live-p sbuf) (kill-buffer sbuf)))
    (when (lispm-ui-scope-p (lispm-kit-app-instance-scope app))
      (lispm-ui-scope-dispose (lispm-kit-app-instance-scope app)))))

;;;; Shell Rendering

(defun lispm-kit-app--render-toolbar (app spec state)
  "Insert top toolbar bar and breadcrumb trail for APP."
  (let* ((title (lispm-kit-app-spec-title spec))
         (icon (lispm-kit-app-spec-icon spec))
         (heading (concat (if icon (concat (lispm-ui-icon icon) " ") "")
                          (propertize title 'face 'lispm-ui-h3)))
         (custom (when (functionp (lispm-kit-app-spec-toolbar spec))
                   (funcall (lispm-kit-app-spec-toolbar spec) app state)))
         (left (delq nil (list
                          (lispm-ui-button "◀" :variant 'ghost
                                           :disabled (null (cdr (lispm-kit-app-instance-history app))))
                          (lispm-ui-button "▶" :variant 'ghost
                                           :disabled (null (lispm-kit-app-instance-forward-stack app)))
                          heading)))
         (right (delq nil (list custom (lispm-ui-button "Commands" :variant 'secondary)))))
    (insert (lispm-ui-toolbar :left left :right right) "\n")
    (when-let* ((crumbs (lispm-kit-app-breadcrumb-items app)))
      (when (> (length crumbs) 1)
        (insert (lispm-ui-breadcrumb crumbs) "\n")))
    (insert (lispm-ui-divider) "\n")))

(defun lispm-kit-app--render-pane (app side fn state)
  "Render optional SIDE (`sidebar' or `inspector') pane via FN for APP."
  (when (functionp fn)
    (let* ((pane-side (if (eq side 'sidebar) 'left 'right))
           (sbuf (or (alist-get side (lispm-kit-app-instance-side-buffers app))
                     (let ((b (generate-new-buffer
                               (format " *%s:%s*"
                                       (lispm-kit-app-spec-title
                                        (lispm-kit-app-instance-spec app))
                                       side))))
                       (setf (alist-get side (lispm-kit-app-instance-side-buffers app)) b)
                       b)))
           (split (lispm-kit-app-instance-split app)))
      (with-current-buffer sbuf
        (lispm-ui-view-mode)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (let ((res (funcall fn app state)))
            (when (stringp res) (insert res)))
          (goto-char (point-min))))
      (when (and (lispm-ui-split-p split)
                 (window-live-p (lispm-ui-split-main-window split)))
        (ignore-errors (lispm-ui-split-open split pane-side sbuf))))))

(defun lispm-kit-app--render-body (app spec state)
  "Render tabs (when `:layout' is `tabs') and active route body for APP."
  (let* ((entry (lispm-kit-app-current-route app))
         (route-id (plist-get entry :route))
         (params (plist-get entry :params))
         (routes (lispm-kit-app-spec-routes spec))
         (r-spec (lispm-kit-app--route-spec spec route-id))
         (render-fn (plist-get (cdr r-spec) :render)))
    (when (and (eq (lispm-kit-app-spec-layout spec) 'tabs) routes)
      (lispm-ui-insert-tabs
       (mapcar (lambda (r)
                 (cons (car r) (or (plist-get (cdr r) :label) (symbol-name (car r)))))
               routes)
       route-id
       (lambda (next-id) (lispm-kit-app-navigate app next-id))))
    (cond
     ((functionp render-fn)
      (let ((out (funcall render-fn app state params)))
        (when (stringp out) (insert out "\n"))))
     ((stringp render-fn)
      (insert render-fn "\n"))
     (t
      (insert (lispm-ui-empty-state "No route renderer") "\n")))))

(defun lispm-kit-app-render (app)
  "Re-render APP main buffer and configured side panes preserving scroll."
  (when (and (lispm-kit-app-instance-p app)
             (not (lispm-kit-app-instance-closed app))
             (not (lispm-kit-app-instance-rendering app))
             (buffer-live-p (lispm-kit-app-instance-buffer app)))
    (setf (lispm-kit-app-instance-rendering app) t)
    (unwind-protect
        (let* ((spec (lispm-kit-app-instance-spec app))
               (state (lispm-kit-app-state app))
               (buf (lispm-kit-app-instance-buffer app))
               (layout (lispm-kit-app-spec-layout spec)))
          (with-current-buffer buf
            (lispm-ui-with-preserved-scroll nil
              (let ((inhibit-read-only t))
                (erase-buffer)
                (lispm-kit-app--render-toolbar app spec state)
                (lispm-kit-app--render-body app spec state)
                (when (lispm-ui-toast-active)
                  (insert "\n" (lispm-ui-toast-render) "\n"))
                (let ((foot (if (functionp (lispm-kit-app-spec-footer spec))
                                (funcall (lispm-kit-app-spec-footer spec) app state)
                              (format "%s  v%s%s"
                                      (lispm-kit-app-spec-title spec)
                                      (lispm-kit-app-spec-version spec)
                                      (if (lispm-kit-app-instance-dirty app) " *" "")))))
                  (insert "\n" (lispm-ui-divider) "\n"
                          (lispm-ui-footer-bar :left foot))))))
          (when (memq layout '(master-detail three-pane))
            (lispm-kit-app--render-pane app 'sidebar
                                        (lispm-kit-app-spec-sidebar spec) state))
          (when (eq layout 'three-pane)
            (lispm-kit-app--render-pane app 'inspector
                                        (lispm-kit-app-spec-inspector spec) state)))
      (setf (lispm-kit-app-instance-rendering app) nil))
    app))

;;;; Launcher & Interactive Commands

(defun lispm-kit-app--attach-hooks (app buf spec)
  "Attach reactive store effects and lifecycle hooks to APP in BUF."
  (let* ((scope (lispm-kit-app-instance-scope app))
         (store (lispm-kit-app-instance-store app))
         (sigs (let (list)
                 (push (lispm-kit-app-instance-rev-signal app) list)
                 (maphash (lambda (_k sig) (push sig list))
                          (lispm-ui-store-slices store))
                 list))
         (size-fn (lambda (frame)
                    (when (and (buffer-live-p buf) (not (lispm-kit-app-instance-closed app))
                               (functionp (lispm-kit-app-spec-on-resize spec)))
                      (funcall (lispm-kit-app-spec-on-resize spec) app frame))))
         (sel-fn (lambda (_frame)
                   (when (and (buffer-live-p buf) (not (lispm-kit-app-instance-closed app)))
                     (let ((now-act (eq (window-buffer (selected-window)) buf)))
                       (unless (eq now-act (lispm-kit-app-instance-active app))
                         (setf (lispm-kit-app-instance-active app) now-act)
                         (let ((fn (if now-act
                                       (lispm-kit-app-spec-on-activate spec)
                                     (lispm-kit-app-spec-on-deactivate spec))))
                           (when (functionp fn) (funcall fn app)))))))))
    (lispm-ui-with-scope scope
      (lispm-ui-effect sigs (lambda (&rest _) (lispm-kit-app-render app))))
    (setf (lispm-kit-app-instance-size-hook app) size-fn
          (lispm-kit-app-instance-select-hook app) sel-fn)
    (with-current-buffer buf
      (add-hook 'kill-buffer-hook (lambda () (lispm-kit-app--teardown app)) nil t)
      (add-hook 'window-size-change-functions size-fn)
      (add-hook 'window-selection-change-functions sel-fn))))

;;;###autoload
(cl-defun lispm-kit-app-launch (name &key route params)
  "Launch or focus registered LispM application NAME and return its instance."
  (let* ((spec (or (gethash name lispm-kit-app--registry)
                   (error "Unknown LispM app: %S" name)))
         (buf (get-buffer-create (format "*%s*" (lispm-kit-app-spec-title spec))))
         (existing (buffer-local-value 'lispm-kit-app--current buf)))
    (if (and (lispm-kit-app-instance-p existing)
             (not (lispm-kit-app-instance-closed existing)))
        (progn
          (when route (lispm-kit-app-navigate existing route params))
          (pop-to-buffer buf)
          existing)
      (with-current-buffer buf
        (funcall (lispm-kit-app-spec-mode spec))
        (let* ((scope (lispm-ui-scope-create))
               (init-state (lispm-kit-app-load-state spec))
               (store (lispm-ui-with-scope scope (lispm-ui-store init-state)))
               (rev (lispm-ui-with-scope scope (lispm-ui-signal 0)))
               (r-id (or route (lispm-kit-app-spec-default-route spec)
                         (car (car (lispm-kit-app-spec-routes spec)))))
               (r-spec (and r-id (lispm-kit-app--route-spec spec r-id)))
               (r-lbl (or (plist-get (cdr r-spec) :label)
                          (and r-id (symbol-name r-id))
                          "Home"))
               (app (lispm-kit-app-instance--create
                     :spec spec :buffer buf :store store :scope scope
                     :rev-signal rev
                     :history (when r-id (list (list :route r-id :params params :label r-lbl)))
                     :split (lispm-ui-split-create :main-window (selected-window)))))
          (setq lispm-ui-view-scope scope
                lispm-kit-app--current app)
          (lispm-kit-app--attach-hooks app buf spec)
          (when (functionp (lispm-kit-app-spec-on-mount spec))
            (funcall (lispm-kit-app-spec-on-mount spec) app))
          (pop-to-buffer buf)
          (when-let* ((w (get-buffer-window buf)))
            (setf (lispm-ui-split-main-window (lispm-kit-app-instance-split app)) w
                  (lispm-kit-app-instance-active app) t))
          (lispm-kit-app-render app)
          app)))))

(defun lispm-kit-app-back-cmd ()
  "Navigate back inside the active LispM App buffer."
  (interactive)
  (when lispm-kit-app--current (lispm-kit-app-back lispm-kit-app--current)))

(defun lispm-kit-app-forward-cmd ()
  "Navigate forward inside the active LispM App buffer."
  (interactive)
  (when lispm-kit-app--current (lispm-kit-app-forward lispm-kit-app--current)))

(defun lispm-kit-app-refresh-cmd ()
  "Re-render the active LispM App buffer."
  (interactive)
  (when lispm-kit-app--current (lispm-kit-app-render lispm-kit-app--current)))

(defun lispm-kit-app-save-cmd ()
  "Persist state of the active LispM App buffer."
  (interactive)
  (when lispm-kit-app--current (lispm-kit-app-save-state lispm-kit-app--current)))

(defun lispm-kit-app-palette-cmd ()
  "Open Command Palette for the active LispM App."
  (interactive)
  (when lispm-kit-app--current (lispm-kit-app-open-palette lispm-kit-app--current)))

(provide 'lispm-kit-app)
;;; lispm-kit-app.el ends here
