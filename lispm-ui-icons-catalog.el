;;; lispm-ui-icons-catalog.el --- Built-in generic 16x16 UI vector icon catalog -*- lexical-binding: t; -*-

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

;; Built-in catalog of 52 clean 16x16 stroke-based UI SVG icons using
;; `currentColor' so `lispm-ui-icon' and `lispm-ui-icon-svg' can tint them
;; dynamically to match light, dark, and custom semantic palettes, paired
;; with single-cell TTY fallback glyphs.

;;; Code:

(defconst lispm-ui-icons-catalog
  '(("search"
     :svg-body "<circle cx=\"7\" cy=\"7\" r=\"4.5\"/><line x1=\"10.5\" y1=\"10.5\" x2=\"14\" y2=\"14\"/>"
     :tty "⌕")
    ("close"
     :svg-body "<line x1=\"4\" y1=\"4\" x2=\"12\" y2=\"12\"/><line x1=\"12\" y1=\"4\" x2=\"4\" y2=\"12\"/>"
     :tty "×")
    ("plus"
     :svg-body "<line x1=\"8\" y1=\"3\" x2=\"8\" y2=\"13\"/><line x1=\"3\" y1=\"8\" x2=\"13\" y2=\"8\"/>"
     :tty "+")
    ("minus"
     :svg-body "<line x1=\"3\" y1=\"8\" x2=\"13\" y2=\"8\"/>"
     :tty "−")
    ("check"
     :svg-body "<polyline points=\"3.5 8.5 6.5 11.5 12.5 4.5\"/>"
     :tty "✓")
    ("edit"
     :svg-body "<path d=\"M11 2.5l2.5 2.5L5.5 13H3v-2.5z\"/>"
     :tty "✎")
    ("trash"
     :svg-body "<polyline points=\"3 4.5 13 4.5\"/><path d=\"M5.5 4.5V3h5v1.5\"/><path d=\"M4.5 4.5l.7 8.5h5.6l.7-8.5\"/>"
     :tty "⌫")
    ("copy"
     :svg-body "<rect x=\"5.5\" y=\"5.5\" width=\"7.5\" height=\"8\" rx=\"1\"/><path d=\"M3 10.5V3.5A1 1 0 0 1 4 2.5h6.5\"/>"
     :tty "❐")
    ("settings"
     :svg-body "<circle cx=\"8\" cy=\"8\" r=\"2.2\"/><path d=\"M8 2v1.6M8 12.4V14M2 8h1.6M12.4 8H14M3.8 3.8l1.1 1.1M11.1 11.1l1.1 1.1M12.2 3.8l-1.1 1.1M4.9 11.1l-1.1 1.1\"/>"
     :tty "⚙")
    ("refresh"
     :svg-body "<path d=\"M13 7a5 5 0 1 0-1.5 4.5\"/><polyline points=\"13 3.5 13 7 9.5 7\"/>"
     :tty "↻")
    ("filter"
     :svg-body "<polygon points=\"2.5 3.5 13.5 3.5 9.5 8.5 9.5 13 6.5 11.5 6.5 8.5\"/>"
     :tty "⧗")
    ("sort-asc"
     :svg-body "<polyline points=\"4.5 9.5 8 5.5 11.5 9.5\" fill=\"currentColor\"/>"
     :tty "↑")
    ("sort-desc"
     :svg-body "<polyline points=\"4.5 6.5 8 10.5 11.5 6.5\" fill=\"currentColor\"/>"
     :tty "↓")
    ("chevron-up"
     :svg-body "<polyline points=\"4 10 8 6 12 10\"/>"
     :tty "⌃")
    ("chevron-down"
     :svg-body "<polyline points=\"4 6 8 10 12 6\"/>"
     :tty "⌄")
    ("chevron-left"
     :svg-body "<polyline points=\"10 4 6 8 10 12\"/>"
     :tty "‹")
    ("chevron-right"
     :svg-body "<polyline points=\"6 4 10 8 6 12\"/>"
     :tty "›")
    ("arrow-up"
     :svg-body "<line x1=\"8\" y1=\"13\" x2=\"8\" y2=\"3\"/><polyline points=\"4.5 6.5 8 3 11.5 6.5\"/>"
     :tty "↑")
    ("arrow-down"
     :svg-body "<line x1=\"8\" y1=\"3\" x2=\"8\" y2=\"13\"/><polyline points=\"4.5 9.5 8 13 11.5 9.5\"/>"
     :tty "↓")
    ("arrow-left"
     :svg-body "<line x1=\"13\" y1=\"8\" x2=\"3\" y2=\"8\"/><polyline points=\"6.5 4.5 3 8 6.5 11.5\"/>"
     :tty "←")
    ("arrow-right"
     :svg-body "<line x1=\"3\" y1=\"8\" x2=\"13\" y2=\"8\"/><polyline points=\"9.5 4.5 13 8 9.5 11.5\"/>"
     :tty "→")
    ("lock"
     :svg-body "<rect x=\"3.5\" y=\"7\" width=\"9\" height=\"6.5\" rx=\"1\"/><path d=\"M5.5 7V5a2.5 2.5 0 0 1 5 0v2\"/>"
     :tty "#")
    ("unlock"
     :svg-body "<rect x=\"3.5\" y=\"7\" width=\"9\" height=\"6.5\" rx=\"1\"/><path d=\"M5.5 7V5a2.5 2.5 0 0 1 4.7-1.2\"/>"
     :tty "◦")
    ("eye"
     :svg-body "<path d=\"M2 8s2.4-4 6-4 6 4 6 4-2.4 4-6 4-6-4-6-4z\"/><circle cx=\"8\" cy=\"8\" r=\"1.5\"/>"
     :tty "◉")
    ("eye-off"
     :svg-body "<path d=\"M2 8s2.4-4 6-4 6 4 6 4-2.4 4-6 4-6-4-6-4z\"/><line x1=\"3\" y1=\"13\" x2=\"13\" y2=\"3\"/>"
     :tty "◌")
    ("bell"
     :svg-body "<path d=\"M4.5 6.5a3.5 3.5 0 0 1 7 0c0 3 1.5 4 1.5 4H3s1.5-1 1.5-4z\"/><line x1=\"6.8\" y1=\"12.8\" x2=\"9.2\" y2=\"12.8\"/>"
     :tty "!")
    ("user"
     :svg-body "<circle cx=\"8\" cy=\"5\" r=\"2.5\"/><path d=\"M3 13.5a5 5 0 0 1 10 0\"/>"
     :tty "@")
    ("folder"
     :svg-body "<path d=\"M2 4.5h4.5l1.5 1.5H14v7H2z\"/>"
     :tty "▪")
    ("file"
     :svg-body "<path d=\"M4 2.5h5.5L12.5 5.5v8H4z\"/><polyline points=\"9.5 2.5 9.5 5.5 12.5 5.5\"/>"
     :tty "▫")
    ("code"
     :svg-body "<polyline points=\"5.5 4.5 2.5 8 5.5 11.5\"/><polyline points=\"10.5 4.5 13.5 8 10.5 11.5\"/>"
     :tty "λ")
    ("terminal"
     :svg-body "<rect x=\"2\" y=\"3\" width=\"12\" height=\"10\" rx=\"1\"/><polyline points=\"4.5 6.5 6.5 8 4.5 9.5\"/><line x1=\"8\" y1=\"10\" x2=\"11\" y2=\"10\"/>"
     :tty "$")
    ("chat"
     :svg-body "<path d=\"M2.5 3.5h11v7.5H6.5L3.5 13.5V11H2.5z\"/>"
     :tty "💬")
    ("play"
     :svg-body "<polygon points=\"5 3.5 12.5 8 5 12.5\" fill=\"currentColor\"/>"
     :tty "▸")
    ("pause"
     :svg-body "<line x1=\"5.5\" y1=\"3.5\" x2=\"5.5\" y2=\"12.5\"/><line x1=\"10.5\" y1=\"3.5\" x2=\"10.5\" y2=\"12.5\"/>"
     :tty "‖")
    ("download"
     :svg-body "<line x1=\"8\" y1=\"2.5\" x2=\"8\" y2=\"10\"/><polyline points=\"5 7.5 8 10.5 11 7.5\"/><path d=\"M3 12v1.5h10V12\"/>"
     :tty "↓")
    ("upload"
     :svg-body "<line x1=\"8\" y1=\"10.5\" x2=\"8\" y2=\"3\"/><polyline points=\"5 5.5 8 2.5 11 5.5\"/><path d=\"M3 12v1.5h10V12\"/>"
     :tty "↑")
    ("external-link"
     :svg-body "<path d=\"M10 3h3v3\"/><line x1=\"7.5\" y1=\"8.5\" x2=\"13\" y2=\"3\"/><path d=\"M11.5 9.5v3.5h-8v-8H7\"/>"
     :tty "↗")
    ("info"
     :svg-body "<circle cx=\"8\" cy=\"8\" r=\"5.5\"/><line x1=\"8\" y1=\"7.2\" x2=\"8\" y2=\"11\"/><circle cx=\"8\" cy=\"5\" r=\"0.5\" fill=\"currentColor\"/>"
     :tty "i")
    ("warning"
     :svg-body "<polygon points=\"8 2.5 14 13 2 13\"/><line x1=\"8\" y1=\"6.5\" x2=\"8\" y2=\"9.5\"/><circle cx=\"8\" cy=\"11.4\" r=\"0.5\" fill=\"currentColor\"/>"
     :tty "!")
    ("error"
     :svg-body "<circle cx=\"8\" cy=\"8\" r=\"5.5\"/><line x1=\"5.8\" y1=\"5.8\" x2=\"10.2\" y2=\"10.2\"/><line x1=\"10.2\" y1=\"5.8\" x2=\"5.8\" y2=\"10.2\"/>"
     :tty "×")
    ("success"
     :svg-body "<circle cx=\"8\" cy=\"8\" r=\"5.5\"/><polyline points=\"5.2 8.2 7.2 10.2 10.8 6.2\"/>"
     :tty "✓")
    ("star"
     :svg-body "<polygon points=\"8 2.5 9.7 6 13.5 6.5 10.7 9.2 11.4 13 8 11.2 4.6 13 5.3 9.2 2.5 6.5 6.3 6\"/>"
     :tty "★")
    ("pin"
     :svg-body "<path d=\"M5.5 3h5l-1 4.5 2 2H4.5l2-2z\"/><line x1=\"8\" y1=\"9.5\" x2=\"8\" y2=\"13.5\"/>"
     :tty "⌖")
    ("menu"
     :svg-body "<line x1=\"3\" y1=\"4.5\" x2=\"13\" y2=\"4.5\"/><line x1=\"3\" y1=\"8\" x2=\"13\" y2=\"8\"/><line x1=\"3\" y1=\"11.5\" x2=\"13\" y2=\"11.5\"/>"
     :tty "≡")
    ("more"
     :svg-body "<circle cx=\"3.5\" cy=\"8\" r=\"1\" fill=\"currentColor\"/><circle cx=\"8\" cy=\"8\" r=\"1\" fill=\"currentColor\"/><circle cx=\"12.5\" cy=\"8\" r=\"1\" fill=\"currentColor\"/>"
     :tty "…")
    ("more-vertical"
     :svg-body "<circle cx=\"8\" cy=\"3.5\" r=\"1\" fill=\"currentColor\"/><circle cx=\"8\" cy=\"8\" r=\"1\" fill=\"currentColor\"/><circle cx=\"8\" cy=\"12.5\" r=\"1\" fill=\"currentColor\"/>"
     :tty "⋮")
    ("home"
     :svg-body "<polyline points=\"2.5 7.5 8 2.5 13.5 7.5\"/><path d=\"M4.5 6.5v6.5h7V6.5\"/>"
     :tty "⌂")
    ("calendar"
     :svg-body "<rect x=\"2.5\" y=\"3.5\" width=\"11\" height=\"10\" rx=\"1\"/><line x1=\"2.5\" y1=\"6.5\" x2=\"13.5\" y2=\"6.5\"/><line x1=\"5.5\" y1=\"2\" x2=\"5.5\" y2=\"4.5\"/><line x1=\"10.5\" y1=\"2\" x2=\"10.5\" y2=\"4.5\"/>"
     :tty "▤")
    ("clock"
     :svg-body "<circle cx=\"8\" cy=\"8\" r=\"5.5\"/><polyline points=\"8 4.5 8 8 10.5 9.5\"/>"
     :tty "◷")
    ("tag"
     :svg-body "<path d=\"M2.5 8V3h5l6 6-5 5z\"/><circle cx=\"5.2\" cy=\"5.5\" r=\"0.7\" fill=\"currentColor\"/>"
     :tty "#")
    ("link"
     :svg-body "<path d=\"M6.5 9.5l3-3\"/><path d=\"M5.5 6.5L4 8a2.5 2.5 0 0 0 3.5 3.5l1.5-1.5M10.5 9.5L12 8a2.5 2.5 0 0 0-3.5-3.5L7 6\"/>"
     :tty "∞")
    ("sidebar"
     :svg-body "<rect x=\"2\" y=\"3\" width=\"12\" height=\"10\" rx=\"1\"/><line x1=\"6\" y1=\"3\" x2=\"6\" y2=\"13\"/>"
     :tty "◧"))
  "Built-in catalog of 52 16x16 stroke-based UI icons with TTY fallbacks.")

(defun lispm-ui-icons-catalog-entry (name)
  "Return catalog entry plist for icon NAME, or nil."
  (let ((key (if (symbolp name) (symbol-name name) name)))
    (cdr (assoc key lispm-ui-icons-catalog))))

(provide 'lispm-ui-icons-catalog)
;;; lispm-ui-icons-catalog.el ends here
