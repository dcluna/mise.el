;;; mise.el --- Manage dev tools, env vars, and tasks with mise -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Daniel Luna

;; Author: Daniel Luna
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (transient "0.6"))
;; Keywords: tools, processes
;; URL: https://github.com/dcluna/mise.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; mise.el provides a comprehensive Emacs interface for mise (mise-en-place),
;; the polyglot dev tool manager.  It uses transient menus for discoverability
;; and tabulated-list-mode buffers for browsing tools, tasks, env vars, and
;; config files.
;;
;; Entry point: M-x mise

;;; Code:

(require 'transient)
(require 'mise-core)

(defun mise--tools-status ()
  "Return a short status string for tools."
  (condition-case nil
      (let* ((tools (mise-core--run-json "ls" "--json"))
             (count 0)
             (outdated 0))
        (dolist (entry tools)
          (let ((versions (alist-get 'versions entry)))
            (setq count (+ count (length versions)))))
        (condition-case nil
            (setq outdated (length (mise-core--run-json "outdated" "--json")))
          (error nil))
        (if (> outdated 0)
            (format "%d installed, %d outdated" count outdated)
          (format "%d installed" count)))
    (error "?")))

(defun mise--tasks-status ()
  "Return a short status string for tasks."
  (condition-case nil
      (let ((tasks (mise-core--run-json "tasks" "ls" "--json")))
        (format "%d defined" (length tasks)))
    (error "?")))

;;;###autoload (autoload 'mise "mise" nil t)
(transient-define-prefix mise ()
  "Manage dev tools, env vars, and tasks with mise."
  [:description
   (lambda ()
     (format "Mise [%s]" (abbreviate-file-name (mise-core--default-directory))))
   ["Resources"
    ("t" mise-tools)
    ("T" mise-tasks)
    ("e" mise-env)
    ("c" mise-config)]
   ["Actions"
    ("d" "Doctor" mise-doctor)
    ("R" "Refresh" mise-refresh)]])

(defun mise-doctor ()
  "Run `mise doctor' and display the output."
  (interactive)
  (let ((output (mise-core--run "doctor")))
    (with-current-buffer (get-buffer-create "*mise-doctor*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert output))
      (special-mode)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

(defun mise-refresh ()
  "Refresh the current mise transient."
  (interactive)
  (transient-setup 'mise))

(provide 'mise)
;;; mise.el ends here
