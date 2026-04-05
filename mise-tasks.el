;;; mise-tasks.el --- Task management for mise.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Daniel Luna

;; Author: Daniel Luna
;; Keywords: tools, processes

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

;; Tabulated-list interface for managing and running mise tasks.

;;; Code:

(require 'tabulated-list)
(require 'transient)
(require 'mise-core)

(defun mise-tasks--list ()
  "Fetch the list of tasks from mise.
Returns entries suitable for `tabulated-list-entries'."
  (let ((tasks (mise-core--run-json "tasks" "ls" "--json"))
        (entries '()))
    (dolist (task tasks)
      (let* ((name (or (alist-get 'name task) ""))
             (description (or (alist-get 'description task) ""))
             (source (let ((src (alist-get 'source task)))
                       (if (stringp src)
                           (abbreviate-file-name src)
                         "")))
             (depends (let ((deps (alist-get 'depends task)))
                        (if deps
                            (string-join (mapcar (lambda (d)
                                                   (if (stringp d) d (format "%s" d)))
                                                 deps)
                                         ", ")
                          ""))))
        (push (list name (vector name description source depends))
              entries)))
    (nreverse entries)))

(defun mise-tasks--refresh ()
  "Refresh the tasks list."
  (setq tabulated-list-entries (mise-tasks--list)))

(defvar mise-tasks-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'mise-tasks-run)
    (define-key map (kbd "w") #'mise-tasks-watch)
    (define-key map (kbd "e") #'mise-tasks-edit)
    (define-key map (kbd "i") #'mise-tasks-info)
    (define-key map (kbd "?") #'mise-tasks-transient)
    map)
  "Keymap for `mise-tasks-mode'.")

(define-derived-mode mise-tasks-mode tabulated-list-mode "Mise Tasks"
  "Major mode for listing mise tasks."
  (setq tabulated-list-format [("Task" 25 t)
                                ("Description" 40 t)
                                ("Source" 35 t)
                                ("Depends" 25 t)]
        tabulated-list-padding 2
        tabulated-list-sort-key '("Task" . nil))
  (add-hook 'tabulated-list-revert-hook #'mise-tasks--refresh nil t)
  (hl-line-mode 1)
  (tabulated-list-init-header))

(defun mise-tasks--current-task ()
  "Return the task name at point."
  (when-let ((entry (tabulated-list-get-entry)))
    (aref entry 0)))

;;;###autoload (autoload 'mise-tasks "mise-tasks" nil t)
(transient-define-prefix mise-tasks ()
  "Manage mise tasks."
  :transient-suffix 'transient--do-return
  ["Actions"
   ("r" "Run" mise-tasks-run)
   ("w" "Watch" mise-tasks-watch)
   ("e" "Edit" mise-tasks-edit)
   ("i" "Info" mise-tasks-info)
   ("l" "List (open buffer)" mise-tasks-list)])

;;;###autoload
(defun mise-tasks-list ()
  "Display the mise tasks buffer."
  (interactive)
  (let ((buf (get-buffer-create "*mise-tasks*")))
    (with-current-buffer buf
      (mise-tasks-mode)
      (mise-tasks--refresh)
      (tabulated-list-print t))
    (pop-to-buffer buf)))

(defun mise-tasks-run (task &optional args)
  "Run TASK with optional ARGS."
  (interactive
   (let* ((task (or (mise-tasks--current-task)
                    (mise-core--completing-read
                     "Run task: "
                     (mapcar #'car (mise-tasks--list))
                     t)))
          (args (read-string (format "Args for %s (empty for none): " task))))
     (list task (unless (string-empty-p args) args))))
  (let* ((cmd (if args
                  (format "%s run %s -- %s"
                          (mise-core--executable) task args)
                (format "%s run %s"
                        (mise-core--executable) task)))
         (buf-name (format "*mise-run[%s]*" task))
         (default-directory (mise-core--default-directory)))
    (compilation-start cmd nil (lambda (_) buf-name))))

(defun mise-tasks-watch (task)
  "Watch TASK and re-run on file changes."
  (interactive
   (list (or (mise-tasks--current-task)
             (mise-core--completing-read
              "Watch task: "
              (mapcar #'car (mise-tasks--list))
              t))))
  (let* ((cmd (format "%s watch %s" (mise-core--executable) task))
         (buf-name (format "*mise-watch[%s]*" task))
         (default-directory (mise-core--default-directory)))
    (compilation-start cmd nil (lambda (_) buf-name))))

(defun mise-tasks-edit (task)
  "Edit TASK definition."
  (interactive
   (list (or (mise-tasks--current-task)
             (mise-core--completing-read
              "Edit task: "
              (mapcar #'car (mise-tasks--list))
              t))))
  (let* ((info (mise-core--run-json "tasks" "info" task "--json"))
         (source (alist-get 'source info)))
    (if (and source (file-exists-p source))
        (find-file source)
      (user-error "Cannot find source file for task %s" task))))

(defun mise-tasks-info (task)
  "Show info about TASK."
  (interactive
   (list (or (mise-tasks--current-task)
             (mise-core--completing-read
              "Task info: "
              (mapcar #'car (mise-tasks--list))
              t))))
  (let ((output (mise-core--run "tasks" "info" task)))
    (with-current-buffer (get-buffer-create "*mise-task-info*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert output))
      (special-mode)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

(transient-define-prefix mise-tasks-transient ()
  "Actions for mise tasks buffer."
  [:description "Task at point"
   ("r" "Run" mise-tasks-run)
   ("w" "Watch" mise-tasks-watch)
   ("e" "Edit" mise-tasks-edit)
   ("i" "Info" mise-tasks-info)])

(provide 'mise-tasks)
;;; mise-tasks.el ends here
