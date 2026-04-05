;;; mise-env.el --- Environment management for mise.el -*- lexical-binding: t; -*-

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

;; Two-part environment support for mise:
;;
;; 1. Interactive: A tabulated-list buffer for viewing/editing env vars
;; 2. Automatic: A global minor mode that sets buffer-local
;;    `process-environment' and `exec-path' based on `mise env'.

;;; Code:

(require 'tabulated-list)
(require 'transient)
(require 'mise-core)

;;; --- Interactive env var listing ---

(defun mise-env--list ()
  "Fetch environment variables from mise.
Returns entries suitable for `tabulated-list-entries'."
  (let ((env (mise-core--run-json "env" "--json-extended"))
        (entries '()))
    (dolist (var env)
      (let* ((name (or (alist-get 'name var) (symbol-name (car var)) ""))
             (value (or (alist-get 'value var) ""))
             (source (let ((src (alist-get 'source var)))
                       (if (stringp src)
                           (abbreviate-file-name src)
                         (or src "")))))
        (push (list name (vector name (format "%s" value) (format "%s" source)))
              entries)))
    (nreverse entries)))

(defun mise-env--refresh ()
  "Refresh the env var list."
  (setq tabulated-list-entries (mise-env--list)))

(defvar mise-env-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s") #'mise-env-set)
    (define-key map (kbd "D") #'mise-env-unset)
    (define-key map (kbd "?") #'mise-env-transient)
    map)
  "Keymap for `mise-env-mode'.")

(define-derived-mode mise-env-mode tabulated-list-mode "Mise Env"
  "Major mode for listing mise environment variables."
  (setq tabulated-list-format [("Variable" 30 t)
                                ("Value" 50 t)
                                ("Source" 35 t)]
        tabulated-list-padding 2
        tabulated-list-sort-key '("Variable" . nil))
  (add-hook 'tabulated-list-revert-hook #'mise-env--refresh nil t)
  (hl-line-mode 1)
  (tabulated-list-init-header))

(defun mise-env--current-var ()
  "Return the variable name at point."
  (when-let ((entry (tabulated-list-get-entry)))
    (aref entry 0)))

;;;###autoload (autoload 'mise-env "mise-env" nil t)
(transient-define-prefix mise-env ()
  "Manage mise environment variables."
  :transient-suffix 'transient--do-return
  ["Actions"
   ("s" "Set" mise-env-set)
   ("D" "Unset" mise-env-unset)
   ("l" "List (open buffer)" mise-env-list)])

;;;###autoload
(defun mise-env-list ()
  "Display the mise environment variables buffer."
  (interactive)
  (let ((buf (get-buffer-create "*mise-env*")))
    (with-current-buffer buf
      (mise-env-mode)
      (mise-env--refresh)
      (tabulated-list-print t))
    (pop-to-buffer buf)))

(defun mise-env-set (name value)
  "Set env var NAME to VALUE in mise.toml."
  (interactive
   (let ((name (or (mise-env--current-var)
                   (read-string "Variable name: ")))
         (value (read-string "Value: ")))
     (list name value)))
  (mise-core--run "set" name value)
  (message "Set %s=%s" name value)
  (when-let ((buf (get-buffer "*mise-env*")))
    (with-current-buffer buf (revert-buffer))))

(defun mise-env-unset (name)
  "Remove env var NAME from mise.toml."
  (interactive
   (list (or (mise-env--current-var)
             (read-string "Variable name: "))))
  (when (yes-or-no-p (format "Unset %s?" name))
    (mise-core--run "unset" name)
    (message "Unset %s" name)
    (when-let ((buf (get-buffer "*mise-env*")))
      (with-current-buffer buf (revert-buffer)))))

(transient-define-prefix mise-env-transient ()
  "Actions for mise env buffer."
  [:description "Env var at point"
   ("s" "Set" mise-env-set)
   ("D" "Unset" mise-env-unset)])

;;; --- Automatic buffer-local environment (global minor mode) ---

(defvar-local mise-env--local-env nil
  "The mise environment applied to this buffer, or nil.")

(defvar mise-env--cache (make-hash-table :test 'equal)
  "Cache of mise environments keyed by directory.
Values are (MTIME . ENV-ALIST) where ENV-ALIST is parsed `mise env --json'.")

(defcustom mise-env-lighter " mise"
  "Mode-line lighter for `global-mise-env-mode'."
  :type 'string
  :group 'mise)

(defun mise-env--config-files (dir)
  "Return a list of mise config files found in DIR and parent dirs."
  (let ((files '())
        (names '("mise.toml" ".mise.toml" "mise.local.toml"
                 ".mise.local.toml" ".tool-versions")))
    (let ((d (expand-file-name dir)))
      (while (and d (not (string= d (file-name-directory (directory-file-name d)))))
        (dolist (name names)
          (let ((f (expand-file-name name d)))
            (when (file-exists-p f)
              (push f files))))
        (setq d (file-name-directory (directory-file-name d)))))
    files))

(defun mise-env--cache-key (dir)
  "Return a cache key for DIR based on config file modification times."
  (let ((configs (mise-env--config-files dir)))
    (cons dir (mapcar (lambda (f)
                        (cons f (file-attribute-modification-time
                                 (file-attributes f))))
                      configs))))

(defun mise-env--get-env (dir)
  "Get the mise environment for DIR, using cache when possible."
  (let* ((key (mise-env--cache-key dir))
         (cached (gethash (car key) mise-env--cache)))
    (if (and cached (equal (car cached) (cdr key)))
        (cdr cached)
      (condition-case _err
          (let* ((default-directory dir)
                 (env (mise-core--run-json "env" "--json")))
            (puthash (car key) (cons (cdr key) env) mise-env--cache)
            env)
        (error
         (puthash (car key) (cons (cdr key) nil) mise-env--cache)
         nil)))))

(defun mise-env--apply ()
  "Apply mise environment to the current buffer."
  (when (and (not mise-env--local-env)
             (buffer-file-name)
             (not (minibufferp))
             (not (string-prefix-p " " (buffer-name))))
    (let ((env (mise-env--get-env default-directory)))
      (when env
        (setq-local mise-env--local-env t)
        (let ((new-env (copy-sequence process-environment))
              (new-path (copy-sequence exec-path)))
          (dolist (entry env)
            (let ((name (symbol-name (car entry)))
                  (value (format "%s" (cdr entry))))
              (setq new-env (cons (format "%s=%s" name value)
                                  (seq-remove (lambda (e)
                                                (string-prefix-p (concat name "=") e))
                                              new-env)))
              (when (string= name "PATH")
                (setq new-path (parse-colon-path value)))))
          (setq-local process-environment new-env)
          (setq-local exec-path new-path))))))

(defun mise-env--setup-buffer ()
  "Hook function to set up mise environment for a buffer."
  (when (bound-and-true-p global-mise-env-mode)
    (mise-env--apply)))

;;;###autoload
(define-minor-mode global-mise-env-mode
  "Global minor mode to automatically set buffer-local mise environments."
  :global t
  :lighter mise-env-lighter
  :group 'mise
  (if global-mise-env-mode
      (progn
        (add-hook 'find-file-hook #'mise-env--setup-buffer)
        (add-hook 'after-change-major-mode-hook #'mise-env--setup-buffer))
    (remove-hook 'find-file-hook #'mise-env--setup-buffer)
    (remove-hook 'after-change-major-mode-hook #'mise-env--setup-buffer)))

(defun mise-env-clear-cache ()
  "Clear the mise environment cache."
  (interactive)
  (clrhash mise-env--cache)
  (message "Mise environment cache cleared"))

(provide 'mise-env)
;;; mise-env.el ends here
