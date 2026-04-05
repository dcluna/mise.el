;;; mise-config.el --- Config file management for mise.el -*- lexical-binding: t; -*-

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

;; Tabulated-list interface for managing mise config files.

;;; Code:

(require 'tabulated-list)
(require 'transient)
(require 'mise-core)

(defun mise-config--list ()
  "Fetch the list of config files from mise.
Returns entries suitable for `tabulated-list-entries'."
  (let ((configs (mise-core--run-json "config" "ls" "--json"))
        (entries '()))
    (dolist (cfg configs)
      (let* ((path (or (alist-get 'path cfg) ""))
             (tools (let ((ts (alist-get 'tools cfg)))
                      (if ts
                          (format "%d" (length ts))
                        "0")))
             (env-count (let ((ev (alist-get 'env cfg)))
                          (if ev
                              (format "%d" (length ev))
                            "0")))
             (plugins (let ((ps (alist-get 'plugins cfg)))
                        (if ps
                            (format "%d" (length ps))
                          "0"))))
        (push (list path (vector (abbreviate-file-name path) tools env-count plugins))
              entries)))
    (nreverse entries)))

(defun mise-config--refresh ()
  "Refresh the config list."
  (setq tabulated-list-entries (mise-config--list)))

(defvar mise-config-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'mise-config-open)
    (define-key map (kbd "t") #'mise-config-trust)
    (define-key map (kbd "?") #'mise-config-transient)
    map)
  "Keymap for `mise-config-mode'.")

(define-derived-mode mise-config-mode tabulated-list-mode "Mise Config"
  "Major mode for listing mise config files."
  (setq tabulated-list-format [("Path" 55 t)
                                ("Tools" 8 t)
                                ("Env Vars" 10 t)
                                ("Plugins" 10 t)]
        tabulated-list-padding 2
        tabulated-list-sort-key '("Path" . nil))
  (add-hook 'tabulated-list-revert-hook #'mise-config--refresh nil t)
  (hl-line-mode 1)
  (tabulated-list-init-header))

(defun mise-config--current-path ()
  "Return the config file path at point."
  (tabulated-list-get-id))

;;;###autoload (autoload 'mise-config "mise-config" nil t)
(transient-define-prefix mise-config ()
  "Manage mise config files."
  :transient-suffix 'transient--do-return
  ["Actions"
   ("o" "Open" mise-config-open)
   ("t" "Trust" mise-config-trust)
   ("l" "List (open buffer)" mise-config-list)])

;;;###autoload
(defun mise-config-list ()
  "Display the mise config files buffer."
  (interactive)
  (let ((buf (get-buffer-create "*mise-config*")))
    (with-current-buffer buf
      (mise-config-mode)
      (mise-config--refresh)
      (tabulated-list-print t))
    (pop-to-buffer buf)))

(defun mise-config-open (path)
  "Open the config file at PATH."
  (interactive
   (list (or (mise-config--current-path)
             (read-file-name "Open config: "))))
  (find-file path))

(defun mise-config-trust (path)
  "Trust the config file at PATH."
  (interactive
   (list (or (mise-config--current-path)
             (read-file-name "Trust config: "))))
  (mise-core--run "trust" path)
  (message "Trusted %s" path))

(transient-define-prefix mise-config-transient ()
  "Actions for mise config buffer."
  [:description "Config at point"
   ("o" "Open" mise-config-open)
   ("t" "Trust" mise-config-trust)])

(provide 'mise-config)
;;; mise-config.el ends here
