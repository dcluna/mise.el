;;; mise-tools.el --- Tool management for mise.el -*- lexical-binding: t; -*-

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

;; Tabulated-list interface for managing mise tools/runtimes.

;;; Code:

(require 'tabulated-list)
(require 'transient)
(require 'mise-core)

(defvar mise-tools--entries nil
  "Cached tool entries for the current buffer.")

(defun mise-tools--list ()
  "Fetch the list of tools from mise and return parsed entries.
Each entry is (ID [TOOL VERSION SOURCE STATUS REQUESTED])."
  (let ((tools (mise-core--run-json "ls" "--json"))
        (entries '()))
    (dolist (tool-group tools)
      (let ((tool-name (symbol-name (car tool-group)))
            (versions (cdr tool-group)))
        (dolist (ver versions)
          (let* ((version (or (alist-get 'version ver) ""))
                 (source (let ((src (alist-get 'source ver)))
                           (if src
                               (let ((path (or (alist-get 'path src) "")))
                                 (abbreviate-file-name path))
                             "")))
                 (active (if (alist-get 'active ver) "active" ""))
                 (requested (or (alist-get 'requested_version ver) ""))
                 (id (format "%s@%s" tool-name version)))
            (push (list id (vector tool-name version source active requested))
                  entries)))))
    (nreverse entries)))

(defun mise-tools--refresh ()
  "Refresh the tools list."
  (setq tabulated-list-entries (mise-tools--list)))

(defvar mise-tools-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "I") #'mise-tools-install)
    (define-key map (kbd "U") #'mise-tools-upgrade)
    (define-key map (kbd "D") #'mise-tools-uninstall)
    (define-key map (kbd "u") #'mise-tools-use)
    (define-key map (kbd "s") #'mise-tools-search)
    (define-key map (kbd "?") #'mise-tools-transient)
    map)
  "Keymap for `mise-tools-mode'.")

(define-derived-mode mise-tools-mode tabulated-list-mode "Mise Tools"
  "Major mode for listing mise tools."
  (setq tabulated-list-format [("Tool" 20 t)
                                ("Version" 15 t)
                                ("Source" 35 t)
                                ("Status" 10 t)
                                ("Requested" 15 t)]
        tabulated-list-padding 2
        tabulated-list-sort-key '("Tool" . nil))
  (add-hook 'tabulated-list-revert-hook #'mise-tools--refresh nil t)
  (hl-line-mode 1)
  (tabulated-list-init-header))

(defun mise-tools--current-tool ()
  "Return the tool name at point."
  (when-let ((entry (tabulated-list-get-entry)))
    (aref entry 0)))

(defun mise-tools--current-tool-version ()
  "Return the tool@version at point."
  (when-let ((entry (tabulated-list-get-entry)))
    (format "%s@%s" (aref entry 0) (aref entry 1))))

;;;###autoload (autoload 'mise-tools "mise-tools" nil t)
(transient-define-prefix mise-tools ()
  "Manage mise tools."
  :transient-suffix 'transient--do-return
  ["Actions"
   ("I" "Install" mise-tools-install)
   ("u" "Use" mise-tools-use)
   ("U" "Upgrade" mise-tools-upgrade)
   ("D" "Uninstall" mise-tools-uninstall)
   ("s" "Search registry" mise-tools-search)
   ("l" "List (open buffer)" mise-tools-list)])

;;;###autoload
(defun mise-tools-list ()
  "Display the mise tools buffer."
  (interactive)
  (let ((buf (get-buffer-create "*mise-tools*")))
    (with-current-buffer buf
      (mise-tools-mode)
      (mise-tools--refresh)
      (tabulated-list-print t))
    (pop-to-buffer buf)))

(defun mise-tools-install (tool)
  "Install TOOL (prompted if interactive)."
  (interactive
   (list (read-string "Install tool (e.g. node@20): ")))
  (message "Installing %s..." tool)
  (mise-core--run-async
   (lambda (buf)
     (kill-buffer buf)
     (message "Installed %s" tool)
     (when-let ((tools-buf (get-buffer "*mise-tools*")))
       (with-current-buffer tools-buf (revert-buffer))))
   "install" tool))

(defun mise-tools-use (tool)
  "Add TOOL to mise.toml and install it."
  (interactive
   (list (read-string "Use tool (e.g. node@20): ")))
  (message "Adding %s to mise.toml..." tool)
  (mise-core--run "use" tool)
  (message "Added %s" tool)
  (when-let ((buf (get-buffer "*mise-tools*")))
    (with-current-buffer buf (revert-buffer))))

(defun mise-tools-uninstall (tool)
  "Uninstall TOOL at point or prompted."
  (interactive
   (list (or (mise-tools--current-tool-version)
             (read-string "Uninstall tool (e.g. node@20.0.0): "))))
  (when (yes-or-no-p (format "Uninstall %s?" tool))
    (message "Uninstalling %s..." tool)
    (mise-core--run "uninstall" tool)
    (message "Uninstalled %s" tool)
    (when-let ((buf (get-buffer "*mise-tools*")))
      (with-current-buffer buf (revert-buffer)))))

(defun mise-tools-upgrade (&optional tool)
  "Upgrade TOOL at point, or all tools if none specified."
  (interactive
   (list (mise-tools--current-tool)))
  (if tool
      (progn
        (message "Upgrading %s..." tool)
        (mise-core--run "upgrade" tool)
        (message "Upgraded %s" tool))
    (message "Upgrading all tools...")
    (mise-core--run "upgrade")
    (message "Upgraded all tools"))
  (when-let ((buf (get-buffer "*mise-tools*")))
    (with-current-buffer buf (revert-buffer))))

(defun mise-tools-search (query)
  "Search the mise registry for QUERY."
  (interactive (list (read-string "Search registry: ")))
  (let ((output (mise-core--run "registry" query)))
    (with-current-buffer (get-buffer-create "*mise-search*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert output))
      (special-mode)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

(transient-define-prefix mise-tools-transient ()
  "Actions for mise tools buffer."
  [:description "Tool at point"
   ("I" "Install" mise-tools-install)
   ("u" "Use" mise-tools-use)
   ("U" "Upgrade" mise-tools-upgrade)
   ("D" "Uninstall" mise-tools-uninstall)
   ("s" "Search registry" mise-tools-search)])

(provide 'mise-tools)
;;; mise-tools.el ends here
