;;; mise-core.el --- Core utilities for mise.el -*- lexical-binding: t; -*-

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

;; Core process helpers and JSON parsing for mise.el.

;;; Code:

(require 'json)

(defgroup mise nil
  "Manage dev tools, env vars, and tasks with mise."
  :group 'tools
  :prefix "mise-")

(defcustom mise-executable "mise"
  "Path to the mise executable."
  :type 'string
  :group 'mise)

(defcustom mise-default-directory nil
  "Default directory for mise commands.
When nil, uses the current `default-directory'."
  :type '(choice (const :tag "Current directory" nil)
                 (directory :tag "Fixed directory"))
  :group 'mise)

(defun mise-core--executable ()
  "Return the path to the mise executable, or signal an error."
  (or (executable-find mise-executable)
      (user-error "Cannot find `%s' executable.  Is mise installed?" mise-executable)))

(defun mise-core--default-directory ()
  "Return the directory to run mise commands in."
  (or mise-default-directory default-directory))

(defun mise-core--run (&rest args)
  "Run mise with ARGS synchronously and return stdout as a string.
Signals an error if the process exits non-zero."
  (let ((exe (mise-core--executable)))
    (with-temp-buffer
      (let* ((default-directory (mise-core--default-directory))
             (exit-code (apply #'process-file exe nil (current-buffer) nil args)))
        (if (zerop exit-code)
            (buffer-string)
          (error "mise %s exited %d: %s"
                 (string-join args " ") exit-code (buffer-string)))))))

(defun mise-core--run-json (&rest args)
  "Run mise with ARGS and parse the JSON output.
Returns the parsed JSON as Emacs Lisp objects (alists/vectors)."
  (let ((output (apply #'mise-core--run args)))
    (if (string-empty-p (string-trim output))
        nil
      (json-parse-string output :object-type 'alist :array-type 'list))))

(defun mise-core--run-async (callback &rest args)
  "Run mise with ARGS asynchronously.
CALLBACK is called with the process buffer when the process exits
successfully, or an error is signaled on failure."
  (let* ((exe (mise-core--executable))
         (buf (generate-new-buffer " *mise-async*"))
         (default-directory (mise-core--default-directory))
         (proc (apply #'start-file-process "mise" buf exe args)))
    (set-process-sentinel
     proc
     (lambda (process _event)
       (when (memq (process-status process) '(exit signal))
         (if (zerop (process-exit-status process))
             (funcall callback (process-buffer process))
           (let ((output (with-current-buffer (process-buffer process)
                           (buffer-string))))
             (kill-buffer (process-buffer process))
             (error "mise %s failed: %s" (string-join args " ") output))))))
    proc))

(defun mise-core--completing-read (prompt collection &optional require-match)
  "Read from COLLECTION with PROMPT using `completing-read'.
REQUIRE-MATCH is passed through."
  (completing-read prompt collection nil require-match))

(provide 'mise-core)
;;; mise-core.el ends here
