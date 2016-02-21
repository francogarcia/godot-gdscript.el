;;; company-godot-gdscript.el --- Company back-end for Godot GDScript completion.

;; Copyright (C) 2016 Franco Eusébio Garcia

;; Author: Franco Eusébio Garcia <francogarcia@protonmail.com>
;; URL: https://github.com/francogarcia/godot-gdscript.el
;; Version: 0.0.1
;; Keywords: godot game engine company

;;; License:

;; This file not shipped as part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Comentary:

;; This is a Company back-end to add auto-completion to Godot-GDScript mode.

;; Package-Requires: ((company "0.9.0"))

(defgroup company-godot-gdscript nil
  "Company back-end for Godot Engine GDScript Language completion."
  ;;:group 'programming
  :group 'godot-gdscript
  :version "24.3"
  :link '(emacs-commentary-link "godot-gdscript"))

;;; Code

(require 'cl-lib)
(require 'company)
(require 'json)

(defun company-godot-gdscript-find-project-configuration (&optional path)
  "Returns the path on which Godot's configuration
  file (\"engine.cfg\") is stored.

If PATH is given, starts searching by it. Otherwise, the search
starts by the current buffer path."
  ;; TODO: Handle error when project file does not exist.
  (let ((base-path (or path default-directory)))
    (locate-dominating-file base-path
                            (lambda (parent)
                              (directory-files parent t "engine.cfg")))))

(defun company-godot-gdscript-project-configuration-md5 (&optional path)
  "Returns the value of the MD5 check-sum of the project's
  configuration path.

If PATH is given, it is used as the leaf directory to search for
the configuration file. Otherwise, the search starts by the
current buffer's directory."
  ;; TODO: Handle error when project file does not exist.
  (md5 (directory-file-name
        (file-truename
          (company-godot-gdscript-find-project-configuration path)))))

(defun company-godot-gdscript-find-autocomplete-server-port (project-md5)
  "Find the server port of the GD Auto-Complete service by its
MD5 value, given by PROJECT-MD5."
  (let ((auto-complete-server-file "~/.godot/.autocomplete-servers.json"))
    (with-temp-buffer
      (insert-file-contents auto-complete-server-file)
      (let* (
             (json-object-type 'plist)
             (json-key-type 'string)
             (json-array-type 'list)
             (json-content-list nil)
             (json-content-list (json-read-from-string
                                 (buffer-substring-no-properties (point-min) (point-max)))))
        (lax-plist-get json-content-list project-md5)
        ))))

(defun company-godot-gdscript-build-json-request-at-point ()
  "Gather the required data to send to GD Auto-Complete Service,
  and pack them all into a JSON string.

The current line and column of the cursor are used as the point
on which to ask for completion."
  (let ((file-path buffer-file-name)
        (buffer-content (current-buffer))
        ;; TODO: Account for narrowing.
        (cursor-line (1- (line-number-at-pos)))
        (cursor-column (current-column))
        (meta-content "Request sent from Emacs Godot GDScript mode."))
    (with-current-buffer buffer-content
      (json-encode `(
                     :path ,file-path
                     :text ,(buffer-substring-no-properties (point-min) (point-max))
                     :cursor (:row ,cursor-line :column ,cursor-column)
                     :meta ,meta-content)))))

(defun company-godot-gdscript-build-json-request-at-point-verbose ()
  "Gather the required data to send to GD Auto-Complete Service,
  and pack them all into a JSON string.

The current line and column of the cursor are used as the point
on which to ask for completion."
  (let ((file-path buffer-file-name)
        (buffer-content (current-buffer))
        ;; TODO: Account for narrowing.
        (cursor-line (1- (line-number-at-pos)))
        (cursor-column (current-column))
        (meta-content "Request sent from Emacs Godot GDScript mode."))
    (progn
      (message "file: %s\nbuffer: %s\nline: %s\tcolumn: %s"
               file-path buffer-content cursor-line cursor-column)
      (with-current-buffer buffer-content
                        (json-encode `(
                                       :path ,file-path
                                       :text ,(buffer-substring-no-properties (point-min) (point-max))
                                       :cursor (:row ,cursor-line :column ,cursor-column)
                                       :meta ,meta-content))))))

(defun company-godot-gdscript-build-json-request-at-point-debug-version ()
  "Gather the required data to send to GD Auto-Complete Service,
  and pack them all into a JSON string.

The current line and column of the cursor are used as the point
on which to ask for completion."
  (let ((file-path "/home/franco/tmp/godot/emacs/example.gd")
        (buffer-content
;; "extends Node
;; func _ready():
;;     get_node("
"extends Node
func _ready():
    get_node(\""
)
        ;; (cursor-line 2)
        ;; (cursor-column 13)
        (cursor-line 2)
        (cursor-column 14)
        (meta-content "Request sent from Emacs Godot GDScript mode."))
    (json-encode `(:path ,file-path
                   :text ,buffer-content
                   :cursor (:row ,cursor-line :column ,cursor-column)
                   ;; Include source code here, as it is returned in
                   ;; response.
                   :meta ,buffer-content))))

(defun company-godot-gdscript-build-curl-command (url port json-request)
  "Build the shell command to invocate Curl. URL and PORT specify
the socket address, and JSON-REQUEST is a string containing the
data for requesting completion to GD Auto-Complete Service."
  ;;(let ((data (concat "--data \"" (company-godot-gdscript-escape-gdscript-symbols json-request) "\""))
  (let ((data (concat "--data-raw \"" (company-godot-gdscript-escape-gdscript-symbols json-request) "\""))
        (header-accept "--header 'Accept: application/json'")
        (header-connection "--header 'Connection: keep-alive'")
        (header-content-type "--header 'Content-Type: application/json; charset=UTF-8'")
        (http-version "--http1.1")
        (http-request (concat "--request POST " url ":" port))
        )
    (concat "curl "
            data " "
            header-accept " "
            header-connection " "
            header-content-type " "
            http-version " "
            http-request)))

(defun company-godot-gdscript-escape-gdscript-symbols (source)
  "Escape quotes in strings, in order to pass source code scripts
  to shells."
  ;; `json-enconde-string' escapes the string's literal quotes as well, so we
  ;; remove them using substring to remove the first and last 2 characters
  ;; (which contains '\"' on both extremes).
  (substring (json-encode-string source) 1 -1))

;; Adapted from: <https://github.com/deepakg/emacs/blob/master/perlysense/async-shell-command-to-string.el>
(defun company-godot-gdscript-async-shell-command (command buffer-name &optional callback)
  "Execute shell command COMMAND asynchronously in the
  background.

Return the temporary output buffer (named BUFFER-NAME), which
  command is writing to during execution.

If CALLBACK is supplied, it is called with the return value of
COMMAND passed as a string.

When the command is finished, call CALLBACK with the resulting
  output as a string.

Synopsis:
  (company-godot-async-shell-command-to-string \"echo hello\" \"Hello World\" (lambda (s) (message \"RETURNED (%s)\" s)))
"
  (lexical-let ((output-buffer (get-buffer-create buffer-name))
                (callback-function callback))
    (set-process-sentinel
     (start-process
      "Godot-GDScript Autocomplete"
      output-buffer
      shell-file-name
      ;; Command line arguments for the subprocess.
      shell-command-switch
      command)
     (lambda (process signal)
       ;; TODO: Handle failure.
       (when (memq (process-status process) '(exit signal))
         (if callback-function
             (with-current-buffer output-buffer
               (let ((output-string
                      (buffer-substring-no-properties (point-min) (point-max))))
                 (funcall callback-function output-string))))
         (kill-buffer output-buffer))))
    output-buffer))

(defun company-godot-gdscript-process-request-completion-at-point (callback)
  "Build and send the request for completion at the current point in buffer.

The request returns a JSON file containing the hint, suggestions,
and prefix offered by GD Auto-Complete Service, if any. The JSON
filled should be handled by the supplied CALLBACK function."
  (company-godot-gdscript-async-shell-command
   (company-godot-gdscript-build-curl-command "http://localhost"
                                              (company-godot-gdscript-find-autocomplete-server-port
                                               (company-godot-gdscript-project-configuration-md5 default-directory))
                                              (company-godot-gdscript-build-json-request-at-point))
   "*Godot-GDScript GD-AutoComplete Service*"
   callback))

(defun company-godot-gdscript-mode-extract-completion-hint-from-json (completion-json)
  "Extract and return a string containing the hint field of the
JSON completions."
  (let* ((json-object-type 'plist)
         (completion-data (json-read-from-string completion-json))
         (completion-hint (plist-get completion-data :hint)))
    completion-hint))

(defun company-godot-gdscript-mode-extract-completion-prefix-from-json (completion-json)
  "Extract and return the string containg prefix field of the JSON
completions."
  (let* ((json-object-type 'plist)
         (completion-data (json-read-from-string completion-json))
         (completion-prefix (plist-get completion-data :prefix)))
    completion-prefix))

(defun company-godot-gdscript-mode-extract-completion-suggestions-from-json (completion-json)
  "Extract and return a list containing the completion candidates
field of the JSON completions."
  (let* ((json-object-type 'plist)
         (completion-data (json-read-from-string completion-json))
         (completion-suggestions (coerce (plist-get completion-data :suggestions) 'list)))
    completion-suggestions))

(add-to-list 'company-backends 'company-godot-gdscript)

(defun company-godot-dscript-grab-symbol-before-quotes ()
  "Return the symbol before opening quotes, to search for path
  completions (such as node paths for the scene tree) inside
  Godot."
  ;; (company-grab-line "get_node(\\\"")
  ;; (company-grab-symbol)
  ;; Send an opening quote to search for candidates.
  (concat "\"" (company-grab-symbol))
  )

(defun company-godot-gdscript-prefix ()
  "Handle Company's prefix command case.

Only complete symbols when the current major mode is
Godot-GDScript.

For strings, it allows completion of code using this back-end and
any other Company back-ends.

For GDScript, if there is no symbol, it aborts the completion."
  (when (eq major-mode 'godot-gdscript-mode)
    (if (not (company-in-string-or-comment))
        ;; Handle source code.
        (or (company-grab-symbol) 'stop)
      ;; Handle strings, as they might be a call such as get_node(). Also allow
      ;; other back-ends to complete the string or comment.
      (or (company-godot-dscript-grab-symbol-before-quotes) 'nil))))

(defun company-godot-gdscript-candidates (callback)
  "Look for possible completion candidates for completion at
point, then updates Company list of candidates by calling
CALLBACK."
  (lexical-let ((callback-function callback))
   (company-godot-gdscript-process-request-completion-at-point
    (lambda (result)
      (funcall callback-function
               (company-godot-gdscript-mode-extract-completion-suggestions-from-json
                result))))))

(defun company-godot-gdscript-post-completion ()
  "Tweak the results of the completions."
  (save-excursion
    ;; Remove two double quotes in a row, if exists.
    (backward-char)
    (if (search-forward-regexp "\"\"" nil t)
        (replace-match "\""))))

;;;###autoload
(defun company-godot-gdscript (command &optional arg &rest ignored)
  "Godot-GDScript backend for company-mode.

See `company-backends' for more information regarding COMMAND and
ARG."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-godot-gdscript))
    (prefix (company-godot-gdscript-prefix))
    (candidates (cons :async
                      (lambda (company-async-callback)
                        (company-godot-gdscript-candidates
                         company-async-callback))))
    (post-completion (company-godot-gdscript-post-completion))
    (sorted t)))



(provide 'company-godot-gdscript)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; company-godot-gdscript.el ends here
