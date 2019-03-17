;;; godot-gdscript.el --- Major mode for editing Godot Engine GDScript files

;; Original code Python Mode (from `python.el'):
;; Copyright (C) 2003--2015 Free Software Foundation, Inc.
;; Godot-GDScript Mode:
;; Copyright (C) 2015--2017 Franco Eusébio Garcia

;; Author: Franco Eusébio Garcia <francogarcia@protonmail.com>
;; URL: https://github.com/francogarcia/godot-gdscript.el
;; Version: 0.0.1
;; Keywords: languages

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

;;; Commentary:

;; This is a draft to add support for GDScript in Emacs. GDScript is the
;; language which Godot Game Engine uses to prototype and implement games. Godot
;; is an open-source game engine, available at: <http://www.godotengine.org/>.

;; The mode uses Fabián E. Gallina's `python.el' as the basis and reference for
;; the implementation, due to the similarities between GDScript and Python
;; syntax. However, as some keywords and operators do differ, `python-mode' is
;; not derived; instead, its code is changed to support the GDScript language.

;; Package-Requires: ((emacs "24.3"))

;;; Code:

(require 'ansi-color)
(require 'cl-lib)
(require 'comint)
(require 'json)

;; Avoid compiler warnings (disable due to package-lint-current-buffer).
;; (defvar view-return-to-alist)
;; (defvar compilation-error-regexp-alist)
;; (defvar outline-heading-end-regexp)
;; (defvar ffap-alist)
;; (defvar electric-indent-inhibit)
;; (autoload 'comint-mode "comint")

;;;###autoload
(add-to-list 'auto-mode-alist (cons (purecopy "\\.gd\\'")  'godot-gdscript-mode))
;;;###autoload
(add-to-list 'interpreter-mode-alist (cons (purecopy "godot-gdscript[0-9.]*") 'godot-gdscript-mode))

(defgroup godot-gdscript nil
  "Godot Engine GDScript Language support for developing games using Emacs."
  :group 'languages
  :version "24.3"
  :link '(emacs-commentary-link "godot-gdscript"))

;;; Bindings

(defvar godot-gdscript-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Movement
    (define-key map [remap backward-sentence] 'godot-gdscript-nav-backward-block)
    (define-key map [remap forward-sentence] 'godot-gdscript-nav-forward-block)
    (define-key map [remap backward-up-list] 'godot-gdscript-nav-backward-up-list)
    (define-key map "\C-c\C-j" 'imenu)
    ;; Indent specific
    (define-key map "\177" 'godot-gdscript-indent-dedent-line-backspace)
    (define-key map (kbd "<backtab>") 'godot-gdscript-indent-dedent-line)
    (define-key map "\C-c<" 'godot-gdscript-indent-shift-left)
    (define-key map "\C-c>" 'godot-gdscript-indent-shift-right)
    ;; Skeletons
    (define-key map "\C-c\C-tc" 'godot-gdscript-skeleton-class)
    (define-key map "\C-c\C-td" 'godot-gdscript-skeleton-def)
    (define-key map "\C-c\C-tf" 'godot-gdscript-skeleton-for)
    (define-key map "\C-c\C-ti" 'godot-gdscript-skeleton-if)
    (define-key map "\C-c\C-tt" 'godot-gdscript-skeleton-try)
    (define-key map "\C-c\C-tw" 'godot-gdscript-skeleton-while)
    ;; Shell interaction
    (define-key map "\C-c\C-g" 'godot-gdscript-run-godot-editor)
    (define-key map "\C-c\C-p" 'godot-gdscript-run-project-in-godot)
    (define-key map "\C-c\C-s" 'godot-gdscript-run-current-scene-in-godot)
    (define-key map "\C-c\C-e" 'godot-gdscript-edit-current-scene-in-godot)
    (define-key map "\C-c\C-r" 'godot-gdscript-run-current-script-in-godot)
    (define-key map "\C-c\C-dp" 'godot-gdscript-run-project-in-godot-debug-mode)
    (define-key map "\C-c\C-ds" 'godot-gdscript-run-current-scene-in-godot-debug-mode)
    (define-key map "\C-c\C-z" 'godot-gdscript-shell-switch-to-shell)
    ;; Some util commands
    (define-key map "\C-c\C-v" 'godot-gdscript-check)
    ;; Utilities
    (substitute-key-definition 'complete-symbol 'completion-at-point
                               map global-map)
    (easy-menu-define godot-gdscript-menu map "Godot-Gdscript Mode menu"
      `("Godot-Gdscript"
        :help "Godot-Gdscript-specific Features"
        ["Shift region left" godot-gdscript-indent-shift-left :active mark-active
         :help "Shift region left by a single indentation step"]
        ["Shift region right" godot-gdscript-indent-shift-right :active mark-active
         :help "Shift region right by a single indentation step"]
        "-"
        ["Start of def/class" beginning-of-defun
         :help "Go to start of outermost definition around point"]
        ["End of def/class" end-of-defun
         :help "Go to end of definition around point"]
        ["Mark def/class" mark-defun
         :help "Mark outermost definition around point"]
        ["Jump to def/class" imenu
         :help "Jump to a class or function definition"]
        "--"
        ("Skeletons")
        "---"
        ["Switch to shell" godot-gdscript-shell-switch-to-shell
         :help "Switch to running inferior Godot-Gdscript process"]
        ["Run Godot Engine editor" godot-gdscript-run-godot-editor
         :help "Run Godot Editor as a subprocess of Emacs, loading this project into it"]
        ["Run project" godot-gdscript-run-project-in-godot
         :help "Run the current project in Godot Engine"]
        ["Run current scene" godot-gdscript-run-current-scene-in-godot
         :help "Run the current scene in Godot Engine"]
        ["Edit scene in Godot Engine" godot-gdscript-edit-current-scene-in-godot
         :help "Edit the current scene in Godot Engine"]
        ["Run current script" godot-gdscript-run-current-script-in-godot
         :help "Run the current script in Godot Engine"]
        ["Run project (debug mode)" godot-gdscript-run-project-in-godot-debug-mode
         :help "Run the project in Godot Engine, using the debug mode option"]
        ["Run scene (debug mode)" godot-gdscript-run-current-scene-in-godot-debug-mode
         :help "Run the current scene in Godot Engine, using the debug mode option"]
        "----"
        ["Check file" godot-gdscript-check
         :help "Check file for errors"]
        ["Complete symbol" completion-at-point
         :help "Complete symbol before point"]))
    map)
  "Keymap for function `godot-gdscript-mode'.")





;;; Godot-Gdscript specialized rx

(eval-when-compile
  (defconst godot-gdscript-rx-constituents
    `((block-start          . ,(rx symbol-start
                                   (or "class" "elif" "else" "except" "finally" "for"
                                       "func" "if" "try" "while" "with"
                                       ;; Multiplayer stuff
                                       "puppet" "master" "remote" "remotesync")
                                   symbol-end))
      (dedenter            . ,(rx symbol-start
                                  (or "elif" "else" "except" "finally")
                                  symbol-end))
      (block-ender         . ,(rx symbol-start
                                  (or
                                   "break" "continue" "pass" "raise" "return")
                                  symbol-end))
      (decorator            . ,(rx line-start
                                   (* space) ?@ (any letter ?_)
                                   (* (any word ?_))))
      (defun                . ,(rx symbol-start (or "func" "class") symbol-end))
      (if-name-main         . ,(rx line-start "if" (+ space) "__name__"
                                   (+ space) "==" (+ space)
                                   (any ?' ?\") "__main__" (any ?' ?\")
                                   (* space) ?:))
      (symbol-name          . ,(rx (any letter ?_) (* (any word ?_))))
      (variable-declaration . ,(rx (or "const" "var")))
      (open-paren           . ,(rx (or "{" "[" "(")))
      (close-paren          . ,(rx (or "}" "]" ")")))
      (simple-operator      . ,(rx (any ?+ ?- ?/ ?& ?^ ?~ ?| ?* ?< ?> ?= ?%)))
      ;; FIXME: rx should support (not simple-operator).
      (not-simple-operator  . ,(rx
                                (not
                                 (any ?+ ?- ?/ ?& ?^ ?~ ?| ?* ?< ?> ?= ?%))))
      ;; FIXME: Use regexp-opt.
      (operator             . ,(rx (or "+" "-" "/" "&" "^" "~" "|" "*" "<" ">"
                                       "=" "%" "//" "<<" ">>" "<=" "!" "!="
                                       "==" ">=" "||" "&&" "is" "not")))
      ;; FIXME: Use regexp-opt.
      (assignment-operator  . ,(rx (or "=" "+=" "-=" "*=" "/=" "//=" "%="
                                       ">>=" "<<=" "&=" "^=" "|=")))
      (string-delimiter . ,(rx (and
                                ;; Match even number of backslashes.
                                (or (not (any ?\\ ?\' ?\")) point
                                    ;; Quotes might be preceded by a escaped quote.
                                    (and (or (not (any ?\\)) point) ?\\
                                         (* ?\\ ?\\) (any ?\' ?\")))
                                (* ?\\ ?\\)
                                ;; Match single or triple quotes of any kind.
                                (group (or  "\"" "\"\"\"" "'" "'''")))))
      (coding-cookie . ,(rx line-start ?# (* space)
                            (or
                             ;; # coding=<encoding name>
                             (: "coding" (or ?: ?=) (* space) (group-n 1 (+ (or word ?-))))
                             ;; # -*- coding: <encoding name> -*-
                             (: "-*-" (* space) "coding:" (* space)
                                (group-n 1 (+ (or word ?-))) (* space) "-*-")))))
    "Additional Godot-Gdscript specific sexps for `godot-gdscript-rx'")

  (defmacro godot-gdscript-rx (&rest regexps)
    "Godot-Gdscript mode specialized rx macro.
This variant of `rx' supports common Godot-Gdscript named REGEXPS."
    (let ((rx-constituents (append godot-gdscript-rx-constituents rx-constituents)))
      (cond ((null regexps)
             (error "No regexp"))
            ((cdr regexps)
             (rx-to-string `(and ,@regexps) t))
            (t
             (rx-to-string (car regexps) t))))))


;;; Font-lock and syntax

(eval-when-compile
  (defun godot-gdscript-syntax--context-compiler-macro (form type &optional syntax-ppss)
    (pcase type
      (`'comment
       `(let ((ppss (or ,syntax-ppss (syntax-ppss))))
          (and (nth 4 ppss) (nth 8 ppss))))
      (`'string
       `(let ((ppss (or ,syntax-ppss (syntax-ppss))))
          (and (nth 3 ppss) (nth 8 ppss))))
      (`'paren
       `(nth 1 (or ,syntax-ppss (syntax-ppss))))
      (_ form))))

(defun godot-gdscript-syntax-context (type &optional syntax-ppss)
  "Return non-nil if point is on TYPE using SYNTAX-PPSS.
TYPE can be `comment', `string' or `paren'.  It returns the start
character address of the specified TYPE."
  (declare (compiler-macro godot-gdscript-syntax--context-compiler-macro))
  (let ((ppss (or syntax-ppss (syntax-ppss))))
    (pcase type
      (`comment (and (nth 4 ppss) (nth 8 ppss)))
      (`string (and (nth 3 ppss) (nth 8 ppss)))
      (`paren (nth 1 ppss))
      (_ nil))))

(defun godot-gdscript-syntax-context-type (&optional syntax-ppss)
  "Return the context type using SYNTAX-PPSS.
The type returned can be `comment', `string' or `paren'."
  (let ((ppss (or syntax-ppss (syntax-ppss))))
    (cond
     ((nth 8 ppss) (if (nth 4 ppss) 'comment 'string))
     ((nth 1 ppss) 'paren))))

(defsubst godot-gdscript-syntax-comment-or-string-p (&optional ppss)
  "Return non-nil if PPSS is inside 'comment or 'string."
  (nth 8 (or ppss (syntax-ppss))))

(defsubst godot-gdscript-syntax-closing-paren-p ()
  "Return non-nil if char after point is a closing paren."
  (= (syntax-class (syntax-after (point)))
     (syntax-class (string-to-syntax ")"))))

(define-obsolete-function-alias
  'godot-gdscript-info-ppss-context #'godot-gdscript-syntax-context "24.3")

(define-obsolete-function-alias
  'godot-gdscript-info-ppss-context-type #'godot-gdscript-syntax-context-type "24.3")

(define-obsolete-function-alias
  'godot-gdscript-info-ppss-comment-or-string-p
  #'godot-gdscript-syntax-comment-or-string-p "24.3")

(defvar godot-gdscript-font-lock-keywords
  ;; Keywords
  `(,(rx symbol-start
         (or
          "and" "in" "is" "not" "or"
          "null" "self"
          "String" "bool" "float" "int"
          ;; Functions
          ;; Variant types
          "AABB" "Array" "Basis" "ByteArray" "Color"
          "ColorArray" "Dictionary" "Image" "InputEvent" "IntArray"
          "Matrix3" "Matrix32" "NodePath" "Object" "Plane"
          "Quat" "RID" "RealArray" "Rect2" "StringArray"
          "Transform" "Vector2" "Vector2Array" "Vector3" "Vector3Array"
          ;; Language keywords
          "assert" "break" "breakpoint" "class" "const" "continue"
          "default" "do" "elif" "else" "enum"
          "export" "extends" "for" "func" "if" "onready"
          ;; Multiplayer stuff
          "puppet" "master" "remote" "remotesync"
          "pass" "preload" "resume" "return" "setget"
          "signal" "static" "tool" "var" "while" "yield")
         symbol-end)
    ;; functions
    (,(rx symbol-start "func" (1+ space) (group (1+ (or word ?_))))
     (1 font-lock-function-name-face))
    ;; classes
    (,(rx symbol-start "class" (1+ space) (group (1+ (or word ?_))))
     (1 font-lock-type-face))
    ;; Constants
    (,(rx symbol-start
          (or
           "PI" "false" "null" "true")
          symbol-end) . font-lock-constant-face)
    ;; Decorators.
    (,(rx line-start (* (any " \t")) (group "@" (1+ (or word ?_))
                                            (0+ "." (1+ (or word ?_)))))
     (1 font-lock-type-face))
    ;; Builtin Exceptions
    (,(rx symbol-start
          (or
           "OK" "FAILED"
           "ERR_UNAVAILABLE" "ERR_UNCONFIGURED" "ERR_UNAUTHORIZED"
           "ERR_PARAMETER_RANGE_ERROR" "ERR_OUT_OF_MEMORY" "ERR_FILE_NOT_FOUND"
           "ERR_FILE_BAD_DRIVE" "ERR_FILE_BAD_PATH" "ERR_FILE_NO_PERMISSION"
           "ERR_FILE_ALREADY_IN_USE" "ERR_FILE_CANT_OPEN" "ERR_FILE_CANT_WRITE"
           "ERR_FILE_CANT_READ" "ERR_FILE_UNRECOGNIZED" "ERR_FILE_CORRUPT"
           "ERR_FILE_MISSING_DEPENDENCIES" "ERR_FILE_EOF" "ERR_CANT_OPEN"
           "ERR_CANT_CREATE" "ERROR_QUERY_FAILED" "ERR_ALREADY_IN_USE"
           "ERR_LOCKED" "ERR_TIMEOUT" "ERR_CANT_CONNECT" "ERR_CANT_RESOLVE"
           "ERR_CONNECTION_ERROR" "ERR_CANT_AQUIRE_RESOURCE" "ERR_CANT_FORK"
           "ERR_INVALID_DATA" "ERR_INVALID_PARAMETER" "ERR_ALREADY_EXISTS"
           "ERR_DOES_NOT_EXIST" "ERR_DATABASE_CANT_READ" "ERR_DATABASE_CANT_WRITE"
           "ERR_COMPILATION_FAILED" "ERR_METHOD_NOT_FOUND" "ERR_LINK_FAILED"
           "ERR_SCRIPT_FAILED" "ERR_CYCLIC_LINK" "ERR_INVALID_DECLARATION"
           "ERR_DUPLICATE_SYMBOL" "ERR_PARSE_ERROR" "ERR_BUSY"
           "ERR_SKIP" "ERR_HELP" "ERR_BUG" "ERR_PRINTER_ON_FIRE"
           "ERR_OMFG_THIS_IS_VERY_VERY_BAD" "ERR_WTF")
          symbol-end) . font-lock-type-face)
    ;; Builtins
    (,(rx symbol-start
          (or
           ;; Inherited methods from Object
           "connect" "emit" "get" "set_signal"
           ;; Inherited methods from Node
           ;; get_node() shorthand
           ;; <https://github.com/godotengine/godot/issues/4309>
           "$"
           ;; (<https://github.com/godotengine/godot/blob/master/scene/scene_string_names.h>)
           "_draw" "_enter_tree" "_enter_world" "_exit_tree" "_exit_world"
           "_fixed_process" "_init" "_input" "_input" "_physics_process"
           "_process" "_process" "_ready" "_unhandled_input"
           "_unhandled_input" "_unhandled_key_input" "_unhandled_key_input"
           ;; (<https://github.com/godotengine/godot/blob/master/scene/main/node.cpp>)
           "add_child" "add_to_group" "can_process" "duplicate" "find_node"
           "get_child" "get_child_count" "get_children" "get_filename"
           "get_groups" "get_index" "get_name" "get_node"
           "get_node_and_resource" "get_owner" "get_parent" "get_path"
           "get_path_to" "get_pause_mode" "get_physics_process_delta_time"
           "get_position_in_parent" "get_process_delta_time"
           "get_scene_instance_load_placeholder" "get_tree" "get_viewport"
           "has_node" "has_node_and_resource" "is_a_parent_of"
           "is_displayed_folded" "is_greater_than" "is_in_group"
           "is_inside_tree" "is_physics_processing"
           "is_physics_processing_internal" "is_processing"
           "is_processing_input" "is_processing_internal"
           "is_processing_unhandled_input"
           "is_processing_unhandled_key_input"
           "move_child" "print_stray_nodes" "print_tree" "print_tree_pretty"
           "propagate_call" "propagate_notification" "queue_free" "raise"
           "remove_and_skip" "remove_child" "remove_from_group" "replace_by"
           "request_ready" "set_display_folded" "set_filename" "set_name"
           "set_owner" "set_pause_mode" "set_physics_process"
           "set_physics_process_internal" "set_process" "set_process_input"
           "set_process_internal" "set_process_unhandled_input"
           "set_process_unhandled_key_input"
           "set_scene_instance_load_placeholder"
           ;; Missing functions from header
           "basefunc" "call" "new" "instance"
           ;; Exported functions
           ;; (<https://github.com/godotengine/godot/blob/master/modules/gdscript/gdscript_functions.cpp>)
           "Color8" "ColorN" "abs" "acos" "asin" "atan" "atan2" "bytes2var"
           "cartesian2polar" "ceil" "char" "clamp" "convert" "cos" "cosh"
           "db2linear" "decimals" "dectime" "deg2rad" "dict2inst" "ease"
           "exp" "floor" "fmod" "fposmod" "funcref" "hash" "inst2dict"
           "instance_from_id" "inverse_lerp" "is_inf" "is_instance_valid"
           "is_nan" "len" "lerp" "linear2db" "load" "log" "max" "min"
           "nearest_po2" "parse_json" "polar2cartesian" "pow" "print"
           "print_stack" "printerr" "printraw" "prints" "printt" "rad2deg"
           "rand_range" "rand_seed" "randf" "randi" "randomize" "range"
           "range_lerp" "round" "seed" "sign" "sinh" "sqrt" "stepify" "str"
           "str2var" "tan" "tanh" "to_json" "type_exists" "typeof"
           "validate_json" "var2bytes" "var2str" "weakref" "wrapf" "wrapi"
           "sin")
          symbol-end) . font-lock-builtin-face)
    ;; assignments
    ;; support for a = b = c = 5
    (,(lambda (limit)
        (let ((re (godot-gdscript-rx (group (+ (any word ?. ?_)))
                                     (? ?\[ (+ (not (any  ?\]))) ?\]) (* space)
                                     assignment-operator))
              (res nil))
          (while (and (setq res (re-search-forward re limit t))
                      (or (godot-gdscript-syntax-context 'paren)
                          (equal (char-after (point-marker)) ?=))))
          res))
     (1 font-lock-variable-name-face nil nil))
    ;; support for a, b, c = (1, 2, 3)
    (,(lambda (limit)
        (let ((re (godot-gdscript-rx (group (+ (any word ?. ?_))) (* space)
                                     (* ?, (* space) (+ (any word ?. ?_)) (* space))
                                     ?, (* space) (+ (any word ?. ?_)) (* space)
                                     assignment-operator))
              (res nil))
          (while (and (setq res (re-search-forward re limit t))
                      (goto-char (match-end 1))
                      (godot-gdscript-syntax-context 'paren)))
          res))
     (1 font-lock-variable-name-face nil nil))))

(defconst godot-gdscript-syntax-propertize-function
  (syntax-propertize-rules
   ((godot-gdscript-rx string-delimiter)
    (0 (ignore (godot-gdscript-syntax-stringify))))))

(defsubst godot-gdscript-syntax-count-quotes (quote-char &optional point limit)
  "Count number of quotes around point (max is 3).
QUOTE-CHAR is the quote char to count. Optional argument POINT is
the point where scan starts (defaults to current point), and
LIMIT is used to limit the scan."
  (let ((i 0))
    (while (and (< i 3)
                (or (not limit) (< (+ point i) limit))
                (eq (char-after (+ point i)) quote-char))
      (setq i (1+ i)))
    i))

(defun godot-gdscript-syntax-stringify ()
  "Put `syntax-table' property correctly on single/triple quotes."
  (let* ((num-quotes (length (match-string-no-properties 1)))
         (ppss (prog2
                   (backward-char num-quotes)
                   (syntax-ppss)
                 (forward-char num-quotes)))
         (string-start (and (not (nth 4 ppss)) (nth 8 ppss)))
         (quote-starting-pos (- (point) num-quotes))
         (quote-ending-pos (point))
         (num-closing-quotes
          (and string-start
               (godot-gdscript-syntax-count-quotes
                (char-before) string-start quote-starting-pos))))
    (cond ((and string-start (= num-closing-quotes 0))
           ;; This set of quotes doesn't match the string starting
           ;; kind. Do nothing.
           nil)
          ((not string-start)
           ;; This set of quotes delimit the start of a string.
           (put-text-property quote-starting-pos (1+ quote-starting-pos)
                              'syntax-table (string-to-syntax "|")))
          ((= num-quotes num-closing-quotes)
           ;; This set of quotes delimit the end of a string.
           (put-text-property (1- quote-ending-pos) quote-ending-pos
                              'syntax-table (string-to-syntax "|")))
          ((> num-quotes num-closing-quotes)
           ;; This may only happen whenever a triple quote is closing
           ;; a single quoted string. Add string delimiter syntax to
           ;; all three quotes.
           (put-text-property quote-starting-pos quote-ending-pos
                              'syntax-table (string-to-syntax "|"))))))

(defvar godot-gdscript-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Give punctuation syntax to ASCII that normally has symbol
    ;; syntax or has word syntax and isn't a letter.
    (let ((symbol (string-to-syntax "_"))
          (sst (standard-syntax-table)))
      (dotimes (i 128)
        (unless (= i ?_)
          (if (equal symbol (aref sst i))
              (modify-syntax-entry i "." table)))))
    (modify-syntax-entry ?$ "." table)
    (modify-syntax-entry ?% "." table)
    ;; exceptions
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?` "$" table)
    table)
  "Syntax table for Godot-Gdscript files.")

(defvar godot-gdscript-dotty-syntax-table
  (let ((table (make-syntax-table godot-gdscript-mode-syntax-table)))
    (modify-syntax-entry ?. "w" table)
    (modify-syntax-entry ?_ "w" table)
    table)
  "Dotty syntax table for Godot-Gdscript files.
It makes underscores and dots word constituent chars.")

;;; Indentation

(defcustom godot-gdscript-indent-offset 4
  "Default indentation offset for Godot-Gdscript."
  :group 'godot-gdscript
  :type 'integer
  :safe 'integerp)

(defcustom godot-gdscript-indent-guess-indent-offset t
  "Non-nil tells Godot-Gdscript mode to guess `godot-gdscript-indent-offset' value."
  :type 'boolean
  :group 'godot-gdscript
  :safe 'booleanp)

(defcustom godot-gdscript-indent-trigger-commands
  '(indent-for-tab-command yas-expand yas/expand)
  "Commands that might trigger a `godot-gdscript-indent-line' call."
  :type '(repeat symbol)
  :group 'godot-gdscript)

(define-obsolete-variable-alias
  'godot-gdscript-indent 'godot-gdscript-indent-offset "24.3")

(define-obsolete-variable-alias
  'godot-gdscript-guess-indent 'godot-gdscript-indent-guess-indent-offset "24.3")

(defvar godot-gdscript-indent-current-level 0
  "Deprecated var available for compatibility.")

(defvar godot-gdscript-indent-levels '(0)
  "Deprecated var available for compatibility.")

(make-obsolete-variable
 'godot-gdscript-indent-current-level
 "The indentation API changed to avoid global state.
The function `godot-gdscript-indent-calculate-levels' does not use it
anymore.  If you were defadvising it and or depended on this
variable for indentation customizations, refactor your code to
work on `godot-gdscript-indent-calculate-indentation' instead."
 "24.5")

(make-obsolete-variable
 'godot-gdscript-indent-levels
 "The indentation API changed to avoid global state.
The function `godot-gdscript-indent-calculate-levels' does not use it
anymore.  If you were defadvising it and or depended on this
variable for indentation customizations, refactor your code to
work on `godot-gdscript-indent-calculate-indentation' instead."
 "24.5")

(defun godot-gdscript-indent-guess-indent-offset ()
  "Guess and set `godot-gdscript-indent-offset' for the current buffer."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((block-end))
        (while (and (not block-end)
                    (re-search-forward
                     (godot-gdscript-rx line-start block-start) nil t))
          (when (and
                 (not (godot-gdscript-syntax-context-type))
                 (progn
                   (goto-char (line-end-position))
                   (godot-gdscript-util-forward-comment -1)
                   (if (equal (char-before) ?:)
                       t
                     (forward-line 1)
                     (when (godot-gdscript-info-block-continuation-line-p)
                       (while (and (godot-gdscript-info-continuation-line-p)
                                   (not (eobp)))
                         (forward-line 1))
                       (godot-gdscript-util-forward-comment -1)
                       (when (equal (char-before) ?:)
                         t)))))
            (setq block-end (point-marker))))
        (let ((indentation
               (when block-end
                 (goto-char block-end)
                 (godot-gdscript-util-forward-comment)
                 (current-indentation))))
          (if (and indentation (not (zerop indentation)))
              (set (make-local-variable 'godot-gdscript-indent-offset) indentation)
            (message "Can't guess godot-gdscript-indent-offset, using defaults: %s"
                     godot-gdscript-indent-offset)))))))

(defun godot-gdscript-indent-context ()
  "Get information about the current indentation context.
Context is returned in a cons with the form (STATUS . START).

STATUS can be one of the following:

keyword
-------

:after-comment
 - Point is after a comment line.
 - START is the position of the \"#\" character.
:inside-string
 - Point is inside string.
 - START is the position of the first quote that starts it.
:no-indent
 - No possible indentation case matches.
 - START is always zero.

:inside-paren
 - Fallback case when point is inside paren.
 - START is the first non space char position *after* the open paren.
:inside-paren-at-closing-nested-paren
 - Point is on a line that contains a nested paren closer.
 - START is the position of the open paren it closes.
:inside-paren-at-closing-paren
 - Point is on a line that contains a paren closer.
 - START is the position of the open paren.
:inside-paren-newline-start
 - Point is inside a paren with items starting in their own line.
 - START is the position of the open paren.
:inside-paren-newline-start-from-block
 - Point is inside a paren with items starting in their own line
   from a block start.
 - START is the position of the open paren.

:after-backslash
 - Fallback case when point is after backslash.
 - START is the char after the position of the backslash.
:after-backslash-assignment-continuation
 - Point is after a backslashed assignment.
 - START is the char after the position of the backslash.
:after-backslash-block-continuation
 - Point is after a backslashed block continuation.
 - START is the char after the position of the backslash.
:after-backslash-dotted-continuation
 - Point is after a backslashed dotted continuation.  Previous
   line must contain a dot to align with.
 - START is the char after the position of the backslash.
:after-backslash-first-line
 - First line following a backslashed continuation.
 - START is the char after the position of the backslash.

:after-block-end
 - Point is after a line containing a block ender.
 - START is the position where the ender starts.
:after-block-start
 - Point is after a line starting a block.
 - START is the position where the block starts.
:after-line
 - Point is after a simple line.
 - START is the position where the previous line starts.
:at-dedenter-block-start
 - Point is on a line starting a dedenter block.
 - START is the position where the dedenter block starts."
  (save-restriction
    (widen)
    (let ((ppss (save-excursion
                  (beginning-of-line)
                  (syntax-ppss))))
      (cond
       ;; Beginning of buffer.
       ((= (line-number-at-pos) 1)
        (cons :no-indent 0))
       ;; Inside a string.
       ((let ((start (godot-gdscript-syntax-context 'string ppss)))
          (when start
            (cons :inside-string start))))
       ;; Inside a paren.
       ((let* ((start (godot-gdscript-syntax-context 'paren ppss))
               (starts-in-newline
                (when start
                  (save-excursion
                    (goto-char start)
                    (forward-char)
                    (not
                     (= (line-number-at-pos)
                        (progn
                          (godot-gdscript-util-forward-comment)
                          (line-number-at-pos))))))))
          (when start
            (cond
             ;; Current line only holds the closing paren.
             ((save-excursion
                (skip-syntax-forward " ")
                (when (and (godot-gdscript-syntax-closing-paren-p)
                           (progn
                             (forward-char 1)
                             (not (godot-gdscript-syntax-context 'paren))))
                  (cons :inside-paren-at-closing-paren start))))
             ;; Current line only holds a closing paren for nested.
             ((save-excursion
                (back-to-indentation)
                (godot-gdscript-syntax-closing-paren-p))
              (cons :inside-paren-at-closing-nested-paren start))
             ;; This line starts from a opening block in its own line.
             ((save-excursion
                (goto-char start)
                (when (and
                       starts-in-newline
                       (save-excursion
                         (back-to-indentation)
                         (looking-at (godot-gdscript-rx block-start))))
                  (cons
                   :inside-paren-newline-start-from-block start))))
             (starts-in-newline
              (cons :inside-paren-newline-start start))
             ;; General case.
             (t (cons :inside-paren
                      (save-excursion
                        (goto-char (1+ start))
                        (skip-syntax-forward "(" 1)
                        (skip-syntax-forward " ")
                        (point))))))))
       ;; After backslash.
       ((let ((start (when (not (godot-gdscript-syntax-comment-or-string-p ppss))
                       (godot-gdscript-info-line-ends-backslash-p
                        (1- (line-number-at-pos))))))
          (when start
            (cond
             ;; Continuation of dotted expression.
             ((save-excursion
                (back-to-indentation)
                (when (eq (char-after) ?\.)
                  ;; Move point back until it's not inside a paren.
                  (while (prog2
                             (forward-line -1)
                             (and (not (bobp))
                                  (godot-gdscript-syntax-context 'paren))))
                  (goto-char (line-end-position))
                  (while (and (search-backward
                               "." (line-beginning-position) t)
                              (godot-gdscript-syntax-context-type)))
                  ;; Ensure previous statement has dot to align with.
                  (when (and (eq (char-after) ?\.)
                             (not (godot-gdscript-syntax-context-type)))
                    (cons :after-backslash-dotted-continuation (point))))))
             ;; Continuation of block definition.
             ((let ((block-continuation-start
                     (godot-gdscript-info-block-continuation-line-p)))
                (when block-continuation-start
                  (save-excursion
                    (goto-char block-continuation-start)
                    (re-search-forward
                     (godot-gdscript-rx block-start (* space))
                     (line-end-position) t)
                    (cons :after-backslash-block-continuation (point))))))
             ;; Continuation of assignment.
             ((let ((assignment-continuation-start
                     (godot-gdscript-info-assignment-continuation-line-p)))
                (when assignment-continuation-start
                  (save-excursion
                    (goto-char assignment-continuation-start)
                    (cons :after-backslash-assignment-continuation (point))))))
             ;; First line after backslash continuation start.
             ((save-excursion
                (goto-char start)
                (when (or (= (line-number-at-pos) 1)
                          (not (godot-gdscript-info-beginning-of-backslash
                                (1- (line-number-at-pos)))))
                  (cons :after-backslash-first-line start))))
             ;; General case.
             (t (cons :after-backslash start))))))
       ;; After beginning of block.
       ((let ((start (save-excursion
                       (back-to-indentation)
                       (godot-gdscript-util-forward-comment -1)
                       (when (equal (char-before) ?:)
                         (godot-gdscript-nav-beginning-of-block)))))
          (when start
            (cons :after-block-start start))))
       ;; At dedenter statement.
       ((let ((start (godot-gdscript-info-dedenter-statement-p)))
          (when start
            (cons :at-dedenter-block-start start))))
       ;; After normal line, comment or ender (default case).
       ((save-excursion
          (back-to-indentation)
          (skip-chars-backward " \t\n")
          (godot-gdscript-nav-beginning-of-statement)
          (cons
           (cond ((godot-gdscript-info-current-line-comment-p)
                  :after-comment)
                 ((save-excursion
                    (goto-char (line-end-position))
                    (godot-gdscript-util-forward-comment -1)
                    (godot-gdscript-nav-beginning-of-statement)
                    (looking-at (godot-gdscript-rx block-ender)))
                  :after-block-end)
                 (t :after-line))
           (point))))))))

(defun godot-gdscript-indent--calculate-indentation ()
  "Internal implementation of `godot-gdscript-indent-calculate-indentation'.
May return an integer for the maximum possible indentation at
current context or a list of integers.  The latter case is only
happening for :at-dedenter-block-start context since the
possibilities can be narrowed to specific indentation points."
  (save-restriction
    (widen)
    (save-excursion
      (pcase (godot-gdscript-indent-context)
        (`(:no-indent . ,_) 0)
        (`(,(or :after-line
                :after-comment
                :inside-string
                :after-backslash
                :inside-paren-at-closing-paren
                :inside-paren-at-closing-nested-paren) . ,start)
         ;; Copy previous indentation.
         (goto-char start)
         (current-indentation))
        (`(,(or :after-block-start
                :after-backslash-first-line
                :inside-paren-newline-start) . ,start)
         ;; Add one indentation level.
         (goto-char start)
         (+ (current-indentation) godot-gdscript-indent-offset))
        (`(,(or :inside-paren
                :after-backslash-block-continuation
                :after-backslash-assignment-continuation
                :after-backslash-dotted-continuation) . ,start)
         ;; Use the column given by the context.
         (goto-char start)
         (current-column))
        (`(:after-block-end . ,start)
         ;; Subtract one indentation level.
         (goto-char start)
         (- (current-indentation) godot-gdscript-indent-offset))
        (`(:at-dedenter-block-start . ,_)
         ;; List all possible indentation levels from opening blocks.
         (let ((opening-block-start-points
                (godot-gdscript-info-dedenter-opening-block-positions)))
           (if (not opening-block-start-points)
               0  ; if not found default to first column
             (mapcar (lambda (pos)
                       (save-excursion
                         (goto-char pos)
                         (current-indentation)))
                     opening-block-start-points))))
        (`(,(or :inside-paren-newline-start-from-block) . ,start)
         ;; Add two indentation levels to make the suite stand out.
         (goto-char start)
         (+ (current-indentation) (* godot-gdscript-indent-offset 2)))))))

(defun godot-gdscript-indent--calculate-levels (indentation)
  "Calculate levels list given INDENTATION.
Argument INDENTATION can either be an integer or a list of
integers.  Levels are returned in ascending order, and in the
case INDENTATION is a list, this order is enforced."
  (if (listp indentation)
      (sort (copy-sequence indentation) #'<)
    (let* ((remainder (% indentation godot-gdscript-indent-offset))
           (steps (/ (- indentation remainder) godot-gdscript-indent-offset))
           (levels (mapcar (lambda (step)
                             (* godot-gdscript-indent-offset step))
                           (number-sequence steps 0 -1))))
      (reverse
       (if (not (zerop remainder))
           (cons indentation levels)
         levels)))))

(defun godot-gdscript-indent--previous-level (levels indentation)
  "Return previous level from LEVELS relative to INDENTATION."
  (let* ((levels (sort (copy-sequence levels) #'>))
         (default (car levels)))
    (catch 'return
      (dolist (level levels)
        (when (funcall #'< level indentation)
          (throw 'return level)))
      default)))

(defun godot-gdscript-indent-calculate-indentation (&optional previous)
  "Calculate indentation.
Get indentation of PREVIOUS level when argument is non-nil.
Return the max level of the cycle when indentation reaches the
minimum."
  (let* ((indentation (godot-gdscript-indent--calculate-indentation))
         (levels (godot-gdscript-indent--calculate-levels indentation)))
    (if previous
        (godot-gdscript-indent--previous-level levels (current-indentation))
      (apply #'max levels))))

(defun godot-gdscript-indent-line (&optional previous)
  "Internal implementation of `godot-gdscript-indent-line-function'.
Use the PREVIOUS level when argument is non-nil, otherwise indent
to the maximum available level.  When indentation is the minimum
possible and PREVIOUS is non-nil, cycle back to the maximum
level."
  (let ((follow-indentation-p
         ;; Check if point is within indentation.
         (and (<= (line-beginning-position) (point))
              (>= (+ (line-beginning-position)
                     (current-indentation))
                  (point)))))
    (save-excursion
      (indent-line-to
       (godot-gdscript-indent-calculate-indentation previous))
      (godot-gdscript-info-dedenter-opening-block-message))
    (when follow-indentation-p
      (back-to-indentation))))

(defun godot-gdscript-indent-calculate-levels ()
  "Return possible indentation levels."
  (godot-gdscript-indent--calculate-levels
   (godot-gdscript-indent--calculate-indentation)))

(defun godot-gdscript-indent-line-function ()
  "`indent-line-function' for Godot-Gdscript mode.
When the variable `last-command' is equal to one of the symbols
inside `godot-gdscript-indent-trigger-commands' it cycles possible
indentation levels from right to left."
  (godot-gdscript-indent-line
   (and (memq this-command godot-gdscript-indent-trigger-commands)
        (eq last-command this-command))))

(defun godot-gdscript-indent-dedent-line ()
  "De-indent current line."
  (interactive "*")
  (when (and (not (bolp))
           (not (godot-gdscript-syntax-comment-or-string-p))
           (= (current-indentation) (current-column)))
      (godot-gdscript-indent-line t)
      t))

(defun godot-gdscript-indent-dedent-line-backspace (arg)
  "De-indent current line.
Argument ARG is passed to `backward-delete-char-untabify' when
point is not in between the indentation."
  (interactive "*p")
  (unless (godot-gdscript-indent-dedent-line)
    (backward-delete-char-untabify arg)))

(put 'godot-gdscript-indent-dedent-line-backspace 'delete-selection 'supersede)

(defun godot-gdscript-indent-region (start end)
  "Indent a Godot-Gdscript region automagically.

Called from a program, START and END specify the region to indent."
  (let ((deactivate-mark nil))
    (save-excursion
      (goto-char end)
      (setq end (point-marker))
      (goto-char start)
      (or (bolp) (forward-line 1))
      (while (< (point) end)
        (or (and (bolp) (eolp))
            (when (and
                   ;; Skip if previous line is empty or a comment.
                   (save-excursion
                     (let ((line-is-comment-p
                            (godot-gdscript-info-current-line-comment-p)))
                       (forward-line -1)
                       (not
                        (or (and (godot-gdscript-info-current-line-comment-p)
                                 ;; Unless this line is a comment too.
                                 (not line-is-comment-p))
                            (godot-gdscript-info-current-line-empty-p)))))
                   ;; Don't mess with strings, unless it's the
                   ;; enclosing set of quotes.
                   (or (not (godot-gdscript-syntax-context 'string))
                       (eq
                        (syntax-after
                         (+ (1- (point))
                            (current-indentation)
                            (godot-gdscript-syntax-count-quotes (char-after) (point))))
                        (string-to-syntax "|")))
                   ;; Skip if current line is a block start, a
                   ;; dedenter or block ender.
                   (save-excursion
                     (back-to-indentation)
                     (not (looking-at
                           (godot-gdscript-rx
                            (or block-start dedenter block-ender))))))
              (godot-gdscript-indent-line)))
        (forward-line 1))
      (move-marker end nil))))

(defun godot-gdscript-indent-shift-left (start end &optional count)
  "Shift lines contained in region START END by COUNT columns to the left.
COUNT defaults to `godot-gdscript-indent-offset'.  If region isn't
active, the current line is shifted.  The shifted region includes
the lines in which START and END lie.  An error is signaled if
any lines in the region are indented less than COUNT columns."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end) current-prefix-arg)
     (list (line-beginning-position) (line-end-position) current-prefix-arg)))
  (if count
      (setq count (prefix-numeric-value count))
    (setq count godot-gdscript-indent-offset))
  (when (> count 0)
    (let ((deactivate-mark nil))
      (save-excursion
        (goto-char start)
        (while (< (point) end)
          (if (and (< (current-indentation) count)
                   (not (looking-at "[ \t]*$")))
              (error "Can't shift all lines enough"))
          (forward-line))
        (indent-rigidly start end (- count))))))

(add-to-list 'debug-ignored-errors "^Can't shift all lines enough")

(defun godot-gdscript-indent-shift-right (start end &optional count)
  "Shift lines contained in region START END by COUNT columns to the right.
COUNT defaults to `godot-gdscript-indent-offset'.  If region isn't
active, the current line is shifted.  The shifted region includes
the lines in which START and END lie."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end) current-prefix-arg)
     (list (line-beginning-position) (line-end-position) current-prefix-arg)))
  (let ((deactivate-mark nil))
    (setq count (if count (prefix-numeric-value count)
                  godot-gdscript-indent-offset))
    (indent-rigidly start end count)))

(defun godot-gdscript-indent-post-self-insert-function ()
  "Adjust indentation after insertion of some characters.
This function is intended to be added to `post-self-insert-hook.'
If a line renders a paren alone, after adding a char before it,
the line will be re-indented automatically if needed."
  (when (and electric-indent-mode
             (eq (char-before) last-command-event))
    (cond
     ;; Electric indent inside parens
     ((and
       (not (bolp))
       (let ((paren-start (godot-gdscript-syntax-context 'paren)))
         ;; Check that point is inside parens.
         (when paren-start
           (not
            ;; Filter the case where input is happening in the same
            ;; line where the open paren is.
            (= (line-number-at-pos)
               (line-number-at-pos paren-start)))))
       ;; When content has been added before the closing paren or a
       ;; comma has been inserted, it's ok to do the trick.
       (or
        (memq (char-after) '(?\) ?\] ?\}))
        (eq (char-before) ?,)))
      (save-excursion
        (goto-char (line-beginning-position))
        (let ((indentation (godot-gdscript-indent-calculate-indentation)))
          (when (< (current-indentation) indentation)
            (indent-line-to indentation)))))
     ;; Electric colon
     ((and (eq ?: last-command-event)
           (memq ?: electric-indent-chars)
           (not current-prefix-arg)
           ;; Trigger electric colon only at end of line
           (eolp)
           ;; Avoid re-indenting on extra colon
           (not (equal ?: (char-before (1- (point)))))
           (not (godot-gdscript-syntax-comment-or-string-p)))
      ;; Just re-indent dedenters
      (let ((dedenter-pos (godot-gdscript-info-dedenter-statement-p))
            (current-pos (point)))
        (when dedenter-pos
          (save-excursion
            (goto-char dedenter-pos)
            (godot-gdscript-indent-line)
            (unless (= (line-number-at-pos dedenter-pos)
                       (line-number-at-pos current-pos))
              ;; Reindent region if this is a multiline statement
              (godot-gdscript-indent-region dedenter-pos current-pos)))))))))

;;; Navigation

(defvar godot-gdscript-nav-beginning-of-defun-regexp
  (godot-gdscript-rx line-start (* space) defun (+ space) (group symbol-name))
  "Regexp matching class or function definition.
The name of the defun should be grouped so it can be retrieved
via `match-string'.")

(defun godot-gdscript-nav--beginning-of-defun (&optional arg)
  "Internal implementation of `godot-gdscript-nav-beginning-of-defun'.
With positive ARG search backwards, else search forwards."
  (when (or (null arg) (= arg 0)) (setq arg 1))
  (let* ((re-search-fn (if (> arg 0)
                           #'re-search-backward
                         #'re-search-forward))
         (line-beg-pos (line-beginning-position))
         (line-content-start (+ line-beg-pos (current-indentation)))
         (pos (point-marker))
         (beg-indentation
          (and (> arg 0)
               (save-excursion
                 (while (and
                         (not (godot-gdscript-info-looking-at-beginning-of-defun))
                         (godot-gdscript-nav-backward-block)))
                 (or (and (godot-gdscript-info-looking-at-beginning-of-defun)
                          (+ (current-indentation) godot-gdscript-indent-offset))
                     0))))
         (found
          (progn
            (when (and (< arg 0)
                       (godot-gdscript-info-looking-at-beginning-of-defun))
              (end-of-line 1))
            (while (and (funcall re-search-fn
                                 godot-gdscript-nav-beginning-of-defun-regexp nil t)
                        (or (godot-gdscript-syntax-context-type)
                            ;; Handle nested defuns when moving
                            ;; backwards by checking indentation.
                            (and (> arg 0)
                                 (not (= (current-indentation) 0))
                                 (>= (current-indentation) beg-indentation)))))
            (and (godot-gdscript-info-looking-at-beginning-of-defun)
                 (or (not (= (line-number-at-pos pos)
                             (line-number-at-pos)))
                     (and (>= (point) line-beg-pos)
                          (<= (point) line-content-start)
                          (> pos line-content-start)))))))
    (if found
        (or (beginning-of-line 1) t)
      (and (goto-char pos) nil))))

(defun godot-gdscript-nav-beginning-of-defun (&optional arg)
  "Move point to `beginning-of-defun'.
With positive ARG search backwards else search forward.
ARG nil or 0 defaults to 1.  When searching backwards,
nested defuns are handled with care depending on current
point position.  Return non-nil if point is moved to
`beginning-of-defun'."
  (when (or (null arg) (= arg 0)) (setq arg 1))
  (let ((found))
    (while (and (not (= arg 0))
                (let ((keep-searching-p
                       (godot-gdscript-nav--beginning-of-defun arg)))
                  (when (and keep-searching-p (null found))
                    (setq found t))
                  keep-searching-p))
      (setq arg (if (> arg 0) (1- arg) (1+ arg))))
    found))

(defun godot-gdscript-nav-end-of-defun ()
  "Move point to the end of def or class.
Returns nil if point is not in a def or class."
  (interactive)
  (let ((beg-defun-indent)
        (beg-pos (point)))
    (when (or (godot-gdscript-info-looking-at-beginning-of-defun)
              (godot-gdscript-nav-beginning-of-defun 1)
              (godot-gdscript-nav-beginning-of-defun -1))
      (setq beg-defun-indent (current-indentation))
      (while (progn
               (godot-gdscript-nav-end-of-statement)
               (godot-gdscript-util-forward-comment 1)
               (and (> (current-indentation) beg-defun-indent)
                    (not (eobp)))))
      (godot-gdscript-util-forward-comment -1)
      (forward-line 1)
      ;; Ensure point moves forward.
      (and (> beg-pos (point)) (goto-char beg-pos)))))

(defun godot-gdscript-nav--syntactically (fn poscompfn &optional contextfn)
  "Move point using FN avoiding places with specific context.
FN must take no arguments.  POSCOMPFN is a two arguments function
used to compare current and previous point after it is moved
using FN, this is normally a less-than or greater-than
comparison.  Optional argument CONTEXTFN defaults to
`godot-gdscript-syntax-context-type' and is used for checking current
point context, it must return a non-nil value if this point must
be skipped."
  (let ((contextfn (or contextfn 'godot-gdscript-syntax-context-type))
        (start-pos (point-marker))
        (prev-pos))
    (catch 'found
      (while t
        (let* ((newpos
                (and (funcall fn) (point-marker)))
               (context (funcall contextfn)))
          (cond ((and (not context) newpos
                      (or (and (not prev-pos) newpos)
                          (and prev-pos newpos
                               (funcall poscompfn newpos prev-pos))))
                 (throw 'found (point-marker)))
                ((and newpos context)
                 (setq prev-pos (point)))
                (t (when (not newpos) (goto-char start-pos))
                   (throw 'found nil))))))))

(defun godot-gdscript-nav--forward-defun (arg)
  "Internal implementation of godot-gdscript-nav-{backward,forward}-defun.
Uses ARG to define which function to call, and how many times
repeat it."
  (let ((found))
    (while (and (> arg 0)
                (setq found
                      (godot-gdscript-nav--syntactically
                       (lambda ()
                         (re-search-forward
                          godot-gdscript-nav-beginning-of-defun-regexp nil t))
                       '>)))
      (setq arg (1- arg)))
    (while (and (< arg 0)
                (setq found
                      (godot-gdscript-nav--syntactically
                       (lambda ()
                         (re-search-backward
                          godot-gdscript-nav-beginning-of-defun-regexp nil t))
                       '<)))
      (setq arg (1+ arg)))
    found))

(defun godot-gdscript-nav-backward-defun (&optional arg)
  "Navigate to closer defun backward ARG times.
Unlikely `godot-gdscript-nav-beginning-of-defun' this doesn't care about
nested definitions."
  (interactive "^p")
  (godot-gdscript-nav--forward-defun (- (or arg 1))))

(defun godot-gdscript-nav-forward-defun (&optional arg)
  "Navigate to closer defun forward ARG times.
Unlikely `godot-gdscript-nav-beginning-of-defun' this doesn't care about
nested definitions."
  (interactive "^p")
  (godot-gdscript-nav--forward-defun (or arg 1)))

(defun godot-gdscript-nav-beginning-of-statement ()
  "Move to start of current statement."
  (interactive "^")
  (back-to-indentation)
  (let* ((ppss (syntax-ppss))
         (context-point
          (or
           (godot-gdscript-syntax-context 'paren ppss)
           (godot-gdscript-syntax-context 'string ppss))))
    (cond ((bobp))
          (context-point
           (goto-char context-point)
           (godot-gdscript-nav-beginning-of-statement))
          ((save-excursion
             (forward-line -1)
             (godot-gdscript-info-line-ends-backslash-p))
           (forward-line -1)
           (godot-gdscript-nav-beginning-of-statement))))
  (point-marker))

(defun godot-gdscript-nav-end-of-statement (&optional noend)
  "Move to end of current statement.
Optional argument NOEND is internal and makes the logic to not
jump to the end of line when moving forward searching for the end
of the statement."
  (interactive "^")
  (let (string-start bs-pos)
    (while (and (or noend (goto-char (line-end-position)))
                (not (eobp))
                (cond ((setq string-start (godot-gdscript-syntax-context 'string))
                       (goto-char string-start)
                       (if (godot-gdscript-syntax-context 'paren)
                           ;; Ended up inside a paren, roll again.
                           (godot-gdscript-nav-end-of-statement t)
                         ;; This is not inside a paren, move to the
                         ;; end of this string.
                         (goto-char (+ (point)
                                       (godot-gdscript-syntax-count-quotes
                                        (char-after (point)) (point))))
                         (or (re-search-forward (rx (syntax string-delimiter)) nil t)
                             (goto-char (point-max)))))
                      ((godot-gdscript-syntax-context 'paren)
                       ;; The statement won't end before we've escaped
                       ;; at least one level of parenthesis.
                       (condition-case err
                           (goto-char (scan-lists (point) 1 -1))
                         (scan-error (goto-char (nth 3 err)))))
                      ((setq bs-pos (godot-gdscript-info-line-ends-backslash-p))
                       (goto-char bs-pos)
                       (forward-line 1))))))
  (point-marker))

(defun godot-gdscript-nav-backward-statement (&optional arg)
  "Move backward to previous statement.
With ARG, repeat.  See `godot-gdscript-nav-forward-statement'."
  (interactive "^p")
  (or arg (setq arg 1))
  (godot-gdscript-nav-forward-statement (- arg)))

(defun godot-gdscript-nav-forward-statement (&optional arg)
  "Move forward to next statement.
With ARG, repeat.  With negative argument, move ARG times
backward to previous statement."
  (interactive "^p")
  (or arg (setq arg 1))
  (while (> arg 0)
    (godot-gdscript-nav-end-of-statement)
    (godot-gdscript-util-forward-comment)
    (godot-gdscript-nav-beginning-of-statement)
    (setq arg (1- arg)))
  (while (< arg 0)
    (godot-gdscript-nav-beginning-of-statement)
    (godot-gdscript-util-forward-comment -1)
    (godot-gdscript-nav-beginning-of-statement)
    (setq arg (1+ arg))))

(defun godot-gdscript-nav-beginning-of-block ()
  "Move to start of current block."
  (interactive "^")
  (let ((starting-pos (point)))
    (if (progn
          (godot-gdscript-nav-beginning-of-statement)
          (looking-at (godot-gdscript-rx block-start)))
        (point-marker)
      ;; Go to first line beginning a statement
      (while (and (not (bobp))
                  (or (and (godot-gdscript-nav-beginning-of-statement) nil)
                      (godot-gdscript-info-current-line-comment-p)
                      (godot-gdscript-info-current-line-empty-p)))
        (forward-line -1))
      (let ((block-matching-indent
             (- (current-indentation) godot-gdscript-indent-offset)))
        (while
            (and (godot-gdscript-nav-backward-block)
                 (> (current-indentation) block-matching-indent)))
        (if (and (looking-at (godot-gdscript-rx block-start))
                 (= (current-indentation) block-matching-indent))
            (point-marker)
          (and (goto-char starting-pos) nil))))))

(defun godot-gdscript-nav-end-of-block ()
  "Move to end of current block."
  (interactive "^")
  (when (godot-gdscript-nav-beginning-of-block)
    (let ((block-indentation (current-indentation)))
      (godot-gdscript-nav-end-of-statement)
      (while (and (forward-line 1)
                  (not (eobp))
                  (or (and (> (current-indentation) block-indentation)
                           (or (godot-gdscript-nav-end-of-statement) t))
                      (godot-gdscript-info-current-line-comment-p)
                      (godot-gdscript-info-current-line-empty-p))))
      (godot-gdscript-util-forward-comment -1)
      (point-marker))))

(defun godot-gdscript-nav-backward-block (&optional arg)
  "Move backward to previous block of code.
With ARG, repeat.  See `godot-gdscript-nav-forward-block'."
  (interactive "^p")
  (or arg (setq arg 1))
  (godot-gdscript-nav-forward-block (- arg)))

(defun godot-gdscript-nav-forward-block (&optional arg)
  "Move forward to next block of code.
With ARG, repeat.  With negative argument, move ARG times
backward to previous block."
  (interactive "^p")
  (or arg (setq arg 1))
  (let ((block-start-regexp
         (godot-gdscript-rx line-start (* whitespace) block-start))
        (starting-pos (point)))
    (while (> arg 0)
      (godot-gdscript-nav-end-of-statement)
      (while (and
              (re-search-forward block-start-regexp nil t)
              (godot-gdscript-syntax-context-type)))
      (setq arg (1- arg)))
    (while (< arg 0)
      (godot-gdscript-nav-beginning-of-statement)
      (while (and
              (re-search-backward block-start-regexp nil t)
              (godot-gdscript-syntax-context-type)))
      (setq arg (1+ arg)))
    (godot-gdscript-nav-beginning-of-statement)
    (if (not (looking-at (godot-gdscript-rx block-start)))
        (and (goto-char starting-pos) nil)
      (and (not (= (point) starting-pos)) (point-marker)))))

(defun godot-gdscript-nav--lisp-forward-sexp (&optional arg)
  "Standard version `forward-sexp'.
It ignores completely the value of `forward-sexp-function' by
setting it to nil before calling `forward-sexp'.  With positive
ARG move forward only one sexp, else move backwards."
  (let ((forward-sexp-function)
        (arg (if (or (not arg) (> arg 0)) 1 -1)))
    (forward-sexp arg)))

(defun godot-gdscript-nav--lisp-forward-sexp-safe (&optional arg)
  "Safe version of standard `forward-sexp'.
When at end of sexp (i.e. looking at a opening/closing paren)
skips it instead of throwing an error.  With positive ARG move
forward only one sexp, else move backwards."
  (let* ((arg (if (or (not arg) (> arg 0)) 1 -1))
         (paren-regexp
          (if (> arg 0) (godot-gdscript-rx close-paren) (godot-gdscript-rx open-paren)))
         (search-fn
          (if (> arg 0) #'re-search-forward #'re-search-backward)))
    (condition-case nil
        (godot-gdscript-nav--lisp-forward-sexp arg)
      (error
       (while (and (funcall search-fn paren-regexp nil t)
                   (godot-gdscript-syntax-context 'paren)))))))

(defun godot-gdscript-nav--forward-sexp (&optional dir safe)
  "Move to forward sexp.
With positive optional argument DIR direction move forward, else
backwards.  When optional argument SAFE is non-nil do not throw
errors when at end of sexp, skip it instead."
  (setq dir (or dir 1))
  (unless (= dir 0)
    (let* ((forward-p (if (> dir 0)
                          (and (setq dir 1) t)
                        (and (setq dir -1) nil)))
           (context-type (godot-gdscript-syntax-context-type)))
      (cond
       ((memq context-type '(string comment))
        ;; Inside of a string, get out of it.
        (let ((forward-sexp-function))
          (forward-sexp dir)))
       ((or (eq context-type 'paren)
            (and forward-p (looking-at (godot-gdscript-rx open-paren)))
            (and (not forward-p)
                 (eq (syntax-class (syntax-after (1- (point))))
                     (car (string-to-syntax ")")))))
        ;; Inside a paren or looking at it, lisp knows what to do.
        (if safe
            (godot-gdscript-nav--lisp-forward-sexp-safe dir)
          (godot-gdscript-nav--lisp-forward-sexp dir)))
       (t
        ;; This part handles the lispy feel of
        ;; `godot-gdscript-nav-forward-sexp'.  Knowing everything about the
        ;; current context and the context of the next sexp tries to
        ;; follow the lisp sexp motion commands in a symmetric manner.
        (let* ((context
                (cond
                 ((godot-gdscript-info-beginning-of-block-p) 'block-start)
                 ((godot-gdscript-info-end-of-block-p) 'block-end)
                 ((godot-gdscript-info-beginning-of-statement-p) 'statement-start)
                 ((godot-gdscript-info-end-of-statement-p) 'statement-end)))
               (next-sexp-pos
                (save-excursion
                  (if safe
                      (godot-gdscript-nav--lisp-forward-sexp-safe dir)
                    (godot-gdscript-nav--lisp-forward-sexp dir))
                  (point)))
               (next-sexp-context
                (save-excursion
                  (goto-char next-sexp-pos)
                  (cond
                   ((godot-gdscript-info-beginning-of-block-p) 'block-start)
                   ((godot-gdscript-info-end-of-block-p) 'block-end)
                   ((godot-gdscript-info-beginning-of-statement-p) 'statement-start)
                   ((godot-gdscript-info-end-of-statement-p) 'statement-end)
                   ((godot-gdscript-info-statement-starts-block-p) 'starts-block)
                   ((godot-gdscript-info-statement-ends-block-p) 'ends-block)))))
          (if forward-p
              (cond ((and (not (eobp))
                          (godot-gdscript-info-current-line-empty-p))
                     (godot-gdscript-util-forward-comment dir)
                     (godot-gdscript-nav--forward-sexp dir))
                    ((eq context 'block-start)
                     (godot-gdscript-nav-end-of-block))
                    ((eq context 'statement-start)
                     (godot-gdscript-nav-end-of-statement))
                    ((and (memq context '(statement-end block-end))
                          (eq next-sexp-context 'ends-block))
                     (goto-char next-sexp-pos)
                     (godot-gdscript-nav-end-of-block))
                    ((and (memq context '(statement-end block-end))
                          (eq next-sexp-context 'starts-block))
                     (goto-char next-sexp-pos)
                     (godot-gdscript-nav-end-of-block))
                    ((memq context '(statement-end block-end))
                     (goto-char next-sexp-pos)
                     (godot-gdscript-nav-end-of-statement))
                    (t (goto-char next-sexp-pos)))
            (cond ((and (not (bobp))
                        (godot-gdscript-info-current-line-empty-p))
                   (godot-gdscript-util-forward-comment dir)
                   (godot-gdscript-nav--forward-sexp dir))
                  ((eq context 'block-end)
                   (godot-gdscript-nav-beginning-of-block))
                  ((eq context 'statement-end)
                   (godot-gdscript-nav-beginning-of-statement))
                  ((and (memq context '(statement-start block-start))
                        (eq next-sexp-context 'starts-block))
                   (goto-char next-sexp-pos)
                   (godot-gdscript-nav-beginning-of-block))
                  ((and (memq context '(statement-start block-start))
                        (eq next-sexp-context 'ends-block))
                   (goto-char next-sexp-pos)
                   (godot-gdscript-nav-beginning-of-block))
                  ((memq context '(statement-start block-start))
                   (goto-char next-sexp-pos)
                   (godot-gdscript-nav-beginning-of-statement))
                  (t (goto-char next-sexp-pos))))))))))

(defun godot-gdscript-nav-forward-sexp (&optional arg)
  "Move forward across expressions.
With ARG, do it that many times.  Negative arg -N means move
backward N times."
  (interactive "^p")
  (or arg (setq arg 1))
  (while (> arg 0)
    (godot-gdscript-nav--forward-sexp 1)
    (setq arg (1- arg)))
  (while (< arg 0)
    (godot-gdscript-nav--forward-sexp -1)
    (setq arg (1+ arg))))

(defun godot-gdscript-nav-backward-sexp (&optional arg)
  "Move backward across expressions.
With ARG, do it that many times.  Negative arg -N means move
forward N times."
  (interactive "^p")
  (or arg (setq arg 1))
  (godot-gdscript-nav-forward-sexp (- arg)))

(defun godot-gdscript-nav-forward-sexp-safe (&optional arg)
  "Move forward safely across expressions.
With ARG, do it that many times.  Negative arg -N means move
backward N times."
  (interactive "^p")
  (or arg (setq arg 1))
  (while (> arg 0)
    (godot-gdscript-nav--forward-sexp 1 t)
    (setq arg (1- arg)))
  (while (< arg 0)
    (godot-gdscript-nav--forward-sexp -1 t)
    (setq arg (1+ arg))))

(defun godot-gdscript-nav-backward-sexp-safe (&optional arg)
  "Move backward safely across expressions.
With ARG, do it that many times.  Negative arg -N means move
forward N times."
  (interactive "^p")
  (or arg (setq arg 1))
  (godot-gdscript-nav-forward-sexp-safe (- arg)))

(defun godot-gdscript-nav--up-list (&optional dir)
  "Internal implementation of `godot-gdscript-nav-up-list'.
DIR is always 1 or -1 and comes sanitized from
`godot-gdscript-nav-up-list' calls."
  (let ((context (godot-gdscript-syntax-context-type))
        (forward-p (> dir 0)))
    (cond
     ((memq context '(string comment)))
     ((eq context 'paren)
      (let ((forward-sexp-function))
        (up-list dir)))
     ((and forward-p (godot-gdscript-info-end-of-block-p))
      (let ((parent-end-pos
             (save-excursion
               (let ((indentation (and
                                   (godot-gdscript-nav-beginning-of-block)
                                   (current-indentation))))
                 (while (and indentation
                             (> indentation 0)
                             (>= (current-indentation) indentation)
                             (godot-gdscript-nav-backward-block)))
                 (godot-gdscript-nav-end-of-block)))))
        (and (> (or parent-end-pos (point)) (point))
             (goto-char parent-end-pos))))
     (forward-p (godot-gdscript-nav-end-of-block))
     ((and (not forward-p)
           (> (current-indentation) 0)
           (godot-gdscript-info-beginning-of-block-p))
      (let ((prev-block-pos
             (save-excursion
               (let ((indentation (current-indentation)))
                 (while (and (godot-gdscript-nav-backward-block)
                             (>= (current-indentation) indentation))))
               (point))))
        (and (> (point) prev-block-pos)
             (goto-char prev-block-pos))))
     ((not forward-p) (godot-gdscript-nav-beginning-of-block)))))

(defun godot-gdscript-nav-up-list (&optional arg)
  "Move forward out of one level of parentheses (or blocks).
With ARG, do this that many times.
A negative argument means move backward but still to a less deep spot.
This command assumes point is not in a string or comment."
  (interactive "^p")
  (or arg (setq arg 1))
  (while (> arg 0)
    (godot-gdscript-nav--up-list 1)
    (setq arg (1- arg)))
  (while (< arg 0)
    (godot-gdscript-nav--up-list -1)
    (setq arg (1+ arg))))

(defun godot-gdscript-nav-backward-up-list (&optional arg)
  "Move backward out of one level of parentheses (or blocks).
With ARG, do this that many times.
A negative argument means move forward but still to a less deep spot.
This command assumes point is not in a string or comment."
  (interactive "^p")
  (or arg (setq arg 1))
  (godot-gdscript-nav-up-list (- arg)))

(defun godot-gdscript-nav-if-name-main ()
  "Move point at the beginning the __main__ block.
When \"if __name__ == '__main__':\" is found returns its
position, else returns nil."
  (interactive)
  (let ((point (point))
        (found (catch 'found
                 (goto-char (point-min))
                 (while (re-search-forward
                         (godot-gdscript-rx line-start
                                    "if" (+ space)
                                    "__name__" (+ space)
                                    "==" (+ space)
                                    (group-n 1 (or ?\" ?\'))
                                    "__main__" (backref 1) (* space) ":")
                         nil t)
                   (when (not (godot-gdscript-syntax-context-type))
                     (beginning-of-line)
                     (throw 'found t))))))
    (if found
        (point)
      (ignore (goto-char point)))))

;;; Shell integration

(defcustom godot-gdscript-shell-buffer-name "Godot-GDScript"
  "Default buffer name for Godot-GDScript interpreter."
  :type 'string
  :group 'godot-gdscript
  :safe 'stringp)

(defcustom godot-gdscript-shell-interpreter "godot-gdscript"
  "Default Godot-GDScript interpreter for shell."
  :type 'string
  :group 'godot-gdscript)

(defcustom godot-gdscript-shell-internal-buffer-name "godot-gdscript internal"
  "Default buffer name for the Internal Godot-GDScript interpreter."
  :type 'string
  :group 'godot-gdscript
  :safe 'stringp)

(defcustom godot-gdscript-shell-interpreter-args "-i"
  "Default arguments for the Godot-GDScript interpreter."
  :type 'string
  :group 'godot-gdscript)

(defcustom godot-gdscript-shell-interpreter-interactive-arg "-i"
  "Interpreter argument to force it to run interactively."
  :type 'string
  :version "24.4")

(defcustom godot-gdscript-shell-prompt-detect-enabled t
  "Non-nil enables autodetection of interpreter prompts."
  :type 'boolean
  :safe 'booleanp
  :version "24.4")

(defcustom godot-gdscript-shell-prompt-detect-failure-warning t
  "Non-nil enables warnings when detection of prompts fail."
  :type 'boolean
  :safe 'booleanp
  :version "24.4")

(defcustom godot-gdscript-shell-prompt-input-regexps
  '(">>> " "\\.\\.\\. "                 ; Godot-GDScript
    "In \\[[0-9]+\\]: "                 ; IGodot-GDScript
    ;; Using ipdb outside IGodot-GDScript may fail to cleanup and leave static
    ;; IGodot-GDScript prompts activated, this adds some safeguard for that.
    "In : " "\\.\\.\\.: ")
  "List of regular expressions matching input prompts."
  :type '(repeat string)
  :version "24.4")

(defcustom godot-gdscript-shell-prompt-output-regexps
  '(""                                  ; Godot-GDScript
    "Out\\[[0-9]+\\]: "                 ; IGodot-GDScript
    "Out :")                            ; ipdb safeguard
  "List of regular expressions matching output prompts."
  :type '(repeat string)
  :version "24.4")

(defcustom godot-gdscript-shell-prompt-regexp ">>> "
  "Regular expression matching top level input prompt of Godot-GDScript shell.
It should not contain a caret (^) at the beginning."
  :type 'string)

(defcustom godot-gdscript-shell-prompt-block-regexp "\\.\\.\\. "
  "Regular expression matching block input prompt of Godot-GDScript shell.
It should not contain a caret (^) at the beginning."
  :type 'string)

(defcustom godot-gdscript-shell-prompt-output-regexp ""
  "Regular expression matching output prompt of Godot-GDScript shell.
It should not contain a caret (^) at the beginning."
  :type 'string)

(defcustom godot-gdscript-shell-prompt-pdb-regexp "[(<]*[Ii]?[Pp]db[>)]+ "
  "Regular expression matching pdb input prompt of Godot-GDScript shell.
It should not contain a caret (^) at the beginning."
  :type 'string)

(defcustom godot-gdscript-shell-enable-font-lock t
  "Should syntax highlighting be enabled in the Godot-GDScript shell buffer?
Restart the Godot-GDScript shell after changing this variable for
it to take effect."
  :type 'boolean
  :group 'godot-gdscript
  :safe 'booleanp)

(defcustom godot-gdscript-shell-unbuffered t
  "Should shell output be unbuffered?.
When non-nil, this may prevent delayed and missing output in the
Godot-GDScript shell.  See commentary for details."
  :type 'boolean
  :group 'godot-gdscript
  :safe 'booleanp)

(defcustom godot-gdscript-shell-process-environment nil
  "List of environment variables for Godot-GDScript shell.
This variable follows the same rules as `process-environment'
since it merges with it before the process creation routines are
called.  When this variable is nil, the Godot-GDScript shell is run with
the default `process-environment'."
  :type '(repeat string)
  :group 'godot-gdscript
  :safe 'listp)

(defcustom godot-gdscript-shell-extra-godot-gdscriptpaths nil
  "List of extra Godot-GDScriptpaths for Godot-GDScript shell.
The values of this variable are added to the existing value of
GODOT-GDSCRIPTPATH in the `process-environment' variable."
  :type '(repeat string)
  :group 'godot-gdscript
  :safe 'listp)

(defcustom godot-gdscript-shell-exec-path nil
  "List of path to search for binaries.
This variable follows the same rules as `exec-path' since it
merges with it before the process creation routines are called.
When this variable is nil, the Godot-GDScript shell is run with the
default `exec-path'."
  :type '(repeat string)
  :group 'godot-gdscript
  :safe 'listp)

(defcustom godot-gdscript-shell-virtualenv-path nil
  "Path to virtualenv root.
This variable, when set to a string, makes the values stored in
`godot-gdscript-shell-process-environment' and
`godot-gdscript-shell-exec-path' to be modified properly so
shells are started with the specified virtualenv."
  :type '(choice (const nil) string)
  :group 'godot-gdscript
  :safe 'stringp)

(defcustom godot-gdscript-shell-setup-codes '(godot-gdscript-shell-completion-setup-code
                                      godot-gdscript-ffap-setup-code)
  "List of code run by `godot-gdscript-shell-send-setup-codes'."
  :type '(repeat symbol)
  :group 'godot-gdscript
  :safe 'listp)

(defcustom godot-gdscript-shell-compilation-regexp-alist
  `((,(rx line-start (1+ (any " \t")) "File \""
          (group (1+ (not (any "\"<")))) ; avoid `<stdin>' &c
          "\", line " (group (1+ digit)))
     1 2)
    (,(rx " in file " (group (1+ not-newline)) " on line "
          (group (1+ digit)))
     1 2)
    (,(rx line-start "> " (group (1+ (not (any "(\"<"))))
          "(" (group (1+ digit)) ")" (1+ (not (any "("))) "()")
     1 2))
  "`compilation-error-regexp-alist' for inferior Godot-GDScript."
  :type '(alist string)
  :group 'godot-gdscript)

(defvar godot-gdscript-shell--prompt-calculated-input-regexp nil
  "Calculated input prompt regexp for inferior Godot-GDScript shell.
Do not set this variable directly, instead use
`godot-gdscript-shell-prompt-set-calculated-regexps'.")

(defvar godot-gdscript-shell--prompt-calculated-output-regexp nil
  "Calculated output prompt regexp for inferior Godot-GDScript shell.
Do not set this variable directly, instead use
`godot-gdscript-shell-set-prompt-regexp'.")

(defun godot-gdscript-shell-prompt-detect ()
  "Detect prompts for the current `godot-gdscript-shell-interpreter'.
When prompts can be retrieved successfully from the
`godot-gdscript-shell-interpreter' run with
`godot-gdscript-shell-interpreter-interactive-arg', returns a list of
three elements, where the first two are input prompts and the
last one is an output prompt.  When no prompts can be detected
and `godot-gdscript-shell-prompt-detect-failure-warning' is non-nil,
shows a warning with instructions to avoid hangs and returns nil.
When `godot-gdscript-shell-prompt-detect-enabled' is nil avoids any
detection and just returns nil."
  (when godot-gdscript-shell-prompt-detect-enabled
    ;; FIXME This check causes Godot to run again, opening a new window.
    (let* ((process-environment (godot-gdscript-shell-calculate-process-environment))
           (exec-path (godot-gdscript-shell-calculate-exec-path))
           (code (concat
                  "import sys\n"
                  "ps = [getattr(sys, 'ps%s' % i, '') for i in range(1,4)]\n"
                  ;; JSON is built manually for compatibility
                  "ps_json = '\\n[\"%s\", \"%s\", \"%s\"]\\n' % tuple(ps)\n"
                  "print (ps_json)\n"
                  "sys.exit(0)\n"))
           (output
            (with-temp-buffer
              ;; TODO: improve error handling by using
              ;; `condition-case' and displaying the error message to
              ;; the user in the no-prompts warning.
              (ignore-errors
                (let ((code-file (godot-gdscript-shell--save-temp-file code)))
                  ;; Use `process-file' as it is remote-host friendly.
                  (process-file
                   godot-gdscript-shell-interpreter
                   code-file
                   '(t nil)
                   nil
                   godot-gdscript-shell-interpreter-interactive-arg)
                  ;; Try to cleanup
                  (delete-file code-file)))
              (buffer-string)))
           (prompts
            (catch 'prompts
              (dolist (line (split-string output "\n" t))
                (let ((res
                       ;; Check if current line is a valid JSON array
                       (and (string= (substring line 0 2) "[\"")
                            (ignore-errors
                              ;; Return prompts as a list, not vector
                              (append (json-read-from-string line) nil)))))
                  ;; The list must contain 3 strings, where the first
                  ;; is the input prompt, the second is the block
                  ;; prompt and the last one is the output prompt.  The
                  ;; input prompt is the only one that can't be empty.
                  (when (and (= (length res) 3)
                             (cl-every #'stringp res)
                             (not (string= (car res) "")))
                    (throw 'prompts res))))
              nil)))
      (when (and (not prompts)
                 godot-gdscript-shell-prompt-detect-failure-warning)
        (warn
         (concat
          "Godot-GDScript shell prompts cannot be detected.\n"
          "If your emacs session hangs when starting Godot-GDScript shells\n"
          "recover with `keyboard-quit' and then try fixing the\n"
          "interactive flag for your interpreter by adjusting the\n"
          "`godot-gdscript-shell-interpreter-interactive-arg' or add regexps\n"
          "matching shell prompts in the directory-local friendly vars:\n"
          "  + `godot-gdscript-shell-prompt-regexp'\n"
          "  + `godot-gdscript-shell-prompt-block-regexp'\n"
          "  + `godot-gdscript-shell-prompt-output-regexp'\n"
          "Or alternatively in:\n"
          "  + `godot-gdscript-shell-prompt-input-regexps'\n"
          "  + `godot-gdscript-shell-prompt-output-regexps'")))
      prompts)))

(defun godot-gdscript-shell-prompt-validate-regexps ()
  "Validate all user provided regexps for prompts.
Signals `user-error' if any of these vars contain invalid
regexps: `godot-gdscript-shell-prompt-regexp',
`godot-gdscript-shell-prompt-block-regexp',
`godot-gdscript-shell-prompt-pdb-regexp',
`godot-gdscript-shell-prompt-output-regexp',
`godot-gdscript-shell-prompt-input-regexps',
`godot-gdscript-shell-prompt-output-regexps'."
  (dolist (symbol (list 'godot-gdscript-shell-prompt-input-regexps
                        'godot-gdscript-shell-prompt-output-regexps
                        'godot-gdscript-shell-prompt-regexp
                        'godot-gdscript-shell-prompt-block-regexp
                        'godot-gdscript-shell-prompt-pdb-regexp
                        'godot-gdscript-shell-prompt-output-regexp))
    (dolist (regexp (let ((regexps (symbol-value symbol)))
                      (if (listp regexps)
                          regexps
                        (list regexps))))
      (when (not (godot-gdscript-util-valid-regexp-p regexp))
        (user-error "Invalid regexp %s in `%s'"
                    regexp symbol)))))

(defun godot-gdscript-shell-prompt-set-calculated-regexps ()
  "Detect and set input and output prompt regexps.
Build and set the values for
`godot-gdscript-shell-input-prompt-regexp' and
`godot-gdscript-shell-output-prompt-regexp' using the values from
`godot-gdscript-shell-prompt-regexp',
`godot-gdscript-shell-prompt-block-regexp',
`godot-gdscript-shell-prompt-pdb-regexp',
`godot-gdscript-shell-prompt-output-regexp',
`godot-gdscript-shell-prompt-input-regexps',
`godot-gdscript-shell-prompt-output-regexps' and detected prompts
from `godot-gdscript-shell-prompt-detect'."
  (when (not (and godot-gdscript-shell--prompt-calculated-input-regexp
                  godot-gdscript-shell--prompt-calculated-output-regexp))
    (let* ((detected-prompts (godot-gdscript-shell-prompt-detect))
           (input-prompts nil)
           (output-prompts nil)
           (build-regexp
            (lambda (prompts)
              (concat "^\\("
                      (mapconcat #'identity
                                 (sort prompts
                                       (lambda (a b)
                                         (let ((length-a (length a))
                                               (length-b (length b)))
                                           (if (= length-a length-b)
                                               (string< a b)
                                             (> (length a) (length b))))))
                                 "\\|")
                      "\\)"))))
      ;; Validate ALL regexps
      (godot-gdscript-shell-prompt-validate-regexps)
      ;; Collect all user defined input prompts
      (dolist (prompt (append godot-gdscript-shell-prompt-input-regexps
                              (list godot-gdscript-shell-prompt-regexp
                                    godot-gdscript-shell-prompt-block-regexp
                                    godot-gdscript-shell-prompt-pdb-regexp)))
        (cl-pushnew prompt input-prompts :test #'string=))
      ;; Collect all user defined output prompts
      (dolist (prompt (cons godot-gdscript-shell-prompt-output-regexp
                            godot-gdscript-shell-prompt-output-regexps))
        (cl-pushnew prompt output-prompts :test #'string=))
      ;; Collect detected prompts if any
      (when detected-prompts
        (dolist (prompt (butlast detected-prompts))
          (setq prompt (regexp-quote prompt))
          (cl-pushnew prompt input-prompts :test #'string=))
        (cl-pushnew (regexp-quote
                     (car (last detected-prompts)))
                    output-prompts :test #'string=))
      ;; Set input and output prompt regexps from collected prompts
      (setq godot-gdscript-shell--prompt-calculated-input-regexp
            (funcall build-regexp input-prompts)
            godot-gdscript-shell--prompt-calculated-output-regexp
            (funcall build-regexp output-prompts)))))

(defun godot-gdscript-shell-get-process-name (dedicated)
  "Calculate the appropriate process name for inferior Godot-GDScript process.
If DEDICATED is t and the variable `buffer-file-name' is non-nil
returns a string with the form
`godot-gdscript-shell-buffer-name'[variable `buffer-file-name']
else returns the value of `godot-gdscript-shell-buffer-name'."
  (let ((process-name
         (if (and dedicated
                  buffer-file-name)
             (format "%s[%s]" godot-gdscript-shell-buffer-name buffer-file-name)
           (format "%s" godot-gdscript-shell-buffer-name))))
    process-name))

(defun godot-gdscript-shell-internal-get-process-name ()
  "Calculate the appropriate process name for internal Godot-GDScript process.
The name is calculated from
`godot-gdscript-shell-global-buffer-name' and a hash of all
relevant global shell settings in order to ensure uniqueness for
different types of configurations."
  (format "%s [%s]"
          godot-gdscript-shell-internal-buffer-name
          (md5
           (concat
            godot-gdscript-shell-interpreter
            godot-gdscript-shell-interpreter-args
            godot-gdscript-shell--prompt-calculated-input-regexp
            godot-gdscript-shell--prompt-calculated-output-regexp
            (mapconcat #'symbol-value godot-gdscript-shell-setup-codes "")
            (mapconcat #'identity godot-gdscript-shell-process-environment "")
            (mapconcat #'identity godot-gdscript-shell-extra-godot-gdscriptpaths "")
            (mapconcat #'identity godot-gdscript-shell-exec-path "")
            (or godot-gdscript-shell-virtualenv-path "")
            (mapconcat #'identity godot-gdscript-shell-exec-path "")))))

(defun godot-gdscript-shell-parse-command ()    ;FIXME: why name it "parse"?
  "Calculate the string used to execute the inferior Godot-GDScript process."
  ;; FIXME: process-environment doesn't seem to be used anywhere within
  ;; this let.
  (let ((process-environment (godot-gdscript-shell-calculate-process-environment))
        (exec-path (godot-gdscript-shell-calculate-exec-path)))
    (format "%s %s"
            ;; FIXME: Why executable-find?
            (shell-quote-argument
             (executable-find godot-gdscript-shell-interpreter))
            godot-gdscript-shell-interpreter-args)))

(defun godot-gdscript-shell-calculate-process-environment ()
  "Calculate process environment given `godot-gdscript-shell-virtualenv-path'."
  (let ((process-environment (append
                              godot-gdscript-shell-process-environment
                              process-environment nil))
        (virtualenv (if godot-gdscript-shell-virtualenv-path
                        (directory-file-name godot-gdscript-shell-virtualenv-path)
                      nil)))
    (when godot-gdscript-shell-unbuffered
      (setenv "GODOT-GDSCRIPTUNBUFFERED" "1"))
    (when godot-gdscript-shell-extra-godot-gdscriptpaths
      (setenv "GODOT-GDSCRIPTPATH"
              (format "%s%s%s"
                      (mapconcat 'identity
                                 godot-gdscript-shell-extra-godot-gdscriptpaths
                                 path-separator)
                      path-separator
                      (or (getenv "GODOT-GDSCRIPTPATH") ""))))
    (if (not virtualenv)
        process-environment
      (setenv "GODOT-GDSCRIPTHOME" nil)
      (setenv "PATH" (format "%s/bin%s%s"
                             virtualenv path-separator
                             (or (getenv "PATH") "")))
      (setenv "VIRTUAL_ENV" virtualenv))
    process-environment))

(defun godot-gdscript-shell-calculate-exec-path ()
  "Calculate exec path given `godot-gdscript-shell-virtualenv-path'."
  (let ((path (append godot-gdscript-shell-exec-path
                      exec-path nil)))  ;FIXME: Why nil?
    (if (not godot-gdscript-shell-virtualenv-path)
        path
      (cons (expand-file-name "bin" godot-gdscript-shell-virtualenv-path)
            path))))

(defun godot-gdscript-comint-output-filter-function (output)
  "Hook run after content is put into comint buffer.
OUTPUT is a string with the contents of the buffer."
  (ansi-color-filter-apply output))

(defvar godot-gdscript-shell--parent-buffer nil)

(defvar godot-gdscript-shell-output-syntax-table
  (let ((table (make-syntax-table godot-gdscript-dotty-syntax-table)))
    (modify-syntax-entry ?\' "." table)
    (modify-syntax-entry ?\" "." table)
    (modify-syntax-entry ?\( "." table)
    (modify-syntax-entry ?\[ "." table)
    (modify-syntax-entry ?\{ "." table)
    (modify-syntax-entry ?\) "." table)
    (modify-syntax-entry ?\] "." table)
    (modify-syntax-entry ?\} "." table)
    table)
  "Syntax table for shell output.
It makes parens and quotes be treated as punctuation chars.")

(define-derived-mode inferior-godot-gdscript-mode comint-mode "Inferior Godot-GDScript"
  "Major mode for Godot-GDScript inferior process.
Runs a Godot-GDScript interpreter as a subprocess of Emacs, with
Godot-GDScript I/O through an Emacs buffer. Variables
`godot-gdscript-shell-interpreter' and
`godot-gdscript-shell-interpreter-args' control which
Godot-GDScript interpreter is run. Variables
`godot-gdscript-shell-prompt-regexp',
`godot-gdscript-shell-prompt-output-regexp',
`godot-gdscript-shell-prompt-block-regexp',
`godot-gdscript-shell-enable-font-lock',
`godot-gdscript-shell-completion-setup-code',
`godot-gdscript-shell-completion-string-code',
`godot-gdscript-ffap-setup-code' and
`godot-gdscript-ffap-string-code' can customize this mode for
different Godot-GDScript interpreters.

You can also add additional setup code to be run at
initialization of the interpreter via `godot-gdscript-shell-setup-codes'
variable.

\(Type \\[describe-mode] in the process buffer for a list of commands.)"
  (let ((interpreter godot-gdscript-shell-interpreter)
        (args godot-gdscript-shell-interpreter-args))
    (when godot-gdscript-shell--parent-buffer
      (godot-gdscript-util-clone-local-variables godot-gdscript-shell--parent-buffer))
    ;; Users can override default values for these vars when calling
    ;; `godot-gdscript-run-script'.  This ensures new values let-bound in
    ;; `godot-gdscript-shell-make-comint' are locally set.
    (set (make-local-variable 'godot-gdscript-shell-interpreter) interpreter)
    (set (make-local-variable 'godot-gdscript-shell-interpreter-args) args))
  (set (make-local-variable 'godot-gdscript-shell--prompt-calculated-input-regexp) nil)
  (set (make-local-variable 'godot-gdscript-shell--prompt-calculated-output-regexp) nil)
  ;; FIXME Causes window to be duplicated in `godot-gdscript-shell-prompt-detect`.
  ;; (godot-gdscript-shell-prompt-set-calculated-regexps)
  ;; As Godot's terminal does not accept input, we avoid font-locking for now.
  ;; (setq comint-prompt-regexp godot-gdscript-shell--prompt-calculated-input-regexp)
  ;; (setq mode-line-process '(":%s"))
  ;; (make-local-variable 'comint-output-filter-functions)
  ;; (add-hook 'comint-output-filter-functions
  ;;           'godot-gdscript-comint-output-filter-function)
  ;; (add-hook 'comint-output-filter-functions
  ;;           'godot-gdscript-pdbtrack-comint-output-filter-function)
  ;; (set (make-local-variable 'compilation-error-regexp-alist)
  ;;      godot-gdscript-shell-compilation-regexp-alist)
  ;; (define-key inferior-godot-gdscript-mode-map [remap complete-symbol]
  ;;   'completion-at-point)
  ;; (add-hook 'completion-at-point-functions
  ;;           #'godot-gdscript-shell-completion-complete-at-point nil 'local)
  ;; (add-hook 'comint-dynamic-complete-functions ;FIXME: really?
  ;;           #'godot-gdscript-shell-completion-complete-at-point nil 'local)
  ;; (define-key inferior-godot-gdscript-mode-map "\t"
  ;;   'godot-gdscript-shell-completion-complete-or-indent)
  ;; (make-local-variable 'godot-gdscript-pdbtrack-buffers-to-kill)
  ;; (make-local-variable 'godot-gdscript-pdbtrack-tracked-buffer)
  ;; (make-local-variable 'godot-gdscript-shell-internal-last-output)
  ;; (when godot-gdscript-shell-enable-font-lock
  ;;   (set-syntax-table godot-gdscript-mode-syntax-table)
  ;;   (set (make-local-variable 'font-lock-defaults)
  ;;        '(godot-gdscript-font-lock-keywords nil nil nil nil))
  ;;   (set (make-local-variable 'syntax-propertize-function)
  ;;        (eval
  ;;         ;; XXX: Unfortunately eval is needed here to make use of the
  ;;         ;; dynamic value of `comint-prompt-regexp'.
  ;;         `(syntax-propertize-rules
  ;;           (,comint-prompt-regexp
  ;;            (0 (ignore
  ;;                (put-text-property
  ;;                 comint-last-input-start end 'syntax-table
  ;;                 godot-gdscript-shell-output-syntax-table)
  ;;                ;; XXX: This might look weird, but it is the easiest
  ;;                ;; way to ensure font lock gets cleaned up before the
  ;;                ;; current prompt, which is needed for unclosed
  ;;                ;; strings to not mess up with current input.
  ;;                (font-lock-unfontify-region comint-last-input-start end))))
  ;;           (,(godot-gdscript-rx string-delimiter)
  ;;            (0 (ignore
  ;;                (and (not (eq (get-text-property start 'field) 'output))
  ;;                     (godot-gdscript-syntax-stringify)))))))))
  ;; (compilation-shell-minor-mode 1)
)

(defun godot-gdscript-shell-make-comint (cmd proc-name &optional pop internal)
  "Create a Godot-GDScript shell comint buffer.
CMD is the Godot-GDScript command to be executed and PROC-NAME is the
process name the comint buffer will get.  After the comint buffer
is created the `inferior-godot-gdscript-mode' is activated.  When
optional argument POP is non-nil the buffer is shown.  When
optional argument INTERNAL is non-nil this process is run on a
buffer with a name that starts with a space, following the Emacs
convention for temporary/internal buffers, and also makes sure
the user is not queried for confirmation when the process is
killed."
  (save-excursion
    (let* ((proc-buffer-name
            (format (if (not internal) "*%s*" " *%s*") proc-name))
           (process-environment (godot-gdscript-shell-calculate-process-environment))
           (exec-path (godot-gdscript-shell-calculate-exec-path)))
      (when (not (comint-check-proc proc-buffer-name))
        (let* ((cmdlist (split-string-and-unquote cmd))
               (interpreter (car cmdlist))
               (args (cdr cmdlist))
               (buffer (apply #'make-comint-in-buffer proc-name proc-buffer-name
                              interpreter nil args))
               (godot-gdscript-shell--parent-buffer (current-buffer))
               (process (get-buffer-process buffer))
               ;; As the user may have overridden default values for
               ;; these vars on `godot-gdscript-run-script', let-binding them allows
               ;; to have the new right values in all setup code
               ;; that's is done in `inferior-godot-gdscript-mode', which is
               ;; important, especially for prompt detection.
               (godot-gdscript-shell-interpreter interpreter)
               (godot-gdscript-shell-interpreter-args
                (mapconcat #'identity args " ")))
          (with-current-buffer buffer
            (inferior-godot-gdscript-mode))
          (accept-process-output process)
          (and pop (pop-to-buffer buffer t))
          (and internal (set-process-query-on-exit-flag process nil))))
      proc-buffer-name)))

;;;###autoload
(defun godot-gdscript-run-script (cmd &optional dedicated show)
  "Run an inferior Godot-GDScript process.
Input and output via buffer named after
`godot-gdscript-shell-buffer-name'.  If there is a process already
running in that buffer, just switch to it.

With argument, allows you to define CMD so you can edit the
command used to call the interpreter and define DEDICATED, so a
dedicated process for the current buffer is open.  When numeric
prefix arg is other than 0 or 4 do not SHOW.

Runs the hook `inferior-godot-gdscript-mode-hook' after
`comint-mode-hook' is run.  (Type \\[describe-mode] in the
process buffer for a list of commands.)"
  (interactive
   (if current-prefix-arg
       (list
        (read-string "Run Godot-GDScript: " (godot-gdscript-shell-parse-command))
        (y-or-n-p "Make dedicated process? ")
        (= (prefix-numeric-value current-prefix-arg) 4))
     (list (godot-gdscript-shell-parse-command) nil t)))
  (godot-gdscript-shell-make-comint
   cmd (godot-gdscript-shell-get-process-name dedicated) show)
  dedicated)

(defun godot-gdscript-run-script-internal ()
  "Run an inferior Internal Godot-GDScript process.
Input and output via buffer named after
`godot-gdscript-shell-internal-buffer-name' and what
`godot-gdscript-shell-internal-get-process-name' returns.

This new kind of shell is intended to be used for generic
communication related to defined configurations; the main
difference with global or dedicated shells is that these ones are
attached to a configuration, not a buffer. This means that can be
used for example to retrieve the sys.path and other stuff,
without messing with user shells. Note that
`godot-gdscript-shell-enable-font-lock' and
`inferior-godot-gdscript-mode-hook' are set to nil for these
shells, so setup codes are not sent at startup."
  (let ((godot-gdscript-shell-enable-font-lock nil)
        (inferior-godot-gdscript-mode-hook nil))
    (get-buffer-process
     (godot-gdscript-shell-make-comint
      (godot-gdscript-shell-parse-command)
      (godot-gdscript-shell-internal-get-process-name) nil t))))

(defun godot-gdscript-shell-get-buffer ()
  "Return inferior Godot-GDScript buffer for current buffer."
  (let* ((dedicated-proc-name (godot-gdscript-shell-get-process-name t))
         (dedicated-proc-buffer-name (format "*%s*" dedicated-proc-name))
         (global-proc-name  (godot-gdscript-shell-get-process-name nil))
         (global-proc-buffer-name (format "*%s*" global-proc-name))
         (dedicated-running (comint-check-proc dedicated-proc-buffer-name))
         (global-running (comint-check-proc global-proc-buffer-name)))
    ;; Always prefer dedicated
    (or (and dedicated-running dedicated-proc-buffer-name)
        (and global-running global-proc-buffer-name))))

(defun godot-gdscript-shell-get-process ()
  "Return inferior godot-gdscript process for current buffer."
  (get-buffer-process (godot-gdscript-shell-get-buffer)))

(defun godot-gdscript-shell-get-or-create-process (&optional cmd dedicated show)
  "Get or create an inferior GDScript process for current buffer and return it.
Arguments CMD, DEDICATED and SHOW are those of
`godot-gdscript-run-script' and are used to start the shell. If those
arguments are not provided, `godot-gdscript-run-script' is called
interactively and the user will be asked for their values."
  (let* ((dedicated-proc-name (godot-gdscript-shell-get-process-name t))
         (dedicated-proc-buffer-name (format "*%s*" dedicated-proc-name))
         (global-proc-name  (godot-gdscript-shell-get-process-name nil))
         (global-proc-buffer-name (format "*%s*" global-proc-name))
         (dedicated-running (comint-check-proc dedicated-proc-buffer-name))
         (global-running (comint-check-proc global-proc-buffer-name))
         (current-prefix-arg 16))
    (when (and (not dedicated-running) (not global-running))
      (if (if (not cmd)
              ;; XXX: Refactor code such that calling `godot-gdscript-run-script'
              ;; interactively is not needed anymore.
              (call-interactively 'godot-gdscript-run-script)
            (godot-gdscript-run-script cmd dedicated show))
          (setq dedicated-running t)
        (setq global-running t)))
    ;; Always prefer dedicated
    (get-buffer-process (if dedicated-running
                            dedicated-proc-buffer-name
                          global-proc-buffer-name))))

(defvar godot-gdscript-shell-internal-buffer nil
  "Current internal shell buffer for the current buffer.
This is really not necessary at all for the code to work but it's
there for compatibility with CEDET.")

(defvar godot-gdscript-shell-internal-last-output nil
  "Last output captured by the internal shell.
This is really not necessary at all for the code to work but it's
there for compatibility with CEDET.")

(defun godot-gdscript-shell-internal-get-or-create-process ()
  "Get or create an inferior internal Godot-GDScript process."
  (let* ((proc-name (godot-gdscript-shell-internal-get-process-name))
         (proc-buffer-name (format " *%s*" proc-name)))
    (when (not (process-live-p proc-name))
      (godot-gdscript-run-script-internal)
      (setq godot-gdscript-shell-internal-buffer proc-buffer-name)
      ;; XXX: Why is this `sit-for' needed?
      ;; `godot-gdscript-shell-make-comint' calls `accept-process-output'
      ;; already but it is not helping to get proper output on
      ;; 'gnu/linux when the internal shell process is not running and
      ;; a call to `godot-gdscript-shell-internal-send-string' is issued.
      (sit-for 0.1 t))
    (get-buffer-process proc-buffer-name)))

(define-obsolete-function-alias
  'godot-gdscript-proc 'godot-gdscript-shell-internal-get-or-create-process "24.3")

(define-obsolete-variable-alias
  'godot-gdscript-buffer 'godot-gdscript-shell-internal-buffer "24.3")

(define-obsolete-variable-alias
  'godot-gdscript-preoutput-result 'godot-gdscript-shell-internal-last-output "24.3")

(defun godot-gdscript-shell--save-temp-file (string)
  "Save a temporary file with contents defined in STRING."
  (let* ((temporary-file-directory
          (if (file-remote-p default-directory)
              (concat (file-remote-p default-directory) "/tmp")
            temporary-file-directory))
         (temp-file-name (make-temp-file "py"))
         (coding-system-for-write (godot-gdscript-info-encoding)))
    (with-temp-file temp-file-name
      (insert string)
      (delete-trailing-whitespace))
    temp-file-name))

(defun godot-gdscript-shell-send-string (string &optional process)
  "Send STRING to inferior Godot-GDScript PROCESS."
  (interactive "sGodot-GDScript command: ")
  (let ((process (or process (godot-gdscript-shell-get-or-create-process))))
    (if (string-match ".\n+." string)   ;Multiline.
        (let* ((temp-file-name (godot-gdscript-shell--save-temp-file string))
               (file-name (or (buffer-file-name) temp-file-name)))
          (godot-gdscript-shell-send-file file-name process temp-file-name t))
      (comint-send-string process string)
      (when (or (not (string-match "\n\\'" string))
                (string-match "\n[ \t].*\n?\\'" string))
        (comint-send-string process "\n")))))

(defvar godot-gdscript-shell-output-filter-in-progress nil)
(defvar godot-gdscript-shell-output-filter-buffer nil)

(defun godot-gdscript-shell-output-filter (string)
  "Filter used in `godot-gdscript-shell-send-string-no-output' to grab output.
STRING is the output received to this point from the process.
This filter saves received output from the process in
`godot-gdscript-shell-output-filter-buffer' and stops receiving
it after detecting a prompt at the end of the buffer."
  (setq
   string (ansi-color-filter-apply string)
   godot-gdscript-shell-output-filter-buffer
   (concat godot-gdscript-shell-output-filter-buffer string))
  (when (string-match
         ;; XXX: It seems on OSX an extra carriage return is attached
         ;; at the end of output, this handles that too.
         (concat
          "\r?\n"
          ;; Remove initial caret from calculated regexp
          (replace-regexp-in-string
           (rx string-start ?^) ""
           godot-gdscript-shell--prompt-calculated-input-regexp)
          "$")
         godot-gdscript-shell-output-filter-buffer)
    ;; Output ends when `godot-gdscript-shell-output-filter-buffer' contains
    ;; the prompt attached at the end of it.
    (setq godot-gdscript-shell-output-filter-in-progress nil
          godot-gdscript-shell-output-filter-buffer
          (substring godot-gdscript-shell-output-filter-buffer
                     0 (match-beginning 0)))
    (when (string-match
           godot-gdscript-shell--prompt-calculated-output-regexp
           godot-gdscript-shell-output-filter-buffer)
      ;; Some shells, like IGodot-GDScript might append a prompt before the
      ;; output, clean that.
      (setq godot-gdscript-shell-output-filter-buffer
            (substring godot-gdscript-shell-output-filter-buffer (match-end 0)))))
  "")

(defun godot-gdscript-shell-send-string-no-output (string &optional process)
  "Send STRING to PROCESS and inhibit output.
Return the output."
  (let ((process (or process (godot-gdscript-shell-get-or-create-process)))
        (comint-preoutput-filter-functions
         '(godot-gdscript-shell-output-filter))
        (godot-gdscript-shell-output-filter-in-progress t)
        (inhibit-quit t))
    (or
     (with-local-quit
       (godot-gdscript-shell-send-string string process)
       (while godot-gdscript-shell-output-filter-in-progress
         ;; `godot-gdscript-shell-output-filter' takes care of setting
         ;; `godot-gdscript-shell-output-filter-in-progress' to NIL after it
         ;; detects end of output.
         (accept-process-output process))
       (prog1
           godot-gdscript-shell-output-filter-buffer
         (setq godot-gdscript-shell-output-filter-buffer nil)))
     (with-current-buffer (process-buffer process)
       (comint-interrupt-subjob)))))

(defun godot-gdscript-shell-internal-send-string (string)
  "Send STRING to the Internal Godot-GDScript interpreter.
Returns the output.  See `godot-gdscript-shell-send-string-no-output'."
  ;; XXX Remove `godot-gdscript-shell-internal-last-output' once CEDET is
  ;; updated to support this new mode.
  (setq godot-gdscript-shell-internal-last-output
        (godot-gdscript-shell-send-string-no-output
         ;; Makes this function compatible with the old
         ;; godot-gdscript-send-receive. (At least for CEDET).
         (replace-regexp-in-string "_emacs_out +" "" string)
         (godot-gdscript-shell-internal-get-or-create-process))))

(define-obsolete-function-alias
  'godot-gdscript-send-receive 'godot-gdscript-shell-internal-send-string "24.3")

(define-obsolete-function-alias
  'godot-gdscript-send-string 'godot-gdscript-shell-internal-send-string "24.3")

(defun godot-gdscript-shell-buffer-substring (start end &optional nomain)
  "Send buffer substring from START to END formatted for shell.
This is a wrapper over `buffer-substring' that takes care of
different transformations for the code sent to be evaluated in
the godot-gdscript shell:
  1. When optional argument NOMAIN is non-nil everything under an
     \"if __name__ == '__main__'\" block will be removed.
  2. When a subregion of the buffer is sent, it takes care of
     appending extra empty lines so tracebacks are correct.
  3. When the region sent is a substring of the current buffer, a
     coding cookie is added.
  4. Wraps indented regions under an \"if true:\" block so the
     interpreter evaluates them correctly."
  (let* ((substring (buffer-substring-no-properties start end))
         (starts-at-point-min-p (save-restriction
                                  (widen)
                                  (= (point-min) start)))
         (encoding (godot-gdscript-info-encoding))
         (fillstr (when (not starts-at-point-min-p)
                    (concat
                     (format "# -*- coding: %s -*-\n" encoding)
                     (make-string
                      ;; Subtract 2 because of the coding cookie.
                      (- (line-number-at-pos start) 2) ?\n))))
         (toplevel-block-p (save-excursion
                             (goto-char start)
                             (or (zerop (line-number-at-pos start))
                                 (progn
                                   (godot-gdscript-util-forward-comment 1)
                                   (zerop (current-indentation)))))))
    (with-temp-buffer
      (godot-gdscript-mode)
      (if fillstr (insert fillstr))
      (insert substring)
      (goto-char (point-min))
      (when (not toplevel-block-p)
        (insert "if true:")
        (delete-region (point) (line-end-position)))
      (when nomain
        (let* ((if-name-main-start-end
                (and nomain
                     (save-excursion
                       (when (godot-gdscript-nav-if-name-main)
                         (cons (point)
                               (progn (godot-gdscript-nav-forward-sexp-safe)
                                      ;; Include ending newline
                                      (forward-line 1)
                                      (point)))))))
               ;; Oh destructuring bind, how I miss you.
               (if-name-main-start (car if-name-main-start-end))
               (if-name-main-end (cdr if-name-main-start-end))
               (fillstr (make-string
                         (- (line-number-at-pos if-name-main-end)
                            (line-number-at-pos if-name-main-start)) ?\n)))
          (when if-name-main-start-end
            (goto-char if-name-main-start)
            (delete-region if-name-main-start if-name-main-end)
            (insert fillstr))))
      ;; Ensure there's only one coding cookie in the generated string.
      (goto-char (point-min))
      (when (looking-at-p (godot-gdscript-rx coding-cookie))
        (forward-line 1)
        (when (looking-at-p (godot-gdscript-rx coding-cookie))
          (delete-region
           (line-beginning-position) (line-end-position))))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun godot-gdscript-shell-send-region (start end &optional send-main)
  "Send the region delimited by START and END to inferior GDScript process.
When optional argument SEND-MAIN is non-nil, allow execution of
code inside blocks delimited by \"if __name__== '__main__':\".
When called interactively SEND-MAIN defaults to nil, unless it's
called with prefix argument."
  (interactive "r\nP")
  (let* ((string (godot-gdscript-shell-buffer-substring start end (not send-main)))
         (process (godot-gdscript-shell-get-or-create-process))
         (original-string (buffer-substring-no-properties start end))
         (_ (string-match "\\`\n*\\(.*\\)" original-string)))
    (message "Sent: %s..." (match-string 1 original-string))
    (godot-gdscript-shell-send-string string process)))

(defun godot-gdscript-shell-send-buffer (&optional send-main)
  "Send the entire buffer to inferior Godot-GDScript process.
When optional argument SEND-MAIN is non-nil, allow execution of
code inside blocks delimited by \"if __name__== '__main__':\".
When called interactively SEND-MAIN defaults to nil, unless it's
called with prefix argument."
  (interactive "P")
  (save-restriction
    (widen)
    (godot-gdscript-shell-send-region (point-min) (point-max) send-main)))

(defun godot-gdscript-shell-send-defun (arg)
  "Send the current defun to inferior Godot-GDScript process.
When argument ARG is non-nil do not include decorators."
  (interactive "P")
  (save-excursion
    (godot-gdscript-shell-send-region
     (progn
       (end-of-line 1)
       (while (and (or (godot-gdscript-nav-beginning-of-defun)
                       (beginning-of-line 1))
                   (> (current-indentation) 0)))
       (when (not arg)
         (while (and (forward-line -1)
                     (looking-at (godot-gdscript-rx decorator))))
         (forward-line 1))
       (point-marker))
     (progn
       (or (godot-gdscript-nav-end-of-defun)
           (end-of-line 1))
       (point-marker)))))

(defun godot-gdscript-shell-send-file (file-name &optional process temp-file-name
                                         delete)
  "Send FILE-NAME to inferior Godot-GDScript PROCESS.
If TEMP-FILE-NAME is passed then that file is used for processing
instead, while internally the shell will continue to use
FILE-NAME.  If TEMP-FILE-NAME and DELETE are non-nil, then
TEMP-FILE-NAME is deleted after evaluation is performed."
  (interactive "fFile to send: ")
  (let* ((process (or process (godot-gdscript-shell-get-or-create-process)))
         (encoding (with-temp-buffer
                     (insert-file-contents
                      (or temp-file-name file-name))
                     (godot-gdscript-info-encoding)))
         (file-name (expand-file-name
                     (or (file-remote-p file-name 'localname)
                         file-name)))
         (temp-file-name (when temp-file-name
                           (expand-file-name
                            (or (file-remote-p temp-file-name 'localname)
                                temp-file-name)))))
    (godot-gdscript-shell-send-string
     (format
      (concat
       "import codecs, os;"
       "__pyfile = codecs.open('''%s''', encoding='''%s''');"
       "__code = __pyfile.read().encode('''%s''');"
       "__pyfile.close();"
       (when (and delete temp-file-name)
         (format "os.remove('''%s''');" temp-file-name))
       "exec(compile(__code, '''%s''', 'exec'));")
      (or temp-file-name file-name) encoding encoding file-name)
     process)))

(defun godot-gdscript-shell-switch-to-shell ()
  "Switch to inferior Godot-GDScript process buffer."
  (interactive)
  (pop-to-buffer (process-buffer (godot-gdscript-shell-get-or-create-process)) t))

(defun godot-gdscript-shell-send-setup-code ()
  "Send all setup code for shell.
This function takes the list of setup code to send from the
`godot-gdscript-shell-setup-codes' list."
  (let ((process (get-buffer-process (current-buffer))))
    (dolist (code godot-gdscript-shell-setup-codes)
      (when code
        (message "Sent %s" code)
        (godot-gdscript-shell-send-string
         (symbol-value code) process)))))

(add-hook 'inferior-godot-gdscript-mode-hook
          #'godot-gdscript-shell-send-setup-code)

;;; Shell completion

(defcustom godot-gdscript-shell-completion-setup-code
  "try:
    import __builtin__
except ImportError:
    # Godot-Gdscript 3
    import builtins as __builtin__
try:
    import readline, rlcompleter
except:
    def __GODOT-GDSCRIPT_EL_get_completions(text):
        return []
else:
    def __GODOT-GDSCRIPT_EL_get_completions(text):
        builtins = dir(__builtin__)
        completions = []
        try:
            splits = text.split()
            is_module = splits and splits[0] in ('from', 'import')
            is_igodot-gdscript = ('__IGODOT-GDSCRIPT__' in builtins or
                          '__IGODOT-GDSCRIPT__active' in builtins)
            if is_module:
                from IGodot-Gdscript.core.completerlib import module_completion
                completions = module_completion(text.strip())
            elif is_igodot-gdscript and '__IP' in builtins:
                completions = __IP.complete(text)
            elif is_igodot-gdscript and 'get_igodot-gdscript' in builtins:
                completions = get_igodot-gdscript().Completer.all_completions(text)
            else:
                i = 0
                while true:
                    res = readline.get_completer()(text, i)
                    if not res:
                        break
                    i += 1
                    completions.append(res)
        except:
            pass
        return completions"
  "Code used to setup completion in inferior Godot-Gdscript processes."
  :type 'string
  :group 'godot-gdscript)

(defcustom godot-gdscript-shell-completion-string-code
  "';'.join(__GODOT-GDSCRIPT_EL_get_completions('''%s'''))\n"
  "Gdscript code used to get a string of completions separated by semicolons.
The string passed to the function is the current Godot-GDScript name or
the full statement in the case of imports."
  :type 'string
  :group 'godot-gdscript)

(define-obsolete-variable-alias
  'godot-gdscript-shell-completion-module-string-code
  'godot-gdscript-shell-completion-string-code
  "24.4"
  "Completion string code must also autocomplete modules.")

(defcustom godot-gdscript-shell-completion-pdb-string-code
  "';'.join(globals().keys() + locals().keys())"
  "Gdscript code used to get completions separated by semicolons for [i]pdb."
  :type 'string
  :group 'godot-gdscript)

(defun godot-gdscript-shell-completion-get-completions (process line input)
  "Do completion at point for PROCESS.
LINE is used to detect the context on how to complete given INPUT."
  (with-current-buffer (process-buffer process)
    (let* ((prompt
            ;; Get last prompt of the inferior process buffer (this
            ;; intentionally avoids using `comint-last-prompt' because
            ;; of incompatibilities with Emacs 24.x).
            (save-excursion
              (buffer-substring-no-properties
               (line-beginning-position) ;End of prompt.
               (re-search-backward "^"))))
           (completion-code
            ;; Check whether a prompt matches a pdb string, an import
            ;; statement or just the standard prompt and use the
            ;; correct godot-gdscript-shell-completion-*-code string
            (cond ((and (> (length godot-gdscript-shell-completion-pdb-string-code) 0)
                        (string-match
                         (concat "^" godot-gdscript-shell-prompt-pdb-regexp) prompt))
                   godot-gdscript-shell-completion-pdb-string-code)
                  ((string-match
                    godot-gdscript-shell--prompt-calculated-input-regexp prompt)
                   godot-gdscript-shell-completion-string-code)
                  (t nil)))
           (input
            (if (string-match
                 (godot-gdscript-rx line-start (* space) (or "from" "import") space)
                 line)
                line
              input)))
      (and completion-code
           (> (length input) 0)
           (let ((completions
                  (godot-gdscript-util-strip-string
                   (godot-gdscript-shell-send-string-no-output
                    (format completion-code input) process))))
             (and (> (length completions) 2)
                  (split-string completions
                                "^'\\|^\"\\|;\\|'$\\|\"$" t)))))))

(defun godot-gdscript-shell-completion-complete-at-point (&optional process)
  "Perform completion at point in inferior Godot-Gdscript.
Optional argument PROCESS forces completions to be retrieved
using that one instead of current buffer's process."
  (setq process (or process (get-buffer-process (current-buffer))))
  (let* ((start
          (save-excursion
            (with-syntax-table godot-gdscript-dotty-syntax-table
              (let* ((paren-depth (car (syntax-ppss)))
                     (syntax-string "w_")
                     (syntax-list (string-to-syntax syntax-string)))
                ;; Stop scanning for the beginning of the completion
                ;; subject after the char before point matches a
                ;; delimiter
                (while (member
                        (car (syntax-after (1- (point)))) syntax-list)
                  (skip-syntax-backward syntax-string)
                  (when (or (equal (char-before) ?\))
                            (equal (char-before) ?\"))
                    (forward-char -1))
                  (while (or
                          ;; honor initial paren depth
                          (> (car (syntax-ppss)) paren-depth)
                          (godot-gdscript-syntax-context 'string))
                    (forward-char -1)))
                (point)))))
         (end (point)))
    (list start end
          (completion-table-dynamic
           (apply-partially
            #'godot-gdscript-shell-completion-get-completions
            process (buffer-substring-no-properties
                     (line-beginning-position) end))))))

(defun godot-gdscript-shell-completion-complete-or-indent ()
  "Complete or indent depending on the context.
If content before pointer is all whitespace, indent.
If not try to complete."
  (interactive)
  (if (string-match "^[[:space:]]*$"
                    (buffer-substring (comint-line-beginning-position)
                                      (point-marker)))
      (indent-for-tab-command)
    (completion-at-point)))

;;; PDB Track integration

(defcustom godot-gdscript-pdbtrack-activate t
  "Non-nil makes Godot-Gdscript shell enable pdbtracking."
  :type 'boolean
  :group 'godot-gdscript
  :safe 'booleanp)

(defcustom godot-gdscript-pdbtrack-stacktrace-info-regexp
  "> \\([^\"(<]+\\)(\\([0-9]+\\))\\([?a-zA-Z0-9_<>]+\\)()"
  "Regular expression matching stacktrace information.
Used to extract the current line and module being inspected."
  :type 'string
  :group 'godot-gdscript
  :safe 'stringp)

(defvar godot-gdscript-pdbtrack-tracked-buffer nil
  "Variable containing the value of the current tracked buffer.
Never set this variable directly, use
`godot-gdscript-pdbtrack-set-tracked-buffer' instead.")

(defvar godot-gdscript-pdbtrack-buffers-to-kill nil
  "List of buffers to be deleted after tracking finishes.")

(defun godot-gdscript-pdbtrack-set-tracked-buffer (file-name)
  "Set the buffer for FILE-NAME as the tracked buffer.
Internally it uses the `godot-gdscript-pdbtrack-tracked-buffer' variable.
Returns the tracked buffer."
  (let ((file-buffer (get-file-buffer
                      (concat (file-remote-p default-directory)
                              file-name))))
    (if file-buffer
        (setq godot-gdscript-pdbtrack-tracked-buffer file-buffer)
      (setq file-buffer (find-file-noselect file-name))
      (when (not (member file-buffer godot-gdscript-pdbtrack-buffers-to-kill))
        (add-to-list 'godot-gdscript-pdbtrack-buffers-to-kill file-buffer)))
    file-buffer))

(defun godot-gdscript-pdbtrack-comint-output-filter-function (output)
  "Move overlay arrow to current pdb line in tracked buffer.
Argument OUTPUT is a string with the output from the comint process."
  (when (and godot-gdscript-pdbtrack-activate (not (string= output "")))
    (let* ((full-output (ansi-color-filter-apply
                         (buffer-substring comint-last-input-end (point-max))))
           (line-number)
           (file-name
            (with-temp-buffer
              (insert full-output)
              ;; When the debugger encounters a pdb.set_trace()
              ;; command, it prints a single stack frame.  Sometimes
              ;; it prints a bit of extra information about the
              ;; arguments of the present function.  When ipdb
              ;; encounters an exception, it prints the _entire_ stack
              ;; trace.  To handle all of these cases, we want to find
              ;; the _last_ stack frame printed in the most recent
              ;; batch of output, then jump to the corresponding
              ;; file/line number.
              (goto-char (point-max))
              (when (re-search-backward godot-gdscript-pdbtrack-stacktrace-info-regexp nil t)
                (setq line-number (string-to-number
                                   (match-string-no-properties 2)))
                (match-string-no-properties 1)))))
      (if (and file-name line-number)
          (let* ((tracked-buffer
                  (godot-gdscript-pdbtrack-set-tracked-buffer file-name))
                 (shell-buffer (current-buffer))
                 (tracked-buffer-window (get-buffer-window tracked-buffer))
                 (tracked-buffer-line-pos))
            (with-current-buffer tracked-buffer
              (set (make-local-variable 'overlay-arrow-string) "=>")
              (set (make-local-variable 'overlay-arrow-position) (make-marker))
              (setq tracked-buffer-line-pos (progn
                                              (goto-char (point-min))
                                              (forward-line (1- line-number))
                                              (point-marker)))
              (when tracked-buffer-window
                (set-window-point
                 tracked-buffer-window tracked-buffer-line-pos))
              (set-marker overlay-arrow-position tracked-buffer-line-pos))
            (pop-to-buffer tracked-buffer)
            (switch-to-buffer-other-window shell-buffer))
        (when godot-gdscript-pdbtrack-tracked-buffer
          (with-current-buffer godot-gdscript-pdbtrack-tracked-buffer
            (set-marker overlay-arrow-position nil))
          (mapc #'(lambda (buffer)
                    (ignore-errors (kill-buffer buffer)))
                godot-gdscript-pdbtrack-buffers-to-kill)
          (setq godot-gdscript-pdbtrack-tracked-buffer nil
                godot-gdscript-pdbtrack-buffers-to-kill nil)))))
  output)

;;; Symbol completion

(defun godot-gdscript-completion-complete-at-point ()
  "Complete current symbol at point.
For this to work as best as possible you should call
`godot-gdscript-shell-send-buffer' from time to time so context in
inferior Godot-Gdscript process is updated properly."
  (when (require 'company nil 'noerror)
    (company-complete)))

;;; Fill paragraph

(defcustom godot-gdscript-fill-comment-function 'godot-gdscript-fill-comment
  "Function to fill comments.
This is the function used by `godot-gdscript-fill-paragraph' to
fill comments."
  :type 'symbol
  :group 'godot-gdscript)

(defcustom godot-gdscript-fill-string-function 'godot-gdscript-fill-string
  "Function to fill strings.
This is the function used by `godot-gdscript-fill-paragraph' to
fill strings."
  :type 'symbol
  :group 'godot-gdscript)

(defcustom godot-gdscript-fill-decorator-function 'godot-gdscript-fill-decorator
  "Function to fill decorators.
This is the function used by `godot-gdscript-fill-paragraph' to
fill decorators."
  :type 'symbol
  :group 'godot-gdscript)

(defcustom godot-gdscript-fill-paren-function 'godot-gdscript-fill-paren
  "Function to fill parens.
This is the function used by `godot-gdscript-fill-paragraph' to
fill parens."
  :type 'symbol
  :group 'godot-gdscript)

(defcustom godot-gdscript-fill-docstring-style 'pep-257
  "Style used to fill docstrings.
This affects `godot-gdscript-fill-string' behavior with regards to
triple quotes positioning.

Possible values are `django', `onetwo', `pep-257', `pep-257-nn',
`symmetric', and nil.  A value of nil won't care about quotes
position and will treat docstrings a normal string, any other
value may result in one of the following docstring styles:

`django':

    \"\"\"
    Process foo, return bar.
    \"\"\"

    \"\"\"
    Process foo, return bar.

    If processing fails throw ProcessingError.
    \"\"\"

`onetwo':

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"
    Process foo, return bar.

    If processing fails throw ProcessingError.

    \"\"\"

`pep-257':

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"Process foo, return bar.

    If processing fails throw ProcessingError.

    \"\"\"

`pep-257-nn':

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"Process foo, return bar.

    If processing fails throw ProcessingError.
    \"\"\"

`symmetric':

    \"\"\"Process foo, return bar.\"\"\"

    \"\"\"
    Process foo, return bar.

    If processing fails throw ProcessingError.
    \"\"\""
  :type '(choice
          (const :tag "Don't format docstrings" nil)
          (const :tag "Django's coding standards style." django)
          (const :tag "One newline and start and Two at end style." onetwo)
          (const :tag "PEP-257 with 2 newlines at end of string." pep-257)
          (const :tag "PEP-257 with 1 newline at end of string." pep-257-nn)
          (const :tag "Symmetric style." symmetric))
  :group 'godot-gdscript
  :safe (lambda (val)
          (memq val '(django onetwo pep-257 pep-257-nn symmetric nil))))

(defun godot-gdscript-fill-paragraph (&optional justify)
  "`fill-paragraph-function' handling multi-line strings and possibly comments.
If any of the current line is in or at the end of a multi-line string,
fill the string or the paragraph of it that point is in, preserving
the string's indentation.
Optional argument JUSTIFY defines if the paragraph should be justified."
  (interactive "P")
  (save-excursion
    (cond
     ;; Comments
     ((godot-gdscript-syntax-context 'comment)
      (funcall godot-gdscript-fill-comment-function justify))
     ;; Strings/Docstrings
     ((save-excursion (or (godot-gdscript-syntax-context 'string)
                          (equal (string-to-syntax "|")
                                 (syntax-after (point)))))
      (funcall godot-gdscript-fill-string-function justify))
     ;; Decorators
     ((equal (char-after (save-excursion
                           (godot-gdscript-nav-beginning-of-statement))) ?@)
      (funcall godot-gdscript-fill-decorator-function justify))
     ;; Parens
     ((or (godot-gdscript-syntax-context 'paren)
          (looking-at (godot-gdscript-rx open-paren))
          (save-excursion
            (skip-syntax-forward "^(" (line-end-position))
            (looking-at (godot-gdscript-rx open-paren))))
      (funcall godot-gdscript-fill-paren-function justify))
     (t t))))

(defun godot-gdscript-fill-comment (&optional justify)
  "Comment fill function for `godot-gdscript-fill-paragraph'.
JUSTIFY should be used (if applicable) as in `fill-paragraph'."
  (fill-comment-paragraph justify))

(defun godot-gdscript-fill-string (&optional justify)
  "String fill function for `godot-gdscript-fill-paragraph'.
JUSTIFY should be used (if applicable) as in `fill-paragraph'."
  (let* ((str-start-pos
          (set-marker
           (make-marker)
           (or (godot-gdscript-syntax-context 'string)
               (and (equal (string-to-syntax "|")
                           (syntax-after (point)))
                    (point)))))
         (num-quotes (godot-gdscript-syntax-count-quotes
                      (char-after str-start-pos) str-start-pos))
         (str-end-pos
          (save-excursion
            (goto-char (+ str-start-pos num-quotes))
            (or (re-search-forward (rx (syntax string-delimiter)) nil t)
                (goto-char (point-max)))
            (point-marker)))
         (multi-line-p
          ;; Docstring styles may vary for oneliners and multi-liners.
          (> (count-matches "\n" str-start-pos str-end-pos) 0))
         (delimiters-style
          (pcase godot-gdscript-fill-docstring-style
            ;; delimiters-style is a cons cell with the form
            ;; (START-NEWLINES .  END-NEWLINES). When any of the sexps
            ;; is NIL means to not add any newlines for start or end
            ;; of docstring.  See `godot-gdscript-fill-docstring-style' for a
            ;; graphic idea of each style.
            (`django (cons 1 1))
            (`onetwo (and multi-line-p (cons 1 2)))
            (`pep-257 (and multi-line-p (cons nil 2)))
            (`pep-257-nn (and multi-line-p (cons nil 1)))
            (`symmetric (and multi-line-p (cons 1 1)))))
         (docstring-p (save-excursion
                        ;; Consider docstrings those strings which
                        ;; start on a line by themselves.
                        (godot-gdscript-nav-beginning-of-statement)
                        (and (= (point) str-start-pos))))
         (fill-paragraph-function))
    (save-restriction
      (narrow-to-region str-start-pos str-end-pos)
      (fill-paragraph justify))
    (save-excursion
      (when (and docstring-p godot-gdscript-fill-docstring-style)
        ;; Add the number of newlines indicated by the selected style
        ;; at the start of the docstring.
        (goto-char (+ str-start-pos num-quotes))
        (delete-region (point) (progn
                                 (skip-syntax-forward "> ")
                                 (point)))
        (and (car delimiters-style)
             (or (newline (car delimiters-style)) t)
             ;; Indent only if a newline is added.
             (indent-according-to-mode))
        ;; Add the number of newlines indicated by the selected style
        ;; at the end of the docstring.
        (goto-char (if (not (= str-end-pos (point-max)))
                       (- str-end-pos num-quotes)
                     str-end-pos))
        (delete-region (point) (progn
                                 (skip-syntax-backward "> ")
                                 (point)))
        (and (cdr delimiters-style)
             ;; Add newlines only if string ends.
             (not (= str-end-pos (point-max)))
             (or (newline (cdr delimiters-style)) t)
             ;; Again indent only if a newline is added.
             (indent-according-to-mode))))) t)

(defun godot-gdscript-fill-decorator (&optional _justify)
  "Decorator fill function for `godot-gdscript-fill-paragraph'.
JUSTIFY should be used (if applicable) as in `fill-paragraph'."
  t)

(defun godot-gdscript-fill-paren (&optional justify)
  "Paren fill function for `godot-gdscript-fill-paragraph'.
JUSTIFY should be used (if applicable) as in `fill-paragraph'."
  (save-restriction
    (narrow-to-region (progn
                        (while (godot-gdscript-syntax-context 'paren)
                          (goto-char (1- (point-marker))))
                        (point-marker)
                        (line-beginning-position))
                      (progn
                        (when (not (godot-gdscript-syntax-context 'paren))
                          (end-of-line)
                          (when (not (godot-gdscript-syntax-context 'paren))
                            (skip-syntax-backward "^)")))
                        (while (and (godot-gdscript-syntax-context 'paren)
                                    (not (eobp)))
                          (goto-char (1+ (point-marker))))
                        (point-marker)))
    (let ((paragraph-start "\f\\|[ \t]*$")
          (paragraph-separate ",")
          (fill-paragraph-function))
      (goto-char (point-min))
      (fill-paragraph justify))
    (while (not (eobp))
      (forward-line 1)
      (godot-gdscript-indent-line)
      (goto-char (line-end-position))))
  t)

;;; Skeletons

(defcustom godot-gdscript-skeleton-autoinsert nil
  "Non-nil means template skeletons will be automagically inserted.
This happens when pressing \"if<SPACE>\", for example, to prompt for
the if condition."
  :type 'boolean
  :group 'godot-gdscript
  :safe 'booleanp)

(define-obsolete-variable-alias
  'godot-gdscript-use-skeletons 'godot-gdscript-skeleton-autoinsert "24.3")

(defvar godot-gdscript-skeleton-available '()
  "Internal list of available skeletons.")

(define-abbrev-table 'godot-gdscript-mode-skeleton-abbrev-table ()
  "Abbrev table for Godot-Gdscript mode skeletons."
  :case-fixed t
  ;; Allow / inside abbrevs.
  :regexp "\\(?:^\\|[^/]\\)\\<\\([[:word:]/]+\\)\\W*"
  ;; Only expand in code.
  :enable-function (lambda ()
                     (and
                      (not (godot-gdscript-syntax-comment-or-string-p))
                      godot-gdscript-skeleton-autoinsert)))

(defmacro godot-gdscript-skeleton-define (name doc &rest skel)
  "Define a skeleton using NAME DOC and SKEL.
The skeleton will be bound to godot-gdscript-skeleton-NAME and will
be added to `godot-gdscript-mode-skeleton-abbrev-table'."
  (declare (indent 2))
  (let* ((name (symbol-name name))
         (function-name (intern (concat "godot-gdscript-skeleton-" name))))
    `(progn
       (define-abbrev godot-gdscript-mode-skeleton-abbrev-table
         ,name "" ',function-name :system t)
       (setq godot-gdscript-skeleton-available
             (cons ',function-name godot-gdscript-skeleton-available))
       (define-skeleton ,function-name
         ,(or doc
              (format "Insert %s statement." name))
         ,@skel))))

(define-abbrev-table 'godot-gdscript-mode-abbrev-table ()
  "Abbrev table for Godot-Gdscript mode."
  :parents (list godot-gdscript-mode-skeleton-abbrev-table))

(defmacro godot-gdscript-define-auxiliary-skeleton (name &optional doc &rest skel)
  "Define a auxiliary skeleton using NAME DOC and SKEL.
The skeleton will be bound to godot-gdscript-skeleton-NAME."
  (declare (indent 2))
  (let* ((name (symbol-name name))
         (function-name (intern (concat "godot-gdscript-skeleton--" name)))
         (msg (funcall (if (fboundp 'format-message) #'format-message #'format)
                       "Add `%s' clause? " name)))
    (when (not skel)
      (setq skel
            `(< ,(format "%s:" name) \n \n
                > _ \n)))
    `(define-skeleton ,function-name
       ,(or doc
            (format "Auxiliary skeleton for %s statement." name))
       nil
       (unless (y-or-n-p ,msg)
         (signal 'quit t))
       ,@skel)))

(godot-gdscript-define-auxiliary-skeleton else)

(godot-gdscript-define-auxiliary-skeleton except)

(godot-gdscript-define-auxiliary-skeleton finally)

(godot-gdscript-skeleton-define if nil
  "Condition: "
  "if " str ":" \n
  _ \n
  ("other condition, %s: "
   <
   "elif " str ":" \n
   > _ \n nil)
  '(godot-gdscript-skeleton--else) | ^)

(godot-gdscript-skeleton-define while nil
  "Condition: "
  "while " str ":" \n
  > _ \n
  '(godot-gdscript-skeleton--else) | ^)

(godot-gdscript-skeleton-define for nil
  "Iteration spec: "
  "for " str ":" \n
  > _ \n
  '(godot-gdscript-skeleton--else) | ^)

(godot-gdscript-skeleton-define try nil
  nil
  "try:" \n
  > _ \n
  ("Exception, %s: "
   <
   "except " str ":" \n
   > _ \n nil)
  resume:
  '(godot-gdscript-skeleton--except)
  '(godot-gdscript-skeleton--else)
  '(godot-gdscript-skeleton--finally) | ^)

(godot-gdscript-skeleton-define def nil
  "Function name: "
  "def " str "(" ("Parameter, %s: "
                  (unless (equal ?\( (char-before)) ", ")
                  str) "):" \n
                  "\"\"\"" - "\"\"\"" \n
                  > _ \n)

(godot-gdscript-skeleton-define class nil
  "Class name: "
  "class " str "(" ("Inheritance, %s: "
                    (unless (equal ?\( (char-before)) ", ")
                    str)
  & ")" | -2
  ":" \n
  "\"\"\"" - "\"\"\"" \n
  > _ \n)

(defun godot-gdscript-skeleton-add-menu-items ()
  "Add menu items to Godot-Gdscript->Skeletons menu."
  (let ((skeletons (sort godot-gdscript-skeleton-available 'string<)))
    (dolist (skeleton skeletons)
      (easy-menu-add-item
       nil '("Godot-Gdscript" "Skeletons")
       `[,(format
           "Insert %s" (nth 2 (split-string (symbol-name skeleton) "-")))
         ,skeleton t]))))

;;; FFAP

(defcustom godot-gdscript-ffap-setup-code
  "def __FFAP_get_module_path(module):
    try:
        import os
        path = __import__(module).__file__
        if path[-4:] == '.pyc' and os.path.exists(path[0:-1]):
            path = path[:-1]
        return path
    except:
        return ''"
  "Godot-Gdscript code to get a module path."
  :type 'string
  :group 'godot-gdscript)

(defcustom godot-gdscript-ffap-string-code
  "__FFAP_get_module_path('''%s''')\n"
  "Godot-Gdscript code used to get a string with the path of a module."
  :type 'string
  :group 'godot-gdscript)

(defun godot-gdscript-ffap-module-path (module)
  "Function for `ffap-alist' to return path for MODULE."
  (let ((process (or
                  (and (derived-mode-p 'inferior-godot-gdscript-mode)
                       (get-buffer-process (current-buffer)))
                  (godot-gdscript-shell-get-process))))
    (if (not process)
        nil
      (let ((module-file
             (godot-gdscript-shell-send-string-no-output
              (format godot-gdscript-ffap-string-code module) process)))
        (when module-file
          (substring-no-properties module-file 1 -1))))))

(eval-after-load "ffap"
  '(progn
     (push '(godot-gdscript-mode . godot-gdscript-ffap-module-path) ffap-alist)
     (push '(inferior-godot-gdscript-mode . godot-gdscript-ffap-module-path) ffap-alist)))

(defun godot-gdscript-build-shell-command (&optional path)
  "Build base shell command to run Godot Engine with the
project's base PATH. If PATH is not provided, try to find it
using the current file's directory as starting point."
  (let* ((project-path (or path (godot-gdscript-find-project-configuration))))
    (concat godot-gdscript-shell-interpreter " --path " project-path)))

(defun godot-gdscript-run-godot-editor ()
  "Run Godot Engine Editor."
  (interactive)
  (godot-gdscript-run-script
   (concat (godot-gdscript-build-shell-command) " -e")))

(defun godot-gdscript-run-project-in-godot ()
  "Run the current project in Godot Engine."
  (interactive)
  (let* ((project-path (godot-gdscript-find-project-configuration)))
    (godot-gdscript-run-script
     (godot-gdscript-build-shell-command))))

(defun godot-gdscript-run-project-in-godot-debug-mode ()
  "Run the current project in Godot Engine."
  (interactive)
  (let* ((project-path (godot-gdscript-find-project-configuration)))
    (godot-gdscript-run-script
     (concat (godot-gdscript-build-shell-command) " -d"))))

(defun godot-gdscript-run-current-scene-in-godot ()
  "Run the current script file in Godot Engine."
  (interactive)
  (godot-gdscript-run-script
   (concat (godot-gdscript-build-shell-command) " " (file-name-sans-extension (file-relative-name buffer-file-name)) ".tscn")))

(defun godot-gdscript-run-current-scene-in-godot-debug-mode ()
  "Run the current script file in Godot Engine."
  (interactive)
  (godot-gdscript-run-script
   (concat (godot-gdscript-build-shell-command) " -d " (file-name-sans-extension (file-relative-name buffer-file-name)) ".tscn")))

(defun godot-gdscript-edit-current-scene-in-godot ()
  "Run the current script file in Godot Engine."
  (interactive)
  (godot-gdscript-run-script
   (concat (godot-gdscript-build-shell-command) " -e " (file-name-sans-extension (file-relative-name buffer-file-name)) ".tscn")))

(defun godot-gdscript-run-current-script-in-godot ()
  "Run the current script file in Godot Engine.

For this to work, the script must inherit either from
\"SceneTree\" or \"MainLoop\"."
  (interactive)
  (godot-gdscript-run-script
   (concat (godot-gdscript-build-shell-command) " -s " (file-relative-name buffer-file-name))))

(defun godot-gdscript-find-project-configuration (&optional path)
  "Return the path where Godot's configuration File (\"project.godot\") is stored.

If PATH is given, starts searching by it. Otherwise, the search
starts by the current buffer path."
  ;; TODO: Handle error when project file does not exist.
  ;; TODO: This is now duplicated in Company-Godot-GDScript.
  (let ((base-path (or path default-directory)))
    (locate-dominating-file base-path
                            (lambda (parent)
                              (directory-files parent t "project.godot")))))

(defun godot-gdscript-get-project-name ()
  "Retrieves the project name from Godot's configuration file."
  (with-temp-buffer
    (insert-file-contents (concat (godot-gdscript-find-project-configuration) "project.godot"))
    (goto-char (point-min))
    (if (re-search-forward "config/name=\"\\([^\"]*\\)\"" nil t)
        (match-string 1)
      (error "Could not find the name of the project"))))

;;; Code check

(defcustom godot-gdscript-check-command
  "pyflakes"
  "Command used to check a Godot-Gdscript file."
  :type 'string
  :group 'godot-gdscript)

(defcustom godot-gdscript-check-buffer-name
  "*Godot-Gdscript check: %s*"
  "Buffer name used for check commands."
  :type 'string
  :group 'godot-gdscript)

(defvar godot-gdscript-check-custom-command nil
  "Internal use.")

(defun godot-gdscript-check (command)
  "Check a Godot-Gdscript file (default current buffer's file).
Runs COMMAND, a shell command, as if by `compile'.
See `godot-gdscript-check-command' for the default."
  (interactive
   (list (read-string "Check command: "
                      (or godot-gdscript-check-custom-command
                          (concat godot-gdscript-check-command " "
                                  (shell-quote-argument
                                   (or
                                    (let ((name (buffer-file-name)))
                                      (and name
                                           (file-name-nondirectory name)))
                                    "")))))))
  (setq godot-gdscript-check-custom-command command)
  (save-some-buffers (not compilation-ask-about-save) nil)
  (let ((process-environment (godot-gdscript-shell-calculate-process-environment))
        (exec-path (godot-gdscript-shell-calculate-exec-path)))
    (compilation-start command nil
                       (lambda (_modename)
                         (format godot-gdscript-check-buffer-name command)))))

;;; Imenu

(defvar godot-gdscript-imenu-format-item-label-function
  'godot-gdscript-imenu-format-item-label
  "Imenu function used to format an item label.
It must be a function with two arguments: TYPE and NAME.")

(defvar godot-gdscript-imenu-format-parent-item-label-function
  'godot-gdscript-imenu-format-parent-item-label
  "Imenu function used to format a parent item label.
It must be a function with two arguments: TYPE and NAME.")

(defvar godot-gdscript-imenu-format-parent-item-jump-label-function
  'godot-gdscript-imenu-format-parent-item-jump-label
  "Imenu function used to format a parent jump item label.
It must be a function with two arguments: TYPE and NAME.")

(defun godot-gdscript-imenu-format-item-label (type name)
  "Return Imenu label for single node using TYPE and NAME."
  (format "%s (%s)" name type))

(defun godot-gdscript-imenu-format-parent-item-label (type name)
  "Return Imenu label for parent node using TYPE and NAME."
  (format "%s..." (godot-gdscript-imenu-format-item-label type name)))

(defun godot-gdscript-imenu-format-parent-item-jump-label (type _name)
  "Return Imenu label for parent node jump using TYPE and NAME."
  (if (string= type "class")
      "*class definition*"
    "*function definition*"))

(defun godot-gdscript-imenu--put-parent (type name pos tree)
  "Add the parent with TYPE, NAME and POS to TREE."
  (let ((label
         (funcall godot-gdscript-imenu-format-item-label-function type name))
        (jump-label
         (funcall godot-gdscript-imenu-format-parent-item-jump-label-function type name)))
    (if (not tree)
        (cons label pos)
      (cons label (cons (cons jump-label pos) tree)))))

(defun godot-gdscript-imenu--build-tree (&optional min-indent prev-indent tree)
  "Recursively build the tree of nested definitions of a node.
Arguments MIN-INDENT, PREV-INDENT and TREE are internal and should
not be passed explicitly unless you know what you are doing."
  (setq min-indent (or min-indent 0)
        prev-indent (or prev-indent godot-gdscript-indent-offset))
  (let* ((pos (godot-gdscript-nav-backward-defun))
         (type)
         (name (when (and pos (looking-at godot-gdscript-nav-beginning-of-defun-regexp))
                 (let ((split (split-string (match-string-no-properties 0))))
                   (setq type (car split))
                   (cadr split))))
         (label (when name
                  (funcall godot-gdscript-imenu-format-item-label-function type name)))
         (indent (current-indentation))
         (children-indent-limit (+ godot-gdscript-indent-offset min-indent)))
    (cond ((not pos)
           ;; Nothing found, probably near to bobp.
           nil)
          ((<= indent min-indent)
           ;; The current indentation points that this is a parent
           ;; node, add it to the tree and stop recursing.
           (godot-gdscript-imenu--put-parent type name pos tree))
          (t
           (godot-gdscript-imenu--build-tree
            min-indent
            indent
            (if (<= indent children-indent-limit)
                ;; This lies within the children indent offset range,
                ;; so it's a normal child of its parent (i.e., not
                ;; a child of a child).
                (cons (cons label pos) tree)
              ;; Oh no, a child of a child?!  Fear not, we
              ;; know how to roll.  We recursively parse these by
              ;; swapping prev-indent and min-indent plus adding this
              ;; newly found item to a fresh subtree.  This works, I
              ;; promise.
              (cons
               (godot-gdscript-imenu--build-tree
                prev-indent indent (list (cons label pos)))
               tree)))))))

(defun godot-gdscript-imenu-create-index ()
  "Return tree Imenu alist for the current Godot-Gdscript buffer.
Change `godot-gdscript-imenu-format-item-label-function',
`godot-gdscript-imenu-format-parent-item-label-function',
`godot-gdscript-imenu-format-parent-item-jump-label-function' to
customize how labels are formatted."
  (goto-char (point-max))
  (let ((index)
        (tree))
    (while (setq tree (godot-gdscript-imenu--build-tree))
      (setq index (cons tree index)))
    index))

(defun godot-gdscript-imenu-create-flat-index (&optional alist prefix)
  "Return flat outline of the current Godot-Gdscript buffer for Imenu.
Optional argument ALIST is the tree to be flattened; when nil
`godot-gdscript-imenu-build-index' is used with
`godot-gdscript-imenu-format-parent-item-jump-label-function'
`godot-gdscript-imenu-format-parent-item-label-function'
`godot-gdscript-imenu-format-item-label-function' set to
  (lambda (type name) name)
Optional argument PREFIX is used in recursive calls and should
not be passed explicitly.

Converts this:

    ((\"Foo\" . 103)
     (\"Bar\" . 138)
     (\"decorator\"
      (\"decorator\" . 173)
      (\"wrap\"
       (\"wrap\" . 353)
       (\"wrapped_f\" . 393))))

To this:

    ((\"Foo\" . 103)
     (\"Bar\" . 138)
     (\"decorator\" . 173)
     (\"decorator.wrap\" . 353)
     (\"decorator.wrapped_f\" . 393))"
  ;; Inspired by imenu--flatten-index-alist removed in revno 21853.
  (apply
   'nconc
   (mapcar
    (lambda (item)
      (let ((name (if prefix
                      (concat prefix "." (car item))
                    (car item)))
            (pos (cdr item)))
        (cond ((or (numberp pos) (markerp pos))
               (list (cons name pos)))
              ((listp pos)
               (cons
                (cons name (cdar pos))
                (godot-gdscript-imenu-create-flat-index (cddr item) name))))))
    (or alist
        (let* ((fn (lambda (_type name) name))
               (godot-gdscript-imenu-format-item-label-function fn)
              (godot-gdscript-imenu-format-parent-item-label-function fn)
              (godot-gdscript-imenu-format-parent-item-jump-label-function fn))
          (godot-gdscript-imenu-create-index))))))

;;; Misc helpers

(defun godot-gdscript-info-current-defun (&optional include-type)
  "Return name of surrounding function with Gdscript compatible dotty syntax.
Optional argument INCLUDE-TYPE indicates to include the type of the defun.
This function can be used as the value of `add-log-current-defun-function'
since it returns nil if point is not inside a defun."
  (save-restriction
    (widen)
    (save-excursion
      (end-of-line 1)
      (let ((names)
            (starting-indentation (current-indentation))
            (starting-pos (point))
            (first-run t)
            (last-indent)
            (type))
        (catch 'exit
          (while (godot-gdscript-nav-beginning-of-defun 1)
            (when (save-match-data
                    (and
                     (or (not last-indent)
                         (< (current-indentation) last-indent))
                     (or
                      (and first-run
                           (save-excursion
                             ;; If this is the first run, we may add
                             ;; the current defun at point.
                             (setq first-run nil)
                             (goto-char starting-pos)
                             (godot-gdscript-nav-beginning-of-statement)
                             (beginning-of-line 1)
                             (looking-at-p
                              godot-gdscript-nav-beginning-of-defun-regexp)))
                      (< starting-pos
                         (save-excursion
                           (let ((min-indent
                                  (+ (current-indentation)
                                     godot-gdscript-indent-offset)))
                             (if (< starting-indentation  min-indent)
                                 ;; If the starting indentation is not
                                 ;; within the min defun indent make the
                                 ;; check fail.
                                 starting-pos
                               ;; Else go to the end of defun and add
                               ;; up the current indentation to the
                               ;; ending position.
                               (godot-gdscript-nav-end-of-defun)
                               (+ (point)
                                  (if (>= (current-indentation) min-indent)
                                      (1+ (current-indentation))
                                    0)))))))))
              (save-match-data (setq last-indent (current-indentation)))
              (if (or (not include-type) type)
                  (setq names (cons (match-string-no-properties 1) names))
                (let ((match (split-string (match-string-no-properties 0))))
                  (setq type (car match))
                  (setq names (cons (cadr match) names)))))
            ;; Stop searching ASAP.
            (and (= (current-indentation) 0) (throw 'exit t))))
        (and names
             (concat (and type (format "%s " type))
                     (mapconcat 'identity names ".")))))))

(defun godot-gdscript-info-current-symbol (&optional replace-self)
  "Return current symbol using dotty syntax.
With optional argument REPLACE-SELF convert \"self\" to current
parent defun name."
  (let ((name
         (and (not (godot-gdscript-syntax-comment-or-string-p))
              (with-syntax-table godot-gdscript-dotty-syntax-table
                (let ((sym (symbol-at-point)))
                  (and sym
                       (substring-no-properties (symbol-name sym))))))))
    (when name
      (if (not replace-self)
          name
        (let ((current-defun (godot-gdscript-info-current-defun)))
          (if (not current-defun)
              name
            (replace-regexp-in-string
             (godot-gdscript-rx line-start word-start "self" word-end ?.)
             (concat
              (mapconcat 'identity
                         (butlast (split-string current-defun "\\."))
                         ".") ".")
             name)))))))

(defun godot-gdscript-info-statement-starts-block-p ()
  "Return non-nil if current statement opens a block."
  (save-excursion
    (godot-gdscript-nav-beginning-of-statement)
    (looking-at (godot-gdscript-rx block-start))))

(defun godot-gdscript-info-statement-ends-block-p ()
  "Return non-nil if point is at end of block."
  (let ((end-of-block-pos (save-excursion
                            (godot-gdscript-nav-end-of-block)))
        (end-of-statement-pos (save-excursion
                                (godot-gdscript-nav-end-of-statement))))
    (and end-of-block-pos end-of-statement-pos
         (= end-of-block-pos end-of-statement-pos))))

(defun godot-gdscript-info-beginning-of-statement-p ()
  "Return non-nil if point is at beginning of statement."
  (= (point) (save-excursion
               (godot-gdscript-nav-beginning-of-statement)
               (point))))

(defun godot-gdscript-info-end-of-statement-p ()
  "Return non-nil if point is at end of statement."
  (= (point) (save-excursion
               (godot-gdscript-nav-end-of-statement)
               (point))))

(defun godot-gdscript-info-beginning-of-block-p ()
  "Return non-nil if point is at beginning of block."
  (and (godot-gdscript-info-beginning-of-statement-p)
       (godot-gdscript-info-statement-starts-block-p)))

(defun godot-gdscript-info-end-of-block-p ()
  "Return non-nil if point is at end of block."
  (and (godot-gdscript-info-end-of-statement-p)
       (godot-gdscript-info-statement-ends-block-p)))

(define-obsolete-function-alias
  'godot-gdscript-info-closing-block
  'godot-gdscript-info-dedenter-opening-block-position "24.4")

(defun godot-gdscript-info-dedenter-opening-block-position ()
  "Return the point of the closest block the current line closes.
Returns nil if point is not on a dedenter statement or no opening
block can be detected.  The latter case meaning current file is
likely an invalid godot-gdscript file."
  (let ((positions (godot-gdscript-info-dedenter-opening-block-positions))
        (indentation (current-indentation))
        (position))
    (while (and (not position)
                positions)
      (save-excursion
        (goto-char (car positions))
        (if (<= (current-indentation) indentation)
            (setq position (car positions))
          (setq positions (cdr positions)))))
    position))

(defun godot-gdscript-info-dedenter-opening-block-positions ()
  "Return points of blocks the current line may close sorted by closer.
Returns nil if point is not on a dedenter statement or no opening
block can be detected.  The latter case meaning current file is
likely an invalid godot-gdscript file."
  (save-excursion
    (let ((dedenter-pos (godot-gdscript-info-dedenter-statement-p)))
      (when dedenter-pos
        (goto-char dedenter-pos)
        (let* ((pairs '(("elif" "elif" "if")
                        ("else" "if" "elif" "except" "for" "while")
                        ("except" "except" "try")
                        ("finally" "else" "except" "try")))
               (dedenter (match-string-no-properties 0))
               (possible-opening-blocks (cdr (assoc-string dedenter pairs)))
               (collected-indentations)
               (opening-blocks))
          (catch 'exit
            (while (godot-gdscript-nav--syntactically
                    (lambda ()
                      (re-search-backward (godot-gdscript-rx block-start) nil t))
                    #'<)
              (let ((indentation (current-indentation)))
                (when (and (not (memq indentation collected-indentations))
                           (or (not collected-indentations)
                               (< indentation (apply #'min collected-indentations))))
                  (setq collected-indentations
                        (cons indentation collected-indentations))
                  (when (member (match-string-no-properties 0)
                                possible-opening-blocks)
                    (setq opening-blocks (cons (point) opening-blocks))))
                (when (zerop indentation)
                  (throw 'exit nil)))))
          ;; sort by closer
          (nreverse opening-blocks))))))

(define-obsolete-function-alias
  'godot-gdscript-info-closing-block-message
  'godot-gdscript-info-dedenter-opening-block-message "24.4")

(defun godot-gdscript-info-dedenter-opening-block-message  ()
  "Message the first line of the block the current statement closes."
  (let ((point (godot-gdscript-info-dedenter-opening-block-position)))
    (when point
      (save-restriction
        (widen)
        (message "Closes %s" (save-excursion
                               (goto-char point)
                               (buffer-substring
                                (point) (line-end-position))))))))

(defun godot-gdscript-info-dedenter-statement-p ()
  "Return point if current statement is a dedenter.
Sets `match-data' to the keyword that starts the dedenter
statement."
  (save-excursion
    (godot-gdscript-nav-beginning-of-statement)
    (when (and (not (godot-gdscript-syntax-context-type))
               (looking-at (godot-gdscript-rx dedenter)))
      (point))))

(defun godot-gdscript-info-line-ends-backslash-p (&optional line-number)
  "Return non-nil if current line ends with backslash.
With optional argument LINE-NUMBER, check that line instead."
  (save-excursion
    (save-restriction
      (widen)
      (when line-number
        (godot-gdscript-util-goto-line line-number))
      (while (and (not (eobp))
                  (goto-char (line-end-position))
                  (godot-gdscript-syntax-context 'paren)
                  (not (equal (char-before (point)) ?\\)))
        (forward-line 1))
      (when (equal (char-before) ?\\)
        (point-marker)))))

(defun godot-gdscript-info-beginning-of-backslash (&optional line-number)
  "Return the point where the backslashed line start.
Optional argument LINE-NUMBER forces the line number to check against."
  (save-excursion
    (save-restriction
      (widen)
      (when line-number
        (godot-gdscript-util-goto-line line-number))
      (when (godot-gdscript-info-line-ends-backslash-p)
        (while (save-excursion
                 (goto-char (line-beginning-position))
                 (godot-gdscript-syntax-context 'paren))
          (forward-line -1))
        (back-to-indentation)
        (point-marker)))))

(defun godot-gdscript-info-continuation-line-p ()
  "Check if current line is continuation of another.
When current line is continuation of another return the point
where the continued line ends."
  (save-excursion
    (save-restriction
      (widen)
      (let* ((context-type (progn
                             (back-to-indentation)
                             (godot-gdscript-syntax-context-type)))
             (line-start (line-number-at-pos))
             (context-start (when context-type
                              (godot-gdscript-syntax-context context-type))))
        (cond ((equal context-type 'paren)
               ;; Lines inside a paren are always a continuation line
               ;; (except the first one).
               (godot-gdscript-util-forward-comment -1)
               (point-marker))
              ((member context-type '(string comment))
               ;; move forward an roll again
               (goto-char context-start)
               (godot-gdscript-util-forward-comment)
               (godot-gdscript-info-continuation-line-p))
              (t
               ;; Not within a paren, string or comment, the only way
               ;; we are dealing with a continuation line is that
               ;; previous line contains a backslash, and this can
               ;; only be the previous line from current
               (back-to-indentation)
               (godot-gdscript-util-forward-comment -1)
               (when (and (equal (1- line-start) (line-number-at-pos))
                          (godot-gdscript-info-line-ends-backslash-p))
                 (point-marker))))))))

(defun godot-gdscript-info-block-continuation-line-p ()
  "Return non-nil if current line is a continuation of a block."
  (save-excursion
    (when (godot-gdscript-info-continuation-line-p)
      (forward-line -1)
      (back-to-indentation)
      (when (looking-at (godot-gdscript-rx block-start))
        (point-marker)))))

(defun godot-gdscript-info-assignment-continuation-line-p ()
  "Check if current line is a continuation of an assignment.
When current line is continuation of another with an assignment
return the point of the first non-blank character after the
operator."
  (save-excursion
    (when (godot-gdscript-info-continuation-line-p)
      (forward-line -1)
      (back-to-indentation)
      (when (and (not (looking-at (godot-gdscript-rx block-start)))
                 (and (re-search-forward (godot-gdscript-rx not-simple-operator
                                                    assignment-operator
                                                    not-simple-operator)
                                         (line-end-position) t)
                      (not (godot-gdscript-syntax-context-type))))
        (skip-syntax-forward "\s")
        (point-marker)))))

(defun godot-gdscript-info-looking-at-beginning-of-defun (&optional syntax-ppss)
  "Check if point is at `beginning-of-defun' using SYNTAX-PPSS."
  (and (not (godot-gdscript-syntax-context-type (or syntax-ppss (syntax-ppss))))
       (save-excursion
         (beginning-of-line 1)
         (looking-at godot-gdscript-nav-beginning-of-defun-regexp))))

(defun godot-gdscript-info-current-line-comment-p ()
  "Return non-nil if current line is a comment line."
  (char-equal
   (or (char-after (+ (line-beginning-position) (current-indentation))) ?_)
   ?#))

(defun godot-gdscript-info-current-line-empty-p ()
  "Return non-nil if current line is empty, ignoring whitespace."
  (save-excursion
    (beginning-of-line 1)
    (looking-at
     (godot-gdscript-rx line-start (* whitespace)
                (group (* not-newline))
                (* whitespace) line-end))
    (string-equal "" (match-string-no-properties 1))))

(defun godot-gdscript-info-encoding-from-cookie ()
  "Detect current buffer's encoding from its coding cookie.
Returns the encoding as a symbol."
  (let ((first-two-lines
         (save-excursion
           (save-restriction
             (widen)
             (goto-char (point-min))
             (forward-line 2)
             (buffer-substring-no-properties
              (point)
              (point-min))))))
    (when (string-match (godot-gdscript-rx coding-cookie) first-two-lines)
      (intern (match-string-no-properties 1 first-two-lines)))))

(defun godot-gdscript-info-encoding ()
  "Return encoding for file.
Try `godot-gdscript-info-encoding-from-cookie', if none is found then
default to utf-8."
  ;; If no encoding is defined, then it's safe to use UTF-8: Godot-Gdscript 2
  ;; uses ASCII as default while Godot-Gdscript 3 uses UTF-8.  This means that
  ;; in the worst case scenario godot-gdscript.el will make things work for
  ;; Godot-Gdscript 2 files with unicode data and no encoding defined.
  (or (godot-gdscript-info-encoding-from-cookie)
      'utf-8))


;;; Utility functions

(defun godot-gdscript-util-goto-line (line-number)
  "Move point to LINE-NUMBER."
  (goto-char (point-min))
  (forward-line (1- line-number)))

;; Stolen from org-mode
(defun godot-gdscript-util-clone-local-variables (from-buffer &optional regexp)
  "Clone local variables from FROM-BUFFER.
Optional argument REGEXP selects variables to clone and defaults
to \"^godot-gdscript-\"."
  (mapc
   (lambda (pair)
     (and (symbolp (car pair))
          (string-match (or regexp "^godot-gdscript-")
                        (symbol-name (car pair)))
          (set (make-local-variable (car pair))
               (cdr pair))))
   (buffer-local-variables from-buffer)))

(defun godot-gdscript-util-forward-comment (&optional direction)
  "Godot-Gdscript mode specific version of `forward-comment'.
Optional argument DIRECTION defines the direction to move to."
  (let ((comment-start (godot-gdscript-syntax-context 'comment))
        (factor (if (< (or direction 0) 0)
                    -99999
                  99999)))
    (when comment-start
      (goto-char comment-start))
    (forward-comment factor)))

(defun godot-gdscript-util-popn (lst n)
  "Return LST first N elements.
N should be an integer, when negative its opposite is used.
When N is bigger than the length of LST, the list is
returned as is."
  (let* ((n (min (abs n)))
         (len (length lst))
         (acc))
    (if (> n len)
        lst
      (while (< 0 n)
        (setq acc (cons (car lst) acc)
              lst (cdr lst)
              n (1- n)))
      (reverse acc))))

(defun godot-gdscript-util-strip-string (string)
  "Strip STRING whitespace and newlines from end and beginning."
  (replace-regexp-in-string
   (rx (or (: string-start (* (any whitespace ?\r ?\n)))
           (: (* (any whitespace ?\r ?\n)) string-end)))
   ""
   string))

(defun godot-gdscript-util-valid-regexp-p (regexp)
  "Return non-nil if REGEXP is valid."
  (ignore-errors (string-match regexp "") t))


(defun godot-gdscript-electric-pair-string-delimiter ()
  "Identify the delimiter for a string."
  (when (and electric-pair-mode
             (memq last-command-event '(?\" ?\'))
             (let ((count 0))
               (while (eq (char-before (- (point) count)) last-command-event)
                 (cl-incf count))
               (= count 3))
             (eq (char-after) last-command-event))
    (save-excursion (insert (make-string 2 last-command-event)))))

;;;###autoload
(define-derived-mode godot-gdscript-mode prog-mode "Godot-Gdscript"
  "Major mode for editing Godot Engine GDScript files.

\\{godot-gdscript-mode-map}"
  (set (make-local-variable 'tab-width) 4)
  (set (make-local-variable 'indent-tabs-mode) t)

  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-start-skip) "#+\\s-*")

  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)

  (set (make-local-variable 'forward-sexp-function)
       'godot-gdscript-nav-forward-sexp)

  (set (make-local-variable 'font-lock-defaults)
       '(godot-gdscript-font-lock-keywords nil nil nil nil))

  (set (make-local-variable 'syntax-propertize-function)
       godot-gdscript-syntax-propertize-function)

  (set (make-local-variable 'indent-line-function)
       #'godot-gdscript-indent-line-function)
  (set (make-local-variable 'indent-region-function) #'godot-gdscript-indent-region)
  ;; Because indentation is not redundant, we cannot safely reindent code.
  (setq-local electric-indent-inhibit t)
  (setq-local electric-indent-chars (cons ?: electric-indent-chars))

  ;; Add """ ... """ pairing to electric-pair-mode.
  (add-hook 'post-self-insert-hook
            #'godot-gdscript-electric-pair-string-delimiter 'append t)

  (set (make-local-variable 'paragraph-start) "\\s-*$")
  (set (make-local-variable 'fill-paragraph-function)
       #'godot-gdscript-fill-paragraph)

  (set (make-local-variable 'beginning-of-defun-function)
       #'godot-gdscript-nav-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function)
       #'godot-gdscript-nav-end-of-defun)

  (add-hook 'completion-at-point-functions
            #'godot-gdscript-completion-complete-at-point nil 'local)

  (add-hook 'post-self-insert-hook
            #'godot-gdscript-indent-post-self-insert-function 'append 'local)

  (set (make-local-variable 'imenu-create-index-function)
       #'godot-gdscript-imenu-create-index)

  (set (make-local-variable 'add-log-current-defun-function)
       #'godot-gdscript-info-current-defun)

  (add-hook 'which-func-functions #'godot-gdscript-info-current-defun nil t)

  (set (make-local-variable 'skeleton-further-elements)
       '((abbrev-mode nil)
         (< '(backward-delete-char-untabify (min godot-gdscript-indent-offset
                                                 (current-column))))
         (^ '(- (1+ (current-indentation))))))

  (add-to-list 'hs-special-modes-alist
               `(godot-gdscript-mode "^\\s-*\\(?:def\\|class\\)\\>" nil "#"
                             ,(lambda (_arg)
                                (godot-gdscript-nav-end-of-defun)) nil))

  (set (make-local-variable 'outline-regexp)
       (godot-gdscript-rx (* space) block-start))
  (set (make-local-variable 'outline-heading-end-regexp) ":[^\n]*\n")
  (set (make-local-variable 'outline-level)
       #'(lambda ()
           "`outline-level' function for Godot-Gdscript mode."
           (1+ (/ (current-indentation) godot-gdscript-indent-offset))))

  (godot-gdscript-skeleton-add-menu-items)

  (make-local-variable 'godot-gdscript-shell-internal-buffer)

  (when godot-gdscript-indent-guess-indent-offset
    (godot-gdscript-indent-guess-indent-offset)))



(provide 'godot-gdscript)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; godot-gdscript.el ends here
