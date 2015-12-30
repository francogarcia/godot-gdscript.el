;;; godot-gdscript.el --- Edit GDSCript code for Godot Game Engine.

;; Copyright (C) 2015 Franco Eusébio Garcia

;; Author: Franco Eusébio Garcia <francogarcia@protonmail.com>
;; Version: 0.0.1
;; Keywords: godot game engine

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

;; This is a draft to add support for GDScript in Emacs. GDScript is the
;; language which Godot Game Engine uses to prototype and implement games. Godot
;; is an open-source game engine, available at: <http://www.godotengine.org/>.

;; Package-Requires: ((dash "2.12.1"))

;;; Code

(require 'dash)

;; (defgroup godot-gdscript-mode nil
;;   "Support for the GDScript programming language, used by the Godot Game Engine (available at: <http://www.godotengine.org/>)."
;;   :group 'languages
;;   :prefix "gd")

;; (defcustom godot-gdscript-mode-modeline-display "GDScript"
;;   "String to display in Emacs modeline."
;;   :type 'string
;;   :tag "godot-gdscript-modeline-display"
;;   :group 'godot-gdscript-mode)

;; (defcustom godot-gdscript-mode-hoook nil
;;   "Hook to run when entering Godot GDScript mode."
;;   :type 'hook
;;   :tag "godot-gdscript-hook"
;;   :group 'godot-gdscript-mode)

(setq godot-gdscript-keywords
      '(
        "break"
        "class"
        "continue"
        "const"
        "extends"
        "export"
        "default"
        "do"
        "elif"
        "else"
        "for"
        "func"
        "if"
        "in"
        "onready"
        "pass"
        "return"
        "static"
        "setget"
        "self"
        "signal"
        "tool"
        "var"
        "while"
        "resume"
        "yield"))

(setq godot-gdscript-types '())

(setq godot-gdscript-types
      (-snoc godot-gdscript-types
             "null"
             "bool"
             "int"
             "float"
             "String"))

(setq godot-gdscript-types
      (-snoc godot-gdscript-types
             "Vector2"
             "Rect2"
             "Vector3"
             "Matrix32"
             "Plane"
             "Quat"
             "AABB"
             "Matrix3"
             "Transform"))

(setq godot-gdscript-types
      (-snoc godot-gdscript-types
             "Color"
             "Image"
             "NodePath"
             "RID"
             "Object"
             "InputEvent"))

(setq godot-gdscript-types
      (-snoc godot-gdscript-types
             "Array"
             "Dictionary"
             "ByteArray"
             "IntArray"
             "StringArray"
             "Vector2Array"
             "Vector3Array"
             "ColorArray"))

(setq godot-gdscript-constants
  '())

(setq godot-gdscript-events
  '())

(setq godot-gdscript-functions
  '("_init"
    "_process"
    "_input"
    "assert"
    "basefunc"
    "call"
    "funcref"
    "new"
    "load"
    "preload"
    "print"
    "range"
    "connect"
    "emit_signal"
    "str"))

(setq godot-gdscript-keywords-regexp (regexp-opt godot-gdscript-keywords 'words))
(setq godot-gdscript-type-regexp (regexp-opt godot-gdscript-types 'words))
(setq godot-gdscript-constant-regexp (regexp-opt godot-gdscript-constants 'words))
(setq godot-gdscript-event-regexp (regexp-opt godot-gdscript-events 'words))
(setq godot-gdscript-functions-regexp (regexp-opt godot-gdscript-functions 'words))

(setq godot-gdscript-font-lock-keywords
      `(
        (,godot-gdscript-type-regexp . font-lock-type-face)
        (,godot-gdscript-constant-regexp . font-lock-constant-face)
        (,godot-gdscript-event-regexp . font-lock-event-face)
        (,godot-gdscript-functions-regexp . font-lock-function-name-face)
        (,godot-gdscript-keywords-regexp . font-lock-keyword-face)
        ;; note: order above matters, because once colored, that part won't change.
        ;; in general, longer words first
        ))

(define-derived-mode godot-gdscript-mode python-mode
  "Major mode for editing GDScript files, used by the Godot Game
engine."
  (set (make-local-variable 'font-lock-defaults) '(godot-gdscript-font-lock-keywords))
  ;; Accept underscores in identifiers, by adding the '_' character
  ;; to the 'w' symbol (word constituent).
  (modify-syntax-entry ?_ "w" godot-gdscript-mode-syntax-table))

(setq godot-gdscript-keywords-regexp nil)
(setq godot-gdscript-types-regexp nil)
(setq godot-gdscript-constants-regexp nil)
(setq godot-gdscript-events-regexp nil)
(setq godot-gdscript-functions-regexp nil)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.gd\\'" . godot-gdscript-mode))

(setq godot-gdscript-mode-map
      (let ((map (make-keymap)))
      (define-key map "\C-j" 'newline-and-indent)
      map))

(provide 'godot-gdscript-mode)

;; Local Variables:
;; coding: utf-8
;; End:

;;; godot-gdscript.el ends here.
