;;; matsurika-mode.el --- Defines a major mode for matsurika janet -*- lexical-binding: t; -*-

;; Lots copied from janet-mode.el by Adam Schwalm.
;; https://github.com/ALSchwalm/janet-mode

;; This program is free software: you can redistribute it and/or modify
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

;;; Code:

(require 'cl-lib)
(require 'clojure-mode)

(defgroup matsurika nil
  "A mode for matsurika"
  :group 'languages)

(defvar matsurika-mode-syntax-table
  (let ((table (make-syntax-table clojure-mode-syntax-table)))
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)

    (modify-syntax-entry ?: "_" table)
    (modify-syntax-entry ?` "\"" table)

    (modify-syntax-entry ?| "_")

    (modify-syntax-entry ?? "_" table)
    (modify-syntax-entry ?! "_" table)
    (modify-syntax-entry ?. "_" table)
    (modify-syntax-entry ?@ "_" table)
    (modify-syntax-entry ?\; "_" table)
    (modify-syntax-entry ?* "_" table)

    (modify-syntax-entry ?\{ "(}")
    (modify-syntax-entry ?\} "){")

    table))

(defconst matsurika-symbol '(one-or-more (or (syntax word) (syntax symbol)))
  "Regex representation of a Matsurika symbol.
A Matsurika symbol is a collection of words or symbol characters as determined by
the syntax table.  This allows us to keep things like '-' in the symbol part of
the syntax table, so `forward-word' works as expected.")

(defconst matsurika-start-of-sexp '("(" (zero-or-more (or space "\n"))))

(defconst matsurika-macro-decl-forms '("defmacro" "defmacro-"))

(defconst matsurika-normal-function-decl-forms '("defun" "defun-"))

(defconst matsurika-function-decl-forms
  `(,@matsurika-normal-function-decl-forms ,@matsurika-macro-decl-forms "varfn" "fn"))

(defconst matsurika-function-pattern
  (rx-to-string `(sequence ,@matsurika-start-of-sexp
                           (group ,matsurika-symbol)))
  "The regex to identify matsurika function names.")


(defconst matsurika-var-decl-forms
  '("var" "var-" "def" "def-" "defglobal" "varglobal" "default" "dyn"))

(defconst matsurika-variable-declaration-pattern
  (rx-to-string `(sequence ,@matsurika-start-of-sexp
                           (or ,@matsurika-var-decl-forms)
                           (one-or-more space) (group ,matsurika-symbol)))
  "The regex to identify variable declarations.")

(defconst matsurika-keyword-pattern
  (rx-to-string `(group symbol-start ":" ,matsurika-symbol)))

(defconst matsurika-error-pattern
  (rx-to-string `(sequence ,@matsurika-start-of-sexp
                           (group symbol-start "error" symbol-end))))

(defconst matsurika-constant-pattern
  (rx-to-string `(group symbol-start
                        (group (or "true" "false" "nil")) symbol-end)))

(defconst matsurika-imenu-generic-expression
  `((nil
     ,(rx-to-string `(sequence line-start ,@matsurika-start-of-sexp
                               (or ,@matsurika-normal-function-decl-forms)
                               (one-or-more space)
                               (group ,matsurika-symbol)))
     1)
    ("Variables"
     ,(rx-to-string `(sequence line-start ,@matsurika-start-of-sexp
                               (or ,@matsurika-var-decl-forms)
                               (one-or-more space)
                               (group ,matsurika-symbol)))
     1)
    ("Macros"
     ,(rx-to-string `(sequence line-start ,@matsurika-start-of-sexp
                               (or ,@matsurika-macro-decl-forms)
                               (one-or-more space)
                               (group ,matsurika-symbol)))
     1)))

(defcustom matsurika-special-forms
  `(
    ;; Not all explicitly special forms, but included for
    ;; symmetry with other lisp-modes

    "->"
    "->>"
    "-?>"
    "-?>>"
    "as->"
    "as?->"
    "-<"
    "-<<"
    "break"
    "cond"
    "coro"
    "do"
    "each"
    "fn"
    "for"
    "generate"
    "if"
    "if-let"
    "if-not"
    "import"
    "let"
    "loop"
    "match"
    "quasiquote"
    "quote"
    "require"
    "seq"
    "set"
    "setdyn"
    "splice"
    "try"
    "unless"
    "unquote"
    "upscope"
    "var"
    "when"
    "when-let"
    "while"
    "with"
    "with-dyns"
    "with-syms"
    "with-vars"
    "$"
    "$*"

    ,@matsurika-var-decl-forms
    ,@matsurika-function-decl-forms)
  "List of Matsurika special forms."
  :type '(repeat string)
  :group 'matsurika)

(defconst matsurika-special-form-pattern
  (let ((builtins (cons 'or matsurika-special-forms)))
    (rx-to-string `(or (sequence ,@matsurika-start-of-sexp (group ,builtins) symbol-end))))
  "The regex to identify builtin Matsurika special forms.")

(defconst matsurika-shorthand-pattern
  (rx-to-string `(or (sequence (group (or "|" "$"))))))

(defconst matsurika-highlights
  `((,matsurika-special-form-pattern . (1 font-lock-keyword-face))
    (,matsurika-function-pattern . (1 font-lock-function-name-face))
    (,matsurika-variable-declaration-pattern . (1 font-lock-variable-name-face))
    (,matsurika-error-pattern . (1 font-lock-warning-face))
    (,matsurika-constant-pattern . (1 font-lock-constant-face))
    (,matsurika-keyword-pattern . (1 font-lock-builtin-face))
    (,matsurika-shorthand-pattern . (1 font-lock-keyword-face))))

(defun matsurika--generate-builtin-ht ()
  (interactive)
  (load-file "./docs.txt")
  (setq mts-ht (make-hash-table :test #'equal))
  (cl-loop for doc in mts-docs
           do (puthash (car doc) (cdr doc) mts-ht)))

(defun company-matsurika-builtins (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-matsurika-builtins))
    (prefix (when (eq major-mode 'matsurika-mode)
              (company-grab-symbol)))
    (candidates (cl-remove-if-not
                 (lambda (c) (string-prefix-p arg c))
                 (hash-table-keys mts-ht)))
    (meta (format (gethash arg mts-ht)))))

(defun matsurika--set-indent ()
  (interactive)
  (put 'if 'lisp-indent-function 2))

(defun matsurika-mode-get-docs ()
  (interactive)
  (message "%s"
           (gethash (thing-at-point 'symbol) mts-ht)))

(defvar matsurika-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c C-d") #'matsurika-mode-get-docs)
    map))

(unless (boundp 'mts-ht)
  (matsurika--generate-builtin-ht))

;;;###autoload
(define-derived-mode matsurika-mode clojure-mode "matsurika"
  "Major mode for the matsurika flavor of the matsurika language"
  :syntax-table matsurika-mode-syntax-table
  (setq-local font-lock-defaults '(matsurika-highlights))
  (setq-local comment-start "#")
  (setq-local comment-start-skip "#+ *")
  (setq-local comment-use-syntax t)
  (setq-local comment-end "")
  (setq-local imenu-generic-expression matsurika-imenu-generic-expression)
  (matsurika--set-indent)
  (add-to-list 'company-backends #'company-matsurika-builtins)
  (use-local-map matsurika-mode-map))


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mts\\'" . matsurika-mode))

(provide 'matsurika-mode)
;;; matsurika-mode.el ends here
