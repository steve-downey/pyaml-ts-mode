;;; pyaml-ts-mode.el --- tree-sitter support for PYAML  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026 Free Software Foundation, Inc.
;; Copyright (C) 2026 Steve Downey

;; Author     : Randy Taylor <dev@rjt.dev>
;; Author     : Steve Downey <sdowney@sdowney.org>
;; Maintainer : Steve Downey <sdowney@sdowney.org>
;; Created    : December 2022
;; Keywords   : yaml languages tree-sitter prog-mode

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;

;;; Code:

(require 'treesit)

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-end "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")

(add-to-list
 'treesit-language-source-alist
 '(pyaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml"
         :commit "4463985dfccc640f3d6991e3396a2047610cf5f8")
 t)

(add-to-list 'treesit-load-name-override-list
             '(pyaml "libtree-sitter-pyaml" "tree_sitter_yaml"))

(defgroup pyaml-ts-mode nil
  "Major mode for editing YAML files as programming text."
  :prefix "pyaml-ts-mode-"
  :group 'languages
  :link '(url-link :tag "GitHub" "https://github.com/steve-downey/pyaml-ts-mode.git")
  :link '(emacs-commentary-link :tag "Commentary" "pyaml"))

(defcustom pyaml-ts-mode-yamllint-options nil
  "Additional options to pass to yamllint command used for Flymake support.
This should be a list of strings, each one passed as a separate argument
to the yamllint command."
  :group 'pyaml-ts-mode
  :version "31.1"
  :type '(repeat string))

;;; Drawn from neocamel: https://github.com/bbatsov/neocaml/blob/06794d8d9ae1180a37b71b02ed8eadd464129b73/neocaml.el#L125-L168
(defconst pyaml-ts-mode-version "0.0.1")

;;;###autoload
(defun pyaml-ts-mode-version ()
  "Display the current package version in the minibuffer.
Fallback to `neocaml-version' when the package version is missing.
When called from other Elisp code returns the version instead of
displaying it."
  (interactive)
  (let ((pkg-version (package-get-version)))
    (if (called-interactively-p 'interactively)
        (if pkg-version
            (message "pyaml-ts-mode %s (package: %s)" pyaml-ts-mode-version pkg-version)
          (message "pyaml-ts-mode %s" pyaml-ts-mode-version))
      (or pkg-version pyaml-ts-mode-version))))

(defconst pyaml-ts-mode-grammar-recipes
  '((pyaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml" :commit "4463985dfccc640f3d6991e3396a2047610cf5f8"))
  "Tree-sitter grammar recipes for YAML.
Each entry is a list of (LANGUAGE URL REV SOURCE-DIR).
Suitable for use as the value of `treesit-language-source-alist'.")

;;;###autoload
(defun pyaml-ts-mode-install-grammars (&optional force)
  "Install required language grammars if not already available.
With prefix argument FORCE, reinstall grammars even if they are
already installed.  This is useful after upgrading pyaml-ts-mode to a
version that requires a newer grammar."
  (interactive "P")
  (dolist (recipe pyaml-ts-mode-grammar-recipes)
    (let ((grammar (car recipe)))
      (when (or force (not (treesit-language-available-p grammar nil)))
        (message "Installing %s tree-sitter grammar..." grammar)
        ;; `treesit-language-source-alist' is dynamically scoped.
        ;; Binding it in this let expression allows
        ;; `treesit-install-language-grammar' to pick up the grammar recipes
        ;; without modifying what the user has configured themselves.
        (let ((treesit-language-source-alist pyaml-ts-mode-grammar-recipes))
          (treesit-install-language-grammar grammar))))))

(defvar pyaml-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?#  "<"  table)
    (modify-syntax-entry ?\n ">"  table)
    (modify-syntax-entry ?&  "."  table)
    (modify-syntax-entry ?*  "."  table)
    (modify-syntax-entry ?\( "."  table)
    (modify-syntax-entry ?\) "."  table)
    (modify-syntax-entry ?\' "\"" table)
    table)
  "Syntax table for `pyaml-ts-mode'.")

(defvar pyaml-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'pyaml
   :feature 'bracket
   '((["[" "]" "{" "}"]) @font-lock-bracket-face)

   :language 'pyaml
   :feature 'comment
   '((comment) @font-lock-comment-face)

   :language 'pyaml
   :feature 'constant
   '([(boolean_scalar)
      (null_scalar)
      (reserved_directive)
      (tag_directive)
      (yaml_directive)] @font-lock-constant-face)

   :language 'pyaml
   :feature 'delimiter
   '((["," ":" "-" ">" "?" "|"]) @font-lock-delimiter-face)

   :language 'pyaml
   :feature 'misc-punctuation
   '((["---" "..." "&" "*"]) @font-lock-misc-punctuation-face)

   :language 'pyaml
   :feature 'number
   '([(float_scalar) (integer_scalar)] @font-lock-number-face)

   :language 'pyaml
   :feature 'type
   '([(alias_name) (anchor_name) (tag)] @font-lock-type-face)

   :language 'pyaml
   :feature 'string
   :override t
   '([(block_scalar)
      (double_quote_scalar)
      (single_quote_scalar)
      (string_scalar)] @font-lock-string-face)

   :language 'pyaml
   :feature 'escape-sequence
   :override t
   '((escape_sequence) @font-lock-escape-face)

   :language 'pyaml
   :feature 'property
   :override t
   '((block_mapping_pair
      key: (flow_node (plain_scalar (string_scalar) @font-lock-property-use-face)))
     (block_mapping_pair
      key: (flow_node
            [(double_quote_scalar) (single_quote_scalar)] @font-lock-property-use-face))
     (flow_mapping
      (_ key: (flow_node (plain_scalar (string_scalar) @font-lock-property-use-face))))
     (flow_mapping
      (_ key:
         (flow_node
          [(double_quote_scalar) (single_quote_scalar)] @font-lock-property-use-face)))
     (flow_sequence
      (_ key: (flow_node (plain_scalar (string_scalar) @font-lock-property-use-face))))
     (flow_sequence
      (_ key:
         (flow_node
          [(double_quote_scalar) (single_quote_scalar)] @font-lock-property-use-face))))

   :language 'pyaml
   :feature 'error
   :override t
   '((ERROR) @font-lock-warning-face))
  "Tree-sitter font-lock settings for `pyaml-ts-mode'.")

(defvar pyaml-ts-mode--font-lock-feature-list
  '((comment)
    (string type)
    (constant escape-sequence number property)
    (bracket delimiter error misc-punctuation))
  "Tree-sitter font-lock feature list for `pyaml-ts-mode'.")

(defun pyaml-ts-mode--fill-paragraph (&optional justify)
  "Fill paragraph.
Behaves like `fill-paragraph', but respects block node
boundaries.  JUSTIFY is passed to `fill-paragraph'."
  (interactive "*P")
  (save-restriction
    (widen)
    (let ((node (treesit-node-at (point))))
      (pcase (treesit-node-type node)
        ("block_scalar"
         (let* ((start (treesit-node-start node))
                (end (treesit-node-end node))
                (start-marker (point-marker))
                (fill-paragraph-function nil))
           (save-excursion
             (goto-char start)
             (forward-line)
             (move-marker start-marker (point))
             (narrow-to-region (point) end))
           (fill-region start-marker end justify)))
        ("comment"
         (fill-comment-paragraph justify))))
    t))

(defun pyaml-ts-mode--defun-name (node)
  "Return the defun name of NODE.
Return nil if there is no name or if NODE is not a defun node."
  (when (equal (treesit-node-type node) "block_mapping_pair")
    (treesit-node-text (treesit-node-child-by-field-name
                        node "key")
                       t)))

(defvar pyaml-ts-mode--outline-nodes
  (rx (or "block_mapping_pair" "block_sequence_item"))
  "Node names for outline headings.")

(defun pyaml-ts-mode--outline-predicate (node)
  "Limit outlines to top-level mappings."
  (when (string-match-p pyaml-ts-mode--outline-nodes (treesit-node-type node))
    (not (treesit-node-top-level node pyaml-ts-mode--outline-nodes))))

;;; Flymake integration
(defvar-local pyaml-ts-mode--flymake-process nil
  "Store the Flymake process.")

(defun pyaml-ts-mode-flymake (report-fn &rest _args)
  "PYAML backend for Flymake.
Calls REPORT-FN directly."
  (when (process-live-p pyaml-ts-mode--flymake-process)
    (kill-process pyaml-ts-mode--flymake-process))
  (let ((yamllint (executable-find "yamllint"))
        (params (append pyaml-ts-mode-yamllint-options '("-f" "parsable" "-")))
        (source (current-buffer))
        (diagnostics-pattern (eval-when-compile
                               (rx bol (+? nonl) ":" ; every diagnostic line start with the filename
                                   (group (1+ digit)) ":" ; 1: line
                                   (group (1+ digit)) ":" ; 2: column
                                   (+ (syntax whitespace))
                                   (group (or "[error]" "[warning]")) ; 3: type
                                   (+ (syntax whitespace))
                                   (group (+? nonl)) ;; 4: message
                                   eol))))

    (if (not yamllint)
        (error "Unable to find yamllint command")
      (save-restriction
        (widen)
        (setq pyaml-ts-mode--flymake-process
              (make-process
               :name "pyaml-ts-mode-flymake"
               :noquery t
               :connection-type 'pipe
               :buffer (generate-new-buffer " *pyaml-ts-mode-flymake*")
               :command `(,yamllint ,@params)
               :sentinel
               (lambda (proc _event)
                 (when (eq 'exit (process-status proc))
                   (unwind-protect
                       (if (with-current-buffer source
                             (eq proc pyaml-ts-mode--flymake-process))
                           (with-current-buffer (process-buffer proc)
                             (goto-char (point-min))
                             (let (diags)
                               (while (search-forward-regexp
                                       diagnostics-pattern
                                       nil t)
                                 (let* ((beg
                                         (car (flymake-diag-region
                                               source
                                               (string-to-number (match-string 1))
                                               (string-to-number (match-string 2)))))
                                        (end
                                         (cdr (flymake-diag-region
                                               source
                                               (string-to-number (match-string 1))
                                               (string-to-number (match-string 2)))))
                                        (msg (match-string 4))
                                        (type (if (string= "[warning]" (match-string 3))
                                                  :warning
                                                :error)))
                                   (push (flymake-make-diagnostic
                                          source beg end type msg)
                                         diags))
                                 (funcall report-fn diags))))
                         (flymake-log :warning "Canceling obsolete check %s" proc))
                     (kill-buffer (process-buffer proc)))))))
        (process-send-region pyaml-ts-mode--flymake-process (point-min) (point-max))
        (process-send-eof pyaml-ts-mode--flymake-process)))))

;;;###autoload
(define-derived-mode pyaml-ts-mode prog-mode "PYAML"
  "Major mode for editing YAML, powered by tree-sitter."
  :group 'pyaml
  :syntax-table pyaml-ts-mode--syntax-table

  (when (treesit-ensure-installed 'pyaml)
    (setq treesit-primary-parser (treesit-parser-create 'pyaml))

    ;; Comments.
    (setq-local comment-start "# ")
    (setq-local comment-end "")
    (setq-local comment-start-skip "#+ *")

    ;; Indentation.
    (setq-local indent-tabs-mode nil)

    ;; Font-lock.
    (setq-local treesit-font-lock-settings pyaml-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list pyaml-ts-mode--font-lock-feature-list)

    (setq-local fill-paragraph-function #'pyaml-ts-mode--fill-paragraph)

    ;; Navigation.
    (setq-local treesit-defun-type-regexp "block_mapping_pair")
    (setq-local treesit-defun-name-function #'pyaml-ts-mode--defun-name)
    (setq-local treesit-defun-tactic 'top-level)

    (setq-local treesit-thing-settings
                `((pyaml
                   (list ,(rx (or "block_mapping_pair" "flow_sequence")))
                   (sentence ,"block_mapping_pair"))))

    ;; Imenu.
    (setq-local treesit-simple-imenu-settings
                '((nil "\\`block_mapping_pair\\'" nil nil)))

    ;; Outline minor mode.
    (setq-local treesit-outline-predicate #'pyaml-ts-mode--outline-predicate)

    ;; Flymake
    (add-hook 'flymake-diagnostic-functions #'pyaml-ts-mode-flymake nil 'local)

    (treesit-major-mode-setup)

    (setq-local hs-treesit-things "block_mapping_pair")
    (setq-local hs-adjust-block-end-function (lambda (_) (line-end-position)))

    ;; Use the `list' thing defined above to navigate only lists
    ;; with `C-M-n', `C-M-p', `C-M-u', `C-M-d', but not sexps
    ;; with `C-M-f', `C-M-b' neither adapt to 'show-paren-mode'
    ;; that is problematic in languages without explicit
    ;; opening/closing nodes.
    (kill-local-variable 'forward-sexp-function)
    (kill-local-variable 'show-paren-data-function)))

(derived-mode-add-parents 'pyaml-ts-mode '(pyaml-mode))

;;;###autoload
(defun pyaml-ts-mode-maybe ()
  "Enable `pyaml-ts-mode' when its grammar is available.
Also propose to install the grammar when `treesit-enabled-modes'
is t or contains the mode name."
  (declare-function treesit-language-available-p "treesit.c")
  (if (or (treesit-language-available-p 'pyaml)
          (eq treesit-enabled-modes t)
          (memq 'pyaml-ts-mode treesit-enabled-modes))
      (pyaml-ts-mode)
    (fundamental-mode)))

;;;###autoload
(when (boundp 'treesit-major-mode-remap-alist)
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . pyaml-ts-mode-maybe))
  ;; To be able to toggle between an external package and pyaml-ts-mode:
  (add-to-list 'treesit-major-mode-remap-alist
               '(yaml-mode . pyaml-ts-mode))
  (add-to-list 'treesit-major-mode-remap-alist
               '(yaml-ts-mode . pyaml-ts-mode)))

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . pyaml-ts-mode))
  )

;;; Drawn from neocaml: https://github.com/bbatsov/neocaml/blob/06794d8d9ae1180a37b71b02ed8eadd464129b73/neocaml.el#L1457-L1460

;; Eglot integration: set the language IDs that ocamllsp expects.
;; These symbol properties are consulted by eglot when it cannot
;; derive the correct language-id from the major-mode name.
(put 'pyaml-ts-mode 'eglot-language-id "yaml")

(provide 'pyaml-ts-mode)

;;; pyaml-ts-mode.el ends here
