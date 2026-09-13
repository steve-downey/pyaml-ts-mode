;;; pyaml-ts-mode-tests.el --- Tests for pyaml-ts-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Steve Downey

;; Author     : Steve Downey <sdowney@sdowney.org>
;; Maintainer : Steve Downey <sdowney@sdowney.org>
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
;; Run with `make test', or:
;;
;;   emacs -Q --batch -L lisp -L test -l ert \
;;     -l test/pyaml-ts-mode-tests.el -f ert-run-tests-batch-and-exit
;;
;; Tests that need the tree-sitter grammar skip themselves when it is not
;; installed; `make grammar' installs it.  The Flymake tests additionally
;; need the yamllint executable.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'imenu)
(require 'treesit)
(require 'flymake)
(require 'pyaml-ts-mode)

(defconst pyaml-ts-mode-tests--sample "\
# a comment
version: 2
metadata:
  name: pyaml
  \"quoted key\": quoted value
  labels: {app: pyaml}
  ports: [80, 443]
anchors:
  base: &base
    retries: 3
    timeout: 1.5
  derived:
    enabled: true
    nothing: null
    alias: *base
script: |
  echo one
  echo two
description: >
  a folded block scalar with enough words in it that refilling it to a narrow fill column has real work to do
items:
  - first
  - name: second
"
  "A YAML document exercising the constructs the mode has rules for.")

(defmacro pyaml-ts-mode-tests--with-sample (&rest body)
  "Run BODY in a `pyaml-ts-mode' buffer holding the sample document.
Skip the test when the tree-sitter grammar is not installed."
  (declare (indent 0) (debug t))
  `(progn
     (skip-unless (treesit-language-available-p 'pyaml))
     (with-temp-buffer
       (insert pyaml-ts-mode-tests--sample)
       (goto-char (point-min))
       (pyaml-ts-mode)
       ,@body)))

(defun pyaml-ts-mode-tests--face-at (regexp &optional group)
  "Return the face at the start of the first match for REGEXP.
GROUP is the subexpression to use, and defaults to 0."
  (goto-char (point-min))
  (re-search-forward regexp)
  (get-text-property (match-beginning (or group 0)) 'face))

(defun pyaml-ts-mode-tests--pair-at-point ()
  "Return the innermost block mapping pair around point."
  (treesit-parent-until (treesit-node-at (point))
                        (lambda (node)
                          (equal (treesit-node-type node) "block_mapping_pair"))
                        t))

(defun pyaml-ts-mode-tests--body-of-block-scalar ()
  "Return the lines of the sample's folded block scalar.
The scalar body runs from the line after its `description: >' header
to the next top-level key."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^description: >\n")
    (let ((start (point))
          (end (progn (re-search-forward "^items:") (match-beginning 0))))
      (split-string (buffer-substring start end) "\n" t))))

(defun pyaml-ts-mode-tests--lint (content &optional timeout)
  "Lint CONTENT with `pyaml-ts-mode-flymake' and return its reports.
Each element is the list of diagnostics passed to REPORT-FN by one
call, so the length of the result is the number of times the backend
reported for a single check.  Give up after TIMEOUT seconds, 20 by
default.

The backend is called directly, rather than through `flymake-mode',
because Flymake offers no way to wait for a check to finish:
`flymake-running-backends' stays non-empty once a backend has run --
Flymake only clears that state when disabling a backend, not when one
reports."
  (with-temp-buffer
    (insert content)
    (pyaml-ts-mode)
    (let ((reports nil)
          (deadline (+ (float-time) (or timeout 20))))
      (pyaml-ts-mode-flymake (lambda (diags &rest _) (push diags reports)))
      (let ((proc pyaml-ts-mode--flymake-process))
        (while (and (process-live-p proc) (< (float-time) deadline))
          (accept-process-output proc 0.05))
        ;; The sentinel reports once the process has exited.
        (while (and (null reports) (< (float-time) deadline))
          (accept-process-output nil 0.05)))
      (nreverse reports))))


;;; Mode setup

(ert-deftest pyaml-ts-mode-test-mode-setup ()
  "The mode turns on, derives from `prog-mode', and parses as pyaml."
  (pyaml-ts-mode-tests--with-sample
    (should (eq major-mode 'pyaml-ts-mode))
    (should (provided-mode-derived-p 'pyaml-ts-mode 'prog-mode))
    (should (eq (treesit-parser-language treesit-primary-parser) 'pyaml))))

(ert-deftest pyaml-ts-mode-test-sample-parses-cleanly ()
  "The sample document parses without an ERROR node.
This is the canary for a grammar pin that no longer matches the
font-lock queries below."
  (pyaml-ts-mode-tests--with-sample
    (should-not (treesit-search-subtree (treesit-buffer-root-node) "ERROR"))))

(ert-deftest pyaml-ts-mode-test-mode-is-a-yaml-mode ()
  "The mode is reachable as a YAML mode by the usual means."
  (should (string-match-p (car (rassq 'pyaml-ts-mode auto-mode-alist)) "foo.yaml"))
  (should (string-match-p (car (rassq 'pyaml-ts-mode auto-mode-alist)) "foo.yml"))
  (should (provided-mode-derived-p 'pyaml-ts-mode 'pyaml-mode))
  (should (equal (get 'pyaml-ts-mode 'eglot-language-id) "yaml")))


;;; Settings carried over from upstream yaml-ts-mode

(ert-deftest pyaml-ts-mode-test-comment-settings ()
  "Comments are line comments starting with `#'.
`comment-start-line-regexp' is set so that Emacs can tell line
comments from block comments.  It is read through `buffer-local-value'
because Emacs 31 has no such variable in its core: the mode sets it for
the benefit of later versions, which is what upstream does, and on 31
it is simply an unused buffer-local."
  (pyaml-ts-mode-tests--with-sample
    (should (equal comment-start "# "))
    (should (equal comment-end ""))
    (should (equal comment-start-skip "#+ *"))
    (should (equal (buffer-local-value 'comment-start-line-regexp
                                       (current-buffer))
                   comment-start-skip))))

(ert-deftest pyaml-ts-mode-test-indentation-settings ()
  "YAML cannot be indented with tabs, and a tab stop is two columns."
  (pyaml-ts-mode-tests--with-sample
    (should-not indent-tabs-mode)
    (should (= tab-width 2))))

(ert-deftest pyaml-ts-mode-test-setup-skipped-when-treesit-not-ready ()
  "Mode setup bails out when tree-sitter is not ready for the buffer.
The `treesit-ready-p' guard also enforces `treesit-max-buffer-size',
so an oversized buffer gets the mode without a parser rather than a
half-configured one."
  (skip-unless (treesit-language-available-p 'pyaml))
  (with-temp-buffer
    (insert pyaml-ts-mode-tests--sample)
    (let ((treesit-max-buffer-size 10)
          (warning-minimum-log-level :emergency))
      (pyaml-ts-mode))
    (should (eq major-mode 'pyaml-ts-mode))
    (should-not treesit-primary-parser)
    (should-not (local-variable-p 'treesit-font-lock-settings))))


;;; Font lock

(ert-deftest pyaml-ts-mode-test-font-lock ()
  "Each font-lock feature paints the node type it claims.
Only features up to `treesit-font-lock-level', 3 by default, are
checked here; the rest are in
`pyaml-ts-mode-test-font-lock-decorative-features'."
  (pyaml-ts-mode-tests--with-sample
    (font-lock-ensure)
    (should (eq (pyaml-ts-mode-tests--face-at "# a comment")
                'font-lock-comment-face))
    (should (eq (pyaml-ts-mode-tests--face-at "^version")
                'font-lock-property-use-face))
    (should (eq (pyaml-ts-mode-tests--face-at "\"quoted key\"")
                'font-lock-property-use-face))
    (should (eq (pyaml-ts-mode-tests--face-at "quoted value")
                'font-lock-string-face))
    (should (eq (pyaml-ts-mode-tests--face-at "echo one")
                'font-lock-string-face))
    (should (eq (pyaml-ts-mode-tests--face-at "&\\(base\\)" 1)
                'font-lock-type-face))
    (should (eq (pyaml-ts-mode-tests--face-at "\\*\\(base\\)" 1)
                'font-lock-type-face))
    (should (eq (pyaml-ts-mode-tests--face-at "version: \\(2\\)" 1)
                'font-lock-number-face))
    (should (eq (pyaml-ts-mode-tests--face-at "\\(1\\.5\\)" 1)
                'font-lock-number-face))
    (should (eq (pyaml-ts-mode-tests--face-at "\\(true\\)" 1)
                'font-lock-constant-face))
    (should (eq (pyaml-ts-mode-tests--face-at "\\(null\\)" 1)
                'font-lock-constant-face))))

(ert-deftest pyaml-ts-mode-test-font-lock-decorative-features ()
  "Brackets, delimiters and punctuation are painted at level 4.
They sit in the last group of `treesit-font-lock-feature-list', so
they are off at the default `treesit-font-lock-level' of 3."
  (pyaml-ts-mode-tests--with-sample
    (let ((treesit-font-lock-level 4))
      (treesit-font-lock-recompute-features)
      (font-lock-flush)
      (font-lock-ensure)
      (should (eq (pyaml-ts-mode-tests--face-at "\\({\\)app" 1)
                  'font-lock-bracket-face))
      (should (eq (pyaml-ts-mode-tests--face-at "ports: \\(\\[\\)" 1)
                  'font-lock-bracket-face))
      (should (eq (pyaml-ts-mode-tests--face-at "version\\(:\\)" 1)
                  'font-lock-delimiter-face))
      (should (eq (pyaml-ts-mode-tests--face-at "^  \\(-\\) first" 1)
                  'font-lock-delimiter-face))
      (should (eq (pyaml-ts-mode-tests--face-at "\\(&\\)base" 1)
                  'font-lock-misc-punctuation-face)))))

(ert-deftest pyaml-ts-mode-test-font-lock-feature-list-is-complete ()
  "Every feature in the font-lock settings appears in the feature list.
A feature missing from the list is a rule that never runs."
  (let ((features (delete-dups
                   (mapcar #'treesit-font-lock-setting-feature
                           pyaml-ts-mode--font-lock-settings)))
        (listed (apply #'append pyaml-ts-mode--font-lock-feature-list)))
    (dolist (feature features)
      (should (memq feature listed)))))


;;; Navigation, imenu, outline, hideshow

(ert-deftest pyaml-ts-mode-test-defun-name ()
  "The defun name of a mapping pair is its key."
  (pyaml-ts-mode-tests--with-sample
    (re-search-forward "^metadata")
    (should (equal (treesit-defun-name (treesit-defun-at-point)) "metadata"))
    (should-not (pyaml-ts-mode--defun-name (treesit-buffer-root-node)))))

(ert-deftest pyaml-ts-mode-test-imenu-lists-keys ()
  "Imenu offers the document's mapping keys."
  (pyaml-ts-mode-tests--with-sample
    (let ((names (mapcar #'car (imenu--make-index-alist))))
      (dolist (key '("version" "metadata" "anchors" "script" "items"))
        (should (member key names))))))

(ert-deftest pyaml-ts-mode-test-outline-predicate-limits-to-top-level ()
  "Only top-level mappings are outline headings."
  (pyaml-ts-mode-tests--with-sample
    (re-search-forward "^metadata")
    (should (pyaml-ts-mode--outline-predicate
             (pyaml-ts-mode-tests--pair-at-point)))
    (re-search-forward "^  name:")
    (should-not (pyaml-ts-mode--outline-predicate
                 (pyaml-ts-mode-tests--pair-at-point)))))

(ert-deftest pyaml-ts-mode-test-hideshow-settings ()
  "Hideshow folds mapping pairs, ending each block at end of line."
  (pyaml-ts-mode-tests--with-sample
    (should (equal hs-treesit-things "block_mapping_pair"))
    (re-search-forward "^metadata")
    (should (= (funcall hs-adjust-block-end-function (point))
               (line-end-position)))))

(ert-deftest pyaml-ts-mode-test-sexp-navigation-is-left-alone ()
  "The mode does not take over `C-M-f' or `show-paren-mode'.
YAML has no explicit opening and closing nodes, so the `list' thing is
used for list motion instead and these stay global."
  (pyaml-ts-mode-tests--with-sample
    (should-not (local-variable-p 'forward-sexp-function))
    (should-not (local-variable-p 'show-paren-data-function))))


;;; Filling

(ert-deftest pyaml-ts-mode-test-fill-paragraph-in-block-scalar ()
  "Filling inside a block scalar refills the body and nothing else."
  (pyaml-ts-mode-tests--with-sample
    (let ((fill-column 40))
      (should (= (length (pyaml-ts-mode-tests--body-of-block-scalar)) 1))
      (goto-char (point-min))
      (re-search-forward "^description: >\n")
      (fill-paragraph)
      (let ((lines (pyaml-ts-mode-tests--body-of-block-scalar)))
        ;; The body is now several lines, each still inside the scalar.
        (should (> (length lines) 1))
        (dolist (line lines)
          (should (string-prefix-p "  " line))
          (should (<= (length line) fill-column))))
      ;; The header line and the key after the scalar are untouched.
      (goto-char (point-min))
      (should (re-search-forward "^description: >$" nil t))
      (should (re-search-forward "^items:$" nil t)))))

(ert-deftest pyaml-ts-mode-test-fill-paragraph-in-comment ()
  "Filling inside a comment fills the comment."
  (pyaml-ts-mode-tests--with-sample
    (goto-char (point-min))
    (end-of-line)
    (insert " with some more words appended to make the comment long enough to wrap")
    (let ((fill-column 40))
      (fill-paragraph)
      (should (> (count-lines (point-min) (point)) 1))
      ;; Every line of the filled comment is still a comment.
      (dolist (line (split-string
                     (buffer-substring (point-min) (point)) "\n" t))
        (should (string-prefix-p "#" line))))))


;;; Flymake

(ert-deftest pyaml-ts-mode-test-flymake-backend-is-installed ()
  "The mode adds its backend to `flymake-diagnostic-functions'."
  (pyaml-ts-mode-tests--with-sample
    (should (memq 'pyaml-ts-mode-flymake flymake-diagnostic-functions))))

(ert-deftest pyaml-ts-mode-test-flymake-reports-diagnostics ()
  "yamllint output becomes diagnostics, reported in a single call."
  (skip-unless (treesit-language-available-p 'pyaml))
  (skip-unless (executable-find "yamllint"))
  (let* ((reports (pyaml-ts-mode-tests--lint "---\na: 1   \nb: 2   \n"))
         (diags (car reports)))
    ;; One check, one report, carrying both of yamllint's findings.
    (should (= (length reports) 1))
    (should (= (length diags) 2))
    (dolist (diag diags)
      (should (memq (flymake-diagnostic-type diag) '(:error :warning))))
    (should (string-match-p "trailing spaces"
                            (mapconcat #'flymake-diagnostic-text diags " ")))))

(ert-deftest pyaml-ts-mode-test-flymake-reports-once-with-nothing-to-say ()
  "A check that finds nothing still reports, with no diagnostics.
REPORT-FN is called once after the search loop.  The backend used to
call it inside the loop, which meant no call at all when there were no
matches, so Flymake went on showing the previous check's diagnostics."
  (skip-unless (treesit-language-available-p 'pyaml))
  (skip-unless (executable-find "yamllint"))
  (let ((reports (pyaml-ts-mode-tests--lint "---\na: 1\n")))
    (should (= (length reports) 1))
    (should-not (car reports))))

(ert-deftest pyaml-ts-mode-test-flymake-without-yamllint ()
  "The backend complains rather than failing silently."
  (skip-unless (treesit-language-available-p 'pyaml))
  (cl-letf (((symbol-function 'executable-find) #'ignore))
    (with-temp-buffer
      (insert "---\na: 1\n")
      (pyaml-ts-mode)
      (should-error (pyaml-ts-mode-flymake #'ignore) :type 'error))))


;;; Package plumbing

(ert-deftest pyaml-ts-mode-test-version ()
  "The version command returns a version string when not interactive."
  (should (stringp (pyaml-ts-mode-version))))

(ert-deftest pyaml-ts-mode-test-grammar-recipe-matches-source-alist ()
  "The recipe used by the install command matches the registered source.
Two copies of the pin drift apart otherwise, and the mode would load a
different grammar than the one the install command builds."
  (should (equal (assq 'pyaml pyaml-ts-mode-grammar-recipes)
                 (assq 'pyaml treesit-language-source-alist)))
  (should (equal (assq 'pyaml treesit-load-name-override-list)
                 '(pyaml "libtree-sitter-pyaml" "tree_sitter_yaml"))))

(provide 'pyaml-ts-mode-tests)

;;; pyaml-ts-mode-tests.el ends here
