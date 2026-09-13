# Development targets for pyaml-ts-mode.
#
#   make grammar   install the tree-sitter grammar the tests need
#   make compile   byte-compile the mode
#   make lint      byte-compile with warnings as errors, as CI does
#   make test      run the ERT suite
#
# Override the Emacs to use with, e.g., make EMACS=~/bin/emacs-31 test

EMACS ?= emacs
BATCH := $(EMACS) -Q --batch -L lisp -L test
LISP  := lisp/pyaml-ts-mode.el

.PHONY: all compile lint grammar test clean

all: compile test

compile:
	$(BATCH) -f batch-byte-compile $(LISP)

lint:
	$(BATCH) --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile $(LISP)

grammar:
	$(BATCH) -l pyaml-ts-mode --eval '(pyaml-ts-mode-install-grammars)'

test:
	$(BATCH) -l ert -l test/pyaml-ts-mode-tests.el \
	  -f ert-run-tests-batch-and-exit

clean:
	rm -f lisp/*.elc
