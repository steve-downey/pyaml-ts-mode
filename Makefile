# Development targets for pyaml-ts-mode.
#
#   make grammar   install the tree-sitter grammar the tests need
#   make compile   byte-compile the mode
#   make test      run the ERT suite
#
# Override the Emacs to use with, e.g., make EMACS=~/bin/emacs-31 test

EMACS ?= emacs
BATCH := $(EMACS) -Q --batch -L lisp -L test

.PHONY: all compile grammar test clean

all: compile test

compile:
	$(BATCH) -f batch-byte-compile lisp/pyaml-ts-mode.el

grammar:
	$(BATCH) -l pyaml-ts-mode --eval '(pyaml-ts-mode-install-grammars)'

test:
	$(BATCH) -l ert -l test/pyaml-ts-mode-tests.el \
	  -f ert-run-tests-batch-and-exit

clean:
	rm -f lisp/*.elc
