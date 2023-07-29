
EMACS?=emacs
EMACS_BATCH=$(EMACS) --batch -Q
EMACS_MAJOR_VERSION = $(shell $(EMACS) -batch -eval '(princ emacs-major-version)')
MY_EMACS_FILES = my-racket-extras.elc

.PHONY: all

all: $(MY_EMACS_FILES)

my-racket-extras.elc : my-racket-extras.el
	$(EMACS_BATCH) -f batch-byte-compile $<

