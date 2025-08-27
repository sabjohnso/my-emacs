
EMACS?=emacs
EMACS_BATCH=$(EMACS) --batch -l my-base-packages.el -L $HOME/.emacs.d/elpa
EMACS_MAJOR_VERSION = $(shell $(EMACS)  -batch -eval '(princ emacs-major-version)')
MY_EMACS_FILES = my-racket-extras.elc my-closet.elc my-gptel.elc

.PHONY: all

all: $(MY_EMACS_FILES)

my-racket-extras.elc : my-racket-extras.el
	$(EMACS_BATCH) -f batch-byte-compile $<

my-gptel.elc : my-gptel.el
	$(EMACS_BATCH) -f batch-byte-compile $<

my-closet.elc : my-closet.el
	$(EMACS_BATCH) -f batch-byte-compile $<

