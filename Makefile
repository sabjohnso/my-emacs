
EMACS?=emacs
EMACS_BATCH=$(EMACS) --batch --load=my-init.el 
EMACS_MAJOR_VERSION = $(shell $(EMACS)  -batch -eval '(princ emacs-major-version)')
MY_EMACS_FILES = my-racket-extras.elc my-closet.elc

.PHONY: all

all: $(MY_EMACS_FILES)

my-racket-extras.elc : my-racket-extras.el
	$(EMACS_BATCH) -f batch-byte-compile $<

my-closet.elc : my-closet.el
	$(EMACS_BATCH) -f batch-byte-compile $<

