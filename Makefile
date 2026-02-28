EMACS ?= emacs
ELPA   = $(HOME)/.emacs.d/elpa

# ELPA package directories needed for batch loading
GPTEL_DIR     = $(firstword $(wildcard $(ELPA)/gptel-[0-9]*))
TRANSIENT_DIR = $(firstword $(wildcard $(ELPA)/transient-[0-9]*))
COMPAT_DIR    = $(firstword $(wildcard $(ELPA)/compat-[0-9]*))
PCRE2EL_DIR   = $(firstword $(wildcard $(ELPA)/pcre2el-[0-9]*))

# use-package :pin melpa requires package-archives to include MELPA
PKG_INIT = --eval '(require (quote package))' \
  --eval '(add-to-list (quote package-archives) (cons "melpa" "https://melpa.org/packages/"))' \
  --eval '(package-initialize)'

BATCH = $(EMACS) --batch -L .
BATCH_FULL = $(BATCH) \
  -L $(GPTEL_DIR) \
  -L $(TRANSIENT_DIR) \
  -L $(COMPAT_DIR) \
  -L $(PCRE2EL_DIR) \
  $(PKG_INIT)

COMPILE_FILES = my-racket-extras.el my-gptel.el my-closet.el

.PHONY: all compile test test-themes test-gptel

all: compile

compile:
	$(BATCH_FULL) $(patsubst %,-l %,$(COMPILE_FILES)) \
	  --eval '(batch-byte-compile)' $(COMPILE_FILES)

test: test-themes test-gptel

test-themes:
	$(BATCH) -l test-my-themes.el -f ert-run-tests-batch-and-exit

test-gptel:
	$(BATCH_FULL) -l test-my-gptel.el -f ert-run-tests-batch-and-exit
