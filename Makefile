EMACS ?= emacs
ELPA   = $(HOME)/.emacs.d/elpa

# The test targets share no artifacts, so run them concurrently; -Otarget
# keeps each target's output whole instead of interleaving three test logs.
MAKEFLAGS += -j -Otarget

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

COMPILE_FILES = my-racket-extras.el my-gptel.el my-closet.el my-granule-mode.el my-reading-mode.el

# Claude Code plugin. The marketplace manifest owns both names, and the
# CLI resolves the plugin through it. Read the names; do not repeat them.
CLAUDE           ?= claude
MARKETPLACE_JSON ?= .claude-plugin/marketplace.json
MARKETPLACE       = $(call manifest-field,.name)
PLUGIN            = $(call manifest-field,.plugins[0].name)
PLUGIN_ID         = $(PLUGIN)@$(MARKETPLACE)

# Read one field of the marketplace manifest. Stop make if the field is empty.
manifest-field = $(or $(shell jq -er '$(1)' $(MARKETPLACE_JSON) 2>/dev/null),\
  $(error No $(1) in $(MARKETPLACE_JSON)))

.PHONY: all compile test test-themes test-gptel test-claude-vterm test-reading-mode
.PHONY: test-plugin-make plugin-install plugin-update help

all: compile ## Byte-compile the Emacs Lisp files (default)

compile: ## Byte-compile the Emacs Lisp files
	$(BATCH_FULL) $(patsubst %,-l %,$(COMPILE_FILES)) \
	  --eval '(batch-byte-compile)' $(COMPILE_FILES)

test: test-themes test-gptel test-claude-vterm test-reading-mode test-plugin-make ## Run all tests

test-themes: ## Run the theme tests
	$(BATCH) -l test-my-themes.el -f ert-run-tests-batch-and-exit

test-gptel: ## Run the GPTel tests
	$(BATCH_FULL) -l test-my-gptel.el -f ert-run-tests-batch-and-exit

test-claude-vterm: ## Run the Claude vterm tests
	$(BATCH) -l test-my-claude-vterm.el -f ert-run-tests-batch-and-exit

test-reading-mode: ## Run the reading mode tests
	$(BATCH) -l test-my-reading-mode.el -f ert-run-tests-batch-and-exit

test-plugin-make: ## Run the Makefile tests
	./test-plugin-make.sh

# Register this repository as a marketplace, then install its plugin.
plugin-install: ## Install the Claude Code plugin
	$(CLAUDE) plugin marketplace add $(CURDIR)
	$(CLAUDE) plugin install --scope user $(PLUGIN_ID)

# Read the marketplace again from this repository, then update the plugin.
# Claude Code applies the update after a restart.
plugin-update: ## Update the Claude Code plugin
	$(CLAUDE) plugin marketplace update $(MARKETPLACE)
	$(CLAUDE) plugin update --scope user $(PLUGIN_ID)

# List each target that has a "## description" comment on its rule line.
# To add a target to this list, put such a comment on its rule line.
help:
	@awk -F ':.*## ' '/^[A-Za-z0-9_-]+:.*## / { printf "  %-20s %s\n", $$1, $$2 }' \
	  $(MAKEFILE_LIST)
