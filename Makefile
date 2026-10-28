# SPDX-License-Identifier: AGPL-3.0-or-later
EMACS ?= emacs
# LispM Kit, whose engine translates LispM UI's words: a checkout beside
# this one by default.
LISPM_KIT ?= ../lispm-kit

FILES = $(wildcard *.el)

.PHONY: all test compile i18n golden gallery

all: test compile

test:
	$(EMACS) -Q --batch -L . -L $(LISPM_KIT) -l test/lispm-ui-test.el -f ert-run-tests-batch-and-exit

# A check: the .elc files are removed again, as they would shadow the
# sources of a checkout Emacs loads from.
compile:
	$(EMACS) -Q --batch -L . -L $(LISPM_KIT) --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile $(FILES); \
	status=$$?; rm -f *.elc $(LISPM_KIT)/*.elc; exit $$status

# The messages each catalog of i18n/ lacks, and those it holds that the
# code no longer gives.
i18n:
	$(EMACS) -Q --batch -L . -L $(LISPM_KIT) -l test/lispm-ui-test.el -f lispm-ui-test-report-catalogs

# What LispM Desktop drew, which LispM UI's tests compare its drawing with,
# from its lisp/ as LISPM_DESKTOP names it, at a commit from before LispM UI
# drew for it: see test/lispm-ui-golden.el.
golden:
	@test -n "$(LISPM_DESKTOP)" || { echo "Say where LispM Desktop's lisp/ is: make golden LISPM_DESKTOP=..."; exit 2; }
	$(EMACS) -Q --batch -L $(LISPM_KIT) -L . -L $(LISPM_DESKTOP) -l test/lispm-ui-golden.el -f lispm-ui-golden-write

# Every control drawn, in each of its states, as SVG in gallery/ (or where
# GALLERY says), with gallery/index.html showing them: to look at.
gallery:
	$(EMACS) -Q --batch -L . -L $(LISPM_KIT) -l test/lispm-ui-gallery.el -f lispm-ui-gallery-write
