# SPDX-License-Identifier: AGPL-3.0-or-later
EMACS ?= emacs

FILES = $(wildcard *.el)

.PHONY: all test compile i18n golden gallery

all: test compile

test:
	$(EMACS) -Q --batch -L . -l test/lispm-kit-test.el -l test/lispm-kit-i18n-test.el -l test/lispm-prebuilt-test.el -l test/lispm-ui-test.el -l test/lispm-ui-stage-test.el -l test/lispm-ui-scene-test.el -l test/lispm-ui-phase1-test.el -l test/lispm-ui-phase2-test.el -f ert-run-tests-batch-and-exit
	$(EMACS) -Q --batch -L . -l test/lispm-finder-test.el -f ert-run-tests-batch-and-exit

# A check: the .elc files are removed again, as they would shadow the
# sources of a checkout Emacs loads from.
compile:
	$(EMACS) -Q --batch -L . --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile $(FILES); \
	status=$$?; rm -f *.elc; exit $$status

# The messages each catalog of i18n/ lacks, and those it holds that the
# code no longer gives.
i18n:
	$(EMACS) -Q --batch -L . -l test/lispm-ui-test.el -f lispm-ui-test-report-catalogs

# What LispM Desktop drew, which LispM UI's tests compare its drawing with,
# from its lisp/ as LISPM_DESKTOP names it, at a commit from before LispM UI
# drew for it, with the lisp/ of LispM's agent, which it requires, as
# LISPM_AGENT names it: see test/lispm-ui-golden.el.
golden:
	@test -n "$(LISPM_DESKTOP)" -a -n "$(LISPM_AGENT)" || { echo "Say where LispM Desktop's lisp/ and LispM's are: make golden LISPM_DESKTOP=... LISPM_AGENT=..."; exit 2; }
	$(EMACS) -Q --batch -L . -L $(LISPM_DESKTOP) -L $(LISPM_AGENT) -l test/lispm-ui-golden.el -f lispm-ui-golden-write

# Every control drawn, in each of its states, as SVG in gallery/ (or where
# GALLERY says), with gallery/index.html showing them: to look at.
gallery:
	$(EMACS) -Q --batch -L . -l test/lispm-ui-gallery.el -f lispm-ui-gallery-write
