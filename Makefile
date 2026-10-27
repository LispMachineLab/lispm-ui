# SPDX-License-Identifier: AGPL-3.0-or-later
EMACS ?= emacs
# LispM Kit, whose engine translates LispM UI's words: a checkout beside
# this one by default.
LISPM_KIT ?= ../lispm-kit

FILES = $(wildcard *.el)

.PHONY: all test compile i18n

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
