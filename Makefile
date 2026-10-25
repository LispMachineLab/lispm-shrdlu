EMACS ?= emacs
# LispM Kit, with which SHRDLU declares its commands: a checkout beside
# this one by default.
LISPM_KIT ?= ../lispm-kit
# Emacs Common Lisp, which runs SHRDLU, and LispMachineLab's www-shrdlu,
# SHRDLU's Common Lisp: checkouts beside this one by default.
EMACS_CL ?= ../emacs-cl
SHRDLU_SOURCE ?= ../www-shrdlu

.PHONY: test compile

test:
	SHRDLU_SOURCE=$(SHRDLU_SOURCE) $(EMACS) -Q --batch -L . -L $(LISPM_KIT) -L $(EMACS_CL) \
	  -l test/lispm-shrdlu-test.el -f ert-run-tests-batch-and-exit

# A check: the .elc file is removed again, as it would shadow the source
# of a checkout Emacs loads from.
compile:
	$(EMACS) -Q --batch -L . -L $(LISPM_KIT) -L $(EMACS_CL) --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile lispm-shrdlu.el; \
	status=$$?; rm -f lispm-shrdlu.elc; exit $$status
