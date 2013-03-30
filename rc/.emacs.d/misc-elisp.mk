# Byte compile miscellaneous Elisp source files.  build.py invokes this.
#
# Copied initially from Evil's makefile, so GPLed.

SHELL := bash
EMACS ?= emacs
# The my- elisp files depend on practically everything, so I always rebuild them from build.py -- for now.
FILES := \
  lisp/goto-chg.el \
  lisp/undo-tree.el \
  lisp/rainbow-delimiters.el
LIBS :=

ELCFILES := $(FILES:.el=.elc)

.PHONY: all compile compile-batch clean evil

# Byte-compile Evil.
all: compile
compile: $(ELCFILES)

$(ELCFILES): %.elc: %.el
	$(EMACS) --batch -Q -L . $(LIBS) -f batch-byte-compile $<

# Byte-compile all files in one batch. This is faster than
# compiling each file in isolation, but also less stringent.
compile-batch: clean
	$(EMACS) --batch -Q -L . $(LIBS) -f batch-byte-compile ${FILES}

evil: evil/lib/goto-chg.el evil/lib/undo-tree.el
	make -C evil

# Delete byte-compiled files etc.
clean:
	rm $(ELCFILES)

