.PHONY : all ecukes

EMACS ?= emacs
SRC = $(filter-out %-pkg.el, $(wildcard *.el reporters/*.el))
CASK ?= cask
PKG_DIR := $(shell $(CASK) package-directory)
FEATURES = $(wildcard features/*.feature features/reporters/*.feature)

all: test

test:
	$(MAKE) ecukes

elpa: $(PKG_DIR)
$(PKG_DIR): Cask
	$(CASK) install
	touch $@

ecukes:
	$(CASK) exec ecukes --script $(FEATURES)
