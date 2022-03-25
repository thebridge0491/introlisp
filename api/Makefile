.POSIX:
.SUFFIXES:
PREFIX ?= /usr/local

DEBUG = 0

# Single package Makefile script.
#.SUFFIXES: .s .o .a .h .c
help:

PKG_CONFIG = pkg-config --with-path=$(PREFIX)/lib/pkgconfig

# LISP=[sbcl | clisp]
LISP = sbcl

pkg = practice
parent = introlisp
proj = introlisp.practice
namespace_path = $(shell echo $(parent) | sed 'y|.|/|')
version = 0.1.0

build/.depend:
	-mkdir -p ~/quicklisp/local-projects/$(parent)
	-find src tests -type d -exec mkdir -p build/{} \;
	-cp -fR resources build
	-@echo '' > build/.depend

build/ts_main: tests/test-suite.lisp
	-rm $@
#	-~/bin/buildapp_$(LISP) --load tests/test-suite.lisp --entry $(proj)/test:main --output $@
	-if [ "clisp" = $(LISP) ] ; then \
#		rlwrap $(LISP) -i tests/test-suite.lisp -x '($(proj)/test:save-image)' ; \
		rlwrap $(LISP) -i tests/test-suite.lisp -x '(asdf:make :$(proj)/test-image)' ; \
	else \
#		rlwrap $(LISP) --load tests/test-suite.lisp --eval '($(proj)/test:save-image)' ; \
		rlwrap $(LISP) --load tests/test-suite.lisp --eval '(asdf:make :$(proj)/test-image)' ; \
	fi

.PHONY: all compile testCompile

all compile : build/.depend ## compile
testCompile : build/ts_main ## compile test

-include Makefile-targets.mk
#-include Makefile-auxffi.mk
