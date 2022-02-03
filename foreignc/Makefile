.POSIX:
.SUFFIXES:
PREFIX ?= /usr/local

DEBUG = 0

# Single package Makefile script.
#.SUFFIXES: .s .o .a .h .c
help:

PKG_CONFIG = pkg-config --with-path=$(PREFIX)/lib/pkgconfig

# COMPILER=[sbcl | ccl]
COMPILER = sbcl

pkg = foreignc
parent = introlisp
proj = introlisp.foreignc
namespace_path = $(shell echo $(parent) | sed 'y|.|/|')
version = 0.1.0

build/.depend:
	-mkdir -p ~/quicklisp/local-projects/$(parent)
	-find src tests -type d -exec mkdir -p build/{} \;
	-cp -fR resources build
	-@echo '' > build/.depend

build/ts_main: tests/test-suite.lisp
	-rm $@
#	-rlwrap $(COMPILER) --load tests/test-suite.lisp --eval '($(proj)/test:save-image)'
#	-~/bin/buildapp_$(COMPILER) --load tests/test-suite.lisp --entry $(proj)/test:main --output $@
	-rlwrap $(COMPILER) --load tests/test-suite.lisp --eval '(asdf:make :$(proj)/test-image)'

.PHONY: all compile testCompile

all compile : build/.depend ## compile
testCompile : build/ts_main ## compile test

-include Makefile-targets.mk
-include Makefile-auxffi.mk
