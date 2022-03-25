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

pkg = intro
parent = introlisp
proj = introlisp.intro
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

.PHONY: all compile run repl_run testCompile

all compile : build/.depend build/main ## compile
testCompile : build/ts_main ## compile test

build/main: src/main.lisp
	-rm $@
#	-~/bin/buildapp_$(LISP) --load src/main.lisp --entry $(proj):main --output $@
	-if [ "clisp" = $(LISP) ] ; then \
#		rlwrap $(LISP) -i src/main.lisp -x '($(proj):save-image)' ; \
		rlwrap $(LISP) -i src/main.lisp -x '(asdf:make :$(proj)/run-image)' ; \
	else \
#		rlwrap $(LISP) --load src/main.lisp --eval '($(proj):save-image)' ; \
		rlwrap $(LISP) --load src/main.lisp --eval '(asdf:make :$(proj)/run-image)' ; \
	fi

run : compile ## run main app [ARGS=""]
#	export [DY]LD_LIBRARY_PATH=. # ([da|ba|z]sh Linux)
#	setenv [DY]LD_LIBRARY_PATH . # (tcsh FreeBSD)
	-build/main $(ARGS)
repl_run : src/main.lisp ## in repl, run main app [ARGS=""]
	-if [ "clisp" = $(LISP) ] ; then \
#		rlwrap $(LISP) -i src/main.lisp -x '(progn (asdf:load-system :$(proj)) ($(proj):run-main))' $(ARGS) ; \
		rlwrap $(LISP) -i src/main.lisp -x '(asdf:test-system :$(proj)/run)' $(ARGS) ; \
	else \
#		rlwrap $(LISP) --load src/main.lisp --eval '(progn (asdf:load-system :$(proj)) ($(proj):run-main))' $(ARGS) ; \
		rlwrap $(LISP) --load src/main.lisp --eval '(asdf:test-system :$(proj)/run)' $(ARGS) ; \
	fi

-include Makefile-targets.mk
#-include Makefile-auxffi.mk
