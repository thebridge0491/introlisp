.POSIX:
.SUFFIXES:
PREFIX = /usr/local

DEBUG = 0

# Single package Makefile script.
#.SUFFIXES: .s .o .a .h .c
help:

PKG_CONFIG = pkg-config --with-path=$(PREFIX)/lib/pkgconfig

# COMPILER=[sbcl | ccl]
COMPILER = sbcl

pkg = intro
parent = introlisp
proj = introlisp.intro
#namespace_path = $(shell echo $(parent) | sed 'y|.|/|')
version = 0.1.0
#ver_major = $(shell echo $(version) | cut -d'.' -f1)

build/.depend:
	-mkdir -p ~/quicklisp/local-projects/$(parent)
	-find src tests -type d -exec mkdir -p build/{} \;
	-cp -fR resources build
	-touch $@

build/ts_main: tests/test-suite.lisp
	-rm $@
#	-rlwrap $(COMPILER) --load tests/test-suite.lisp --eval '($(proj)/test:save-image)'
#	-~/bin/buildapp_$(COMPILER) --load tests/test-suite.lisp --entry $(proj)/test:main --output $@
	-rlwrap $(COMPILER) --load tests/test-suite.lisp --eval '(asdf:make :$(proj)/test-image)'

.PHONY: all compile run repl_run testCompile

all compile : build/.depend  build/main ## compile
testCompile : build/ts_main ## compile test

build/main: src/main.lisp
	-rm $@
#	-rlwrap $(COMPILER) --load src/main.lisp --eval '($(proj):save-image)'
#	-~/bin/buildapp_$(COMPILER) --load src/main.lisp --entry $(proj):main --output $@
	-rlwrap $(COMPILER) --load src/main.lisp --eval '(asdf:make :$(proj)/run-image)'

run : compile ## run main app [ARGS=""]
#	export [DY]LD_LIBRARY_PATH=. # ([da|ba|z]sh Linux)
#	setenv [DY]LD_LIBRARY_PATH . # (tcsh FreeBSD)
	-build/main $(ARGS)
repl_run : src/main.lisp ## in repl, run main app [ARGS=""]
#	-rlwrap $(COMPILER) --load src/main.lisp --eval '(progn (asdf:load-system :$(proj)) ($(proj):run-main))' $(ARGS)
	-rlwrap $(COMPILER) --load src/main.lisp --eval '(asdf:test-system :$(proj)/run)' $(ARGS)

-include Makefile-targets.mk
#-include Makefile-auxffi.mk
