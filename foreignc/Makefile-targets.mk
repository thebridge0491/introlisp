# Targets Makefile script.
#----------------------------------------
# Common automatic variables legend (GNU make: make (Linux) gmake (FreeBSD)):
# $* - basename (cur target)  $^ - name(s) (all depns)  $< - name (1st depn)
# $@ - name (cur target)      $% - archive member name  $? - changed depns

FMTS ?= tar.gz
distdir = $(proj)-$(version)

.PHONY: help clean test repl_test uninstall install dist doc
help: ## help
	@echo "##### subproject: $(proj) #####"
	@echo "Usage: $(MAKE) [COMPILER="$(COMPILER)"] [target] -- some valid targets:"
#	-@for fileX in $(MAKEFILE_LIST) `if [ -z "$(MAKEFILE_LIST)" ] ; then echo Makefile Makefile-targets.mk ; fi` ; do \
#		grep -ve '^[A-Z]' $$fileX | awk '/^[^.%][-A-Za-z0-9_]+[ ]*:.*$$/ { print "...", substr($$1, 1, length($$1)) }' | sort ; \
#	done
	-@for fileX in $(MAKEFILE_LIST) `if [ -z "$(MAKEFILE_LIST)" ] ; then echo Makefile Makefile-targets.mk ; fi` ; do \
		grep -E '^[ a-zA-Z_-]+:.*?## .*$$' $$fileX | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "%-25s%s\n", $$1, $$2}' ; \
	done
clean: ## clean build artifacts
	-rm -rf build/* build/.??* *.log
test: testCompile ## run test [TOPTS=""]
#	export [DY]LD_LIBRARY_PATH=. # ([da|ba|z]sh Linux)
#	setenv [DY]LD_LIBRARY_PATH . # (tcsh FreeBSD)
	-build/ts_main $(TOPTS)
repl_test : tests/test-suite.lisp ## in repl, run test [TOPTS=""]
#	-rlwrap $(COMPILER) --load tests/test-suite.lisp --eval '($(proj)/test:run-suites)' $(TOPTS)
	-rlwrap $(COMPILER) --load tests/test-suite.lisp --eval '(asdf:test-system :$(proj)/test)' $(TOPTS)
uninstall install: ## [un]install artifacts
	-@if [ "uninstall" = "$@" ] ; then \
		rm -rf ~/quicklisp/local-projects/$(parent)/$(proj) ; \
	else \
		ln -sf $(PWD) $(proj) ; \
		mv $(proj) ~/quicklisp/local-projects/$(parent)/ ; \
	fi
	-rlwrap $(COMPILER) --eval '(progn (ql:register-local-projects) (uiop:quit))'
dist: ## [FMTS="tar.gz"] archive source code
	-@mkdir -p build/$(distdir) ; cp -f exclude.lst build/
#	#-zip -9 -q --exclude @exclude.lst -r - . | unzip -od build/$(distdir) -
	-tar --format=posix --dereference --exclude-from=exclude.lst -cf - . | tar -xpf - -C build/$(distdir)
	
	-@for fmt in `echo $(FMTS) | tr ',' ' '` ; do \
		case $$fmt in \
			zip) echo "### build/$(distdir).zip ###" ; \
				rm -f build/$(distdir).zip ; \
				(cd build ; zip -9 -q -r $(distdir).zip $(distdir)) ;; \
			*) tarext=`echo $$fmt | grep -e '^tar$$' -e '^tar.xz$$' -e '^tar.bz2$$' || echo tar.gz` ; \
				echo "### build/$(distdir).$$tarext ###" ; \
				rm -f build/$(distdir).$$tarext ; \
				(cd build ; tar --posix -L -caf $(distdir).$$tarext $(distdir)) ;; \
		esac \
	done
	-@rm -r build/$(distdir)
doc: ## generate documentation
	-rm -fr build/html ; mkdir -p build/html
	-rlwrap $(COMPILER) --load doc_gen.lisp --eval '(uiop:quit)'
