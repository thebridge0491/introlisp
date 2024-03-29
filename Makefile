# Multi-package project Makefile script.
.POSIX:
help:

#MAKE = make # (GNU make variants: make (Linux) gmake (FreeBSD)

# LISP=[sbcl | clisp]
LISP = sbcl

parent = introlisp
version = 0.1.0
SUBDIRS = common foreignc api app

.PHONY: all help clean test repl_test uninstall install
help: $(SUBDIRS)
	-for dirX in $^ ; do $(MAKE) -C $$dirX $@ ; done
	@echo "##### Top-level multiproject: $(parent) #####"
	@echo "Usage: $(MAKE) [SUBDIRS="$(SUBDIRS)"] [LISP="$(LISP)"] [target]"
all test repl_test uninstall install: $(SUBDIRS)
	-for dirX in $^ ; do $(MAKE) -C $$dirX $@ ; done
clean: $(SUBDIRS)
	-for dirX in $^ ; do $(MAKE) -C $$dirX $@ ; done
	-rm -fr core* *~ .*~ build/* *.log */*.log

#----------------------------------------
FMTS ?= tar.gz,zip
distdir = $(parent)-$(version)

build/$(distdir) : 
	-@mkdir -p build/$(distdir) ; cp -f exclude.lst build/
#	#-zip -9 -q --exclude @exclude.lst -r - . | unzip -od build/$(distdir) -
	-tar --format=posix --dereference --exclude-from=exclude.lst -cf - . | tar -xpf - -C build/$(distdir)

.PHONY: dist doc run repl_run
dist | build/$(distdir): $(SUBDIRS)
	-@for fmt in `echo $(FMTS) | tr ',' ' '` ; do \
		case $$fmt in \
			7z) echo "### build/$(distdir).7z ###" ; \
				rm -f build/$(distdir).7z ; \
				(cd build ; 7za a -t7z -mx=9 $(distdir).7z $(distdir)) ;; \
			zip) echo "### build/$(distdir).zip ###" ; \
				rm -f build/$(distdir).zip ; \
				(cd build ; zip -9 -q -r $(distdir).zip $(distdir)) ;; \
			*) tarext=`echo $$fmt | grep -e '^tar$$' -e '^tar.xz$$' -e '^tar.zst$$' -e '^tar.bz2$$' || echo tar.gz` ; \
				echo "### build/$(distdir).$$tarext ###" ; \
				rm -f build/$(distdir).$$tarext ; \
				(cd build ; tar --posix -L -caf $(distdir).$$tarext $(distdir)) ;; \
		esac \
	done
	-@rm -r build/$(distdir)
	-for dirX in $^ ; do $(MAKE) -C $$dirX $@ ; done
doc: $(SUBDIRS)
	-for dirX in $^ ; do $(MAKE) -C $$dirX $@ ; done
run repl_run: app
	-$(MAKE) -C app $@
