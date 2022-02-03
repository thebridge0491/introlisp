# FFI auxiliary makefile script

ffi_libdir = $(shell $(PKG_CONFIG) --variable=libdir intro_c-practice || echo .)
ffi_incdir = $(shell $(PKG_CONFIG) --variable=includedir intro_c-practice || echo .)
LD_LIBRARY_PATH := $(LD_LIBRARY_PATH):$(ffi_libdir)
export LD_LIBRARY_PATH

.PHONY: prep_swig
prep_swig : src/classic-c.i ## prepare Swig files
	-swig -cffi -noswig-lisp -I$(ffi_incdir) -outdir build src/classic-c.i
