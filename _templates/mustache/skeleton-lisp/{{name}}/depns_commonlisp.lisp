":"; exec sbcl --script $0 $@
":"; exit
;#!/usr/bin/env sbcl --script

#|
	--- one-time QuickLisp install/setup
	curl -O https://beta.quicklisp.org/quicklisp.lisp[.asc]
	gpg --verify quicklisp.lisp.asc quicklisp.lisp
	[sbcl | ccl] --load quicklisp.lisp
	  * (quicklisp-quickstart:install)
	  * (ql:add-to-init-file)
	
	--- subsequent load quicklisp & install package example
	(load "~/quicklisp/setup.lisp")
	(ql:system-apropos "vecto")
	(ql:quickload "vecto")		; requires internet access
	
	--- load & require installed package
	(asdf:load-system :vecto)	; no internet required
	(require :vecto)
|#

(load "~/quicklisp/setup.lisp")

(defun main (argv)
    (progn
		(mapcar (lambda (pkg) (ql:quickload pkg))
			(list "fiveam" "lisp-unit2" "clunit" "cl-quickcheck" "cl-project" "documentation-template" "codex" "buildapp" "cl-ppcre" "py-configparser" "cl-json" "cl-yaml" "log4cl" "unix-options" "getopt" "cli-parser" "cffi"))
		nil)

    ) ;(uiop:quit))

(if (member (pathname-name *load-truename*) (uiop:command-line-arguments)
        :test #'(lambda (x y) (search x y :test #'equalp)))
    (main (uiop:command-line-arguments)))
;(main (uiop:command-line-arguments))
