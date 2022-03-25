":"; exec sbcl --script $0 $@
":"; exit
;#!/usr/bin/env sbcl --script

#|
	--- one-time QuickLisp install/setup
	curl -LO https://beta.quicklisp.org/{quicklisp.lisp[.asc],release-key.txt}
	gpg --import release-key.txt ; gpg --verify quicklisp.lisp.asc quicklisp.lisp
	[sbcl | clisp] [--load | -i] quicklisp.lisp
	  * (quicklisp-quickstart:install)
	  * (ql:add-to-init-file)
	
	--- subsequent load quicklisp & install package example
	(load "~/quicklisp/setup.lisp")
	(ql:system-apropos "vecto")
	(ql:quickload '("vecto" "fiveam"))  ; requires internet access
	
	--- load & require installed package
	(asdf:load-system :vecto)	; no internet required
	(require :vecto)
|#

(load "~/quicklisp/setup.lisp")

(defun main (argv)
  (let ((pkgs (list "fiveam" "lisp-unit2" "clunit" "cl-quickcheck" "cl-project" "documentation-template" "codex" "cl-ppcre" "py-configparser" "cl-json" "cl-yaml" "log4cl" "unix-options" "getopt" "cli-parser" "cffi" "queues.simple-queue" "queues.priority-queue" "clazy" "buildapp")))
    #|(progn
	  (mapcar (lambda (pkg) (progn (ql:system-apropos pkg)
	      (ql:quickload pkg))) pkgs) nil)|#
    (progn (ql:system-apropos (car pkgs)) (ql:quickload pkgs))
    )) ;(uiop:quit))

(if (member (pathname-name *load-truename*) (uiop:command-line-arguments)
        :test #'(lambda (x y) (search x y :test #'equalp)))
    (main (uiop:command-line-arguments)))
;(main (uiop:command-line-arguments))
