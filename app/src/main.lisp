":"; exec sbcl --script $0 $@
":"; exit
;#!/usr/bin/env sbcl --script

; Common Lisp - comments style conventions
; title       ;;;; Short title for block of code
; intro       ;;;  Description before block of code
; state       ;;   State of program or following code
; explanation ;    Regarding line on which it appears

; single-line comment
#| multi-line comment
    --- package manager - init/configure package ---
    (asdf:load-systems :cl-project)
    (cl-project:make-project "./" :name "pkg" :author "imcomputer"
      :license "..." :depends-on '(...)' :email "..." :description "...")
    
    --- generate docs ---
    (asdf:load-systems {:codex | :documentation-template} :pkg)
    ;(codex:document :pkg)
    (documentation-template:create-template :pkg :subtitle "Title"
        :target "docpath/dir/file.html")
    
    --- [native|byte]code - compile ; then run app ---
    ;[sbcl | ccl] --load main.lisp --eval '(pkg:save-image)'
    ;buildapp --load main.lisp --entry pkg:main --output main
    [sbcl | ccl] --load main.lisp --eval '(asdf:make :pkg/run-image)'
    ./main arg1 argN
    
    --- start REPL w/ loaded script ; then run app ---
    ; [sbcl | ccl] --load main.lisp --eval '(pkg:run-main)' -- arg1 argN
    ;[sbcl | ccl] --load main.lisp
    ;  --eval '(asdf:test-system :pkg/run)' -- arg1 argN
    [sbcl | ccl] --load main.lisp --eval '(main (list arg1 argN))'
    * > (main '("arg1" "argN"))
    
    --- help/info tools in REPL ---
    (apropos 'PATTERN [:PKG]) (describe 'SYMBOL) *features*
    (documentation 'NAME 'DOC-TYPE)
    ; --- DOC-TYPE(s): 'T|'TYPE|'FUNCTION|'VARIABLE|'STRUCTURE|'SETF
|#

;(load "~/quicklisp/setup.lisp")
;(ql:register-local-projects)

#|
;(require :asdf)
(eval-when (:compile-toplevel :load-toplevel :execute)
	(require "asdf"))
|#

(asdf:load-systems :log4cl :unix-options :py-configparser :cl-ppcre :cl-json
	:cl-yaml) ; :introlisp.intro/src/lib :introlisp.intro/src/person)

(defpackage :introlisp.intro/src/main
  (:documentation "Main app")
  (:use :cl :log4cl :unix-options :py-configparser :cl-ppcre :cl-json
    :cl-yaml)
  (:nicknames :introlisp.intro)
  (:export :main :run-main :save-image)
  )
(in-package :introlisp.intro/src/main)

;(rename-package :introlisp.intro/src/lib :introlisp.intro/src/lib '(:lib))
;(rename-package :introlisp.intro.person :introlisp.intro.person '(:person))

(defun mkstring-htbl (htbl)
	(with-hash-table-iterator (next-entry htbl)
		(loop
			collect (multiple-value-bind (more? k v) (next-entry)
				(unless more? (return lst))
				(format nil "{~a: ~a} " k v)) into lst))
	)

(defun run-intro (name)
	"Run intro"
	(let ((pat "(?i:^quit$)"))
		(format t "~a match ~a to ~a~%" (if (cl-ppcre:scan pat name) 
			"Good" "Does not") name pat)
	))

(defun parse-cmdopts (argv)
	"Parse command-line options"
    (log:info '(root) "parse-cmdopts")
    (unix-options:with-cli-options (argv t) 
        ((verbose (verbose "VERBOSITY" "Verbosity level"))
            (user (user "USER" "User name")))
        
        (values (or verbose 0) (or user "World"))))

(defun main (argv)
	"Entry point"
	(let* ((sys-dir (asdf:system-source-directory :introlisp.intro))
			(rsrc-dir (pathname (or (uiop:getenv "RSRC_PATH")
				(merge-pathnames "resources/" sys-dir))))
			(ini-cfg (py-configparser:make-config))
			(alst-cfg (cl-json:decode-json-from-source
				(merge-pathnames "prac.json" rsrc-dir)))
			(cl-json:*json-symbols-package* nil)
			(obj-cfg (cl-json:with-decoder-simple-clos-semantics
				(cl-json:decode-json-from-source (merge-pathnames 
					"prac_htbl.json" rsrc-dir))))
			(htbl (make-hash-table :test 'equal))
			(yaml-htbl (yaml:parse (merge-pathnames "prac.yaml" rsrc-dir)))
			)
		(log:config :properties (merge-pathnames "log4cl.conf"
			rsrc-dir))
		
		(py-configparser:read-files ini-cfg (list 
			(merge-pathnames "prac.conf" rsrc-dir)))
		
		(maphash (lambda (k v) (setf (gethash (princ-to-string k) htbl) v)) 
			obj-cfg)
		(maphash (lambda (k v) (setf (gethash (princ-to-string k)
			(gethash "USER-1" htbl)) v)) (gethash "USER-1" htbl))
		
		(let* ((tup-vec (vector 
			(cons (slot-value ini-cfg 'py-configparser:sections) 
				(cons (py-configparser:get-option ini-cfg "default" "domain")
					(py-configparser:get-option ini-cfg "user1" "name")))
			(cons alst-cfg (cons (cdr (assoc :domain alst-cfg))
				(cdr (assoc :name (cdr (assoc :user-1 alst-cfg))))))
			(cons (mkstring-htbl obj-cfg) (cons (gethash "DOMAIN" htbl)
				(gethash "NAME" (gethash "USER-1" htbl))))
			(cons (mkstring-htbl yaml-htbl) (cons (gethash "domain" yaml-htbl)
				(gethash "name" (gethash "user1" yaml-htbl))))
			)))
			(map nil (lambda (tup3) (progn 
				(format t "config: ~a~%" (car tup3))
				(format t "domain: ~a~%" (cadr tup3))
				(format t "user1Name: ~a~%" (cddr tup3))
				)) tup-vec)
			)
		
		(multiple-value-bind (verbose user)
			(parse-cmdopts argv)
			
			(handler-case 
				(run-intro user)
				(type-error (condition) (uiop:quit))))
		
    )) ;(uiop:quit))

;(if (member (pathname-name *load-truename*) (uiop:command-line-arguments)
;        :test #'(lambda (x y) (search x y :test #'equalp)))
;    (main (uiop:command-line-arguments)))
;(main (uiop:command-line-arguments))

(defun run-main ()
	(main (uiop:command-line-arguments))
	(uiop:quit)
	)

(defun save-image ()
	#+sbcl (sb-ext:save-lisp-and-die "build/main" :executable t
		:toplevel 'introlisp.intro:run-main)
	#+ccl (ccl:save-application "build/main" :prepend-kernel t
		:toplevel-function 'introlisp.intro:run-main)
	)
