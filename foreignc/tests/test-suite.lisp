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
    
    --- [native|byte]code - compile ; then run tests ---
    ;[sbcl | clisp] [--load | -i] test-suite.lisp [--eval | -x] '(pkg/test:save-image)'
    ;buildapp --load test-suite.lisp --entry pkg/test:main --output ts_main
    [sbcl | clisp] [--load | -i] test-suite.lisp [--eval | -x] '(asdf:make :pkg/test-image)'
    ./ts_main topt1 toptN
    
    --- start REPL w/ loaded script ; then run tests ---
    ; [sbcl | clisp] [--load | -i] test-suite.lisp
    ;   --eval '(pkg/test:run-suites)' -- topt1 toptN
    ;[sbcl | clisp] [--load | -i] test-suite.lisp
    ;  --eval '(asdf:test-system :pkg/test)' -- topt1 toptN
    [sbcl | clisp] [--load | -i] test-suite.lisp [--eval | -x] '(pkg/test:main (list topt1 toptN))'
    * > (main '("topt1" "toptN"))
    
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

(asdf:load-systems :cl-quickcheck :fiveam :introlisp.util :introlisp.foreignc)

;; NOTE: To run this test file, execute `(asdf:test-system :introlisp.foreignc)' in your Lisp.

(defpackage :introlisp.foreignc/test
  (:documentation "Test system for introlisp.foreignc")
  (:use :cl :fiveam :introlisp.util :introlisp.foreignc)
  (:export :main :run-suites :save-image)
  )
(in-package :introlisp.foreignc/test)

(rename-package 'cl-quickcheck 'cl-quickcheck '(qc))
;(setf qc:*num-trials* 100)

(rename-package 'introlisp.foreignc.classic 'introlisp.foreignc.classic '(classic)) 

(rename-package :introlisp.util :introlisp.util '(:util))
(rename-package 'introlisp.foreignc.classic 'introlisp.foreignc.classic '(classic)) 

(defconstant +epsilon+ 0.001)

(defconstant +num0+ 0)
(defvar lst (loop for i from +num0+ below 5 collect i))
(defvar revlst (loop for i from (- 5 1) downto +num0+ collect i))


(mapcar (lambda (filenm) (load (merge-pathnames filenm
		(asdf:system-source-file :introlisp.foreignc))))
	'("tests/classic-tests.lisp" "tests/classic-props.lisp"
		)
	)

(defun main (argv)
    (mapcar (lambda (suite) (fiveam:run! suite))
		(list 'tc-classic 'tp-classic))
	
	(introlisp.foreignc.classic:close-lib)
    ) ;(uiop:quit))

;(if (member (pathname-name *load-truename*) (uiop:command-line-arguments)
;        :test #'(lambda (x y) (search x y :test #'equalp)))
;    (main (uiop:command-line-arguments)))
;(main (uiop:command-line-arguments))

(defun run-suites ()
	(main (uiop:command-line-arguments))
	(uiop:quit)
	)

(defun save-image ()
	#+sbcl (sb-ext:save-lisp-and-die "build/ts_main" :executable t
		:toplevel 'introlisp.foreignc/test:run-suites)
	#+clisp (ext:saveinitmem "build/ts_main" :executable t
		:init-function 'introlisp.foreignc/test:run-suites)
	#+ccl (ccl:save-application "build/ts_main" :prepend-kernel t
		:toplevel-function 'introlisp.foreignc/test:run-suites)
	)
