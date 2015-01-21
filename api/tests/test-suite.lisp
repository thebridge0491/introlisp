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
    ;[sbcl | ccl] --load test-suite.lisp --eval '(pkg/test:save-image)'
    ;buildapp --load test-suite.lisp --entry pkg/test:main --output ts_main
    [sbcl | ccl] --load test-suite.lisp --eval '(asdf:make :pkg/test-image)'
    ./ts_main topt1 toptN
    
    --- start REPL w/ loaded script ; then run tests ---
    ; [sbcl | ccl] --load test-suite.lisp
    ;   --eval '(pkg/test:run-suites)' -- topt1 toptN
    ;[sbcl | ccl] --load test-suite.lisp
    ;  --eval '(asdf:test-system :pkg/test)' -- topt1 toptN
    [sbcl | ccl] --load test-suite.lisp --eval '(pkg/test:main (list topt1 toptN))'
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

(asdf:load-systems :log4cl :cl-quickcheck :fiveam :introlisp.util :introlisp.practice)

;; NOTE: To run this test file, execute `(asdf:test-system :introlisp.practice)' in your Lisp.

(defpackage :introlisp.practice/test
  (:documentation "Test system for introlisp.practice")
  (:use :cl :log4cl :fiveam :introlisp.util :introlisp.practice)
  (:export :main :run-suites :save-image)
  )
(in-package :introlisp.practice/test)

(rename-package 'cl-quickcheck 'cl-quickcheck '(qc))
;(setf qc:*num-trials* 100)

(rename-package 'introlisp.util 'introlisp.util '(util))
(rename-package 'introlisp.practice.classic 'introlisp.practice.classic '(classic)) 
(rename-package 'introlisp.practice.sequenceops 'introlisp.practice.sequenceops
  '(sequenceops seqops)) 

(defconstant +epsilon+ 0.001)

(defconstant +num0+ 0)
(defvar lst (loop for i from +num0+ below 5 collect i))
(defvar revlst (loop for i from (- 5 1) downto +num0+ collect i))

(defun unfold-right (pred func gen seed)
	(loop for ini = seed then
		(funcall gen ini)
		until (funcall pred ini)
		for acc = (list (funcall func ini)) then
			(cons (funcall func ini) acc)
		finally (return acc)))


(mapcar (lambda (filenm) (load (merge-pathnames filenm
		(asdf:system-source-file :introlisp.practice))))
	'("tests/classic-tests.lisp" "tests/classic-props.lisp"
		"tests/sequenceops-tests.lisp" "tests/sequenceops-props.lisp")
	)

(defun main (argv)
    (mapcar (lambda (suite) (fiveam:run! suite))
		(list 'tc-classic 'tp-classic 'tc-sequenceops 'tp-sequenceops))
	
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
		:toplevel 'introlisp.practice/test:run-suites)
	#+ccl (ccl:save-application "build/ts_main" :prepend-kernel t
		:toplevel-function 'introlisp.practice/test:run-suites)
	)
