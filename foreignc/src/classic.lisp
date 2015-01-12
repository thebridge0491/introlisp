; Common Lisp - comments style conventions
; title       ;;;; Short title for block of code
; intro       ;;;  Description before block of code
; state       ;;   State of program or following code
; explanation ;    Regarding line on which it appears

(defpackage :introlisp.foreignc/src/classic
  (:documentation "FFI (CFFI) examples")
  (:use :cl :cffi)
  (:nicknames :introlisp.foreignc.classic)
  (:export :close-lib :expt-lp :expt-i :fact-lp :fact-i)
  )
(in-package :introlisp.foreignc/src/classic)

;; set, if needed, [DY]LD_LIBRARY_PATH <path>
(cffi:define-foreign-library libintro_c-practice
	(:darwin "libintro_c-practice.dylib")
	(:unix "libintro_c-practice.so")
	(t (:default "libintro_c-practice")))

(defun close-lib ()
	"Close foreign library, when finished"
	(cffi:close-foreign-library 'libintro_c-practice))

;(cffi:load-foreign-library 'libintro_c-practice)
(cffi:use-foreign-library libintro_c-practice)

; explicit lisp-ier name
;(cffi:defcfun ("fact_lp" fact-lp) :unsigned-long (n :unsigned-int))
; implicit lisp-ier name (underscores to hyphens)
(cffi:defcfun "fact_lp" :unsigned-long
	"(C) Factorial n using loop"
	(n :unsigned-int))
(cffi:defcfun "fact_i" :unsigned-long
	"(C) Factorial n iteratively"
	(n :unsigned-int))

(cffi:defcfun "expt_lp" :float
	"(C) Exponent b n using loop"
	(b :float) (n :float))
(cffi:defcfun "expt_i" :float
	"(C) Exponent b n iteratively"
	(b :float) (n :float))

#|(mapcar (lambda (fn) (export fn)) '('fact-lp 'fact-i 'expt-lp 'expt-i
	'close-lib))|#


(defun lib-main (argv)
	(format t "(fact-i ~a): ~a~%" 5 (fact-i 5))
	
	);(uiop:quit)

;(if (member (pathname-name *load-truename*) '("src/classic.lisp")
;        :test #'(lambda (x y) (search x y :test #'equalp)))
;    (lib-main (uiop:command-line-arguments)))
