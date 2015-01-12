; Common Lisp - comments style conventions
; title       ;;;; Short title for block of code
; intro       ;;;  Description before block of code
; state       ;;   State of program or following code
; explanation ;    Regarding line on which it appears

(defpackage :introlisp.practice/src/lib
  (:documentation "Practice sub-package for Common Lisp Intro examples project")
  (:use :cl)
  (:nicknames :introlisp.practice)
  ;(:export :????)
  )
(in-package :introlisp.practice/src/lib)

;(defparameter *version* "X.Y.Z" "library version.")
#|
(defun lib-main (argv)
	(format t "(fact-i ~a): ~a~%" 5 (fact-i 5))
	
	);(uiop:quit)
|#
;(if (member (pathname-name *load-truename*) '("src/lib.lisp")
;        :test #'(lambda (x y) (search x y :test #'equalp)))
;    (lib-main (uiop:command-line-arguments)))
