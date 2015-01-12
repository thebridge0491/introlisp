; Common Lisp - comments style conventions
; title       ;;;; Short title for block of code
; intro       ;;;  Description before block of code
; state       ;;   State of program or following code
; explanation ;    Regarding line on which it appears

(defpackage :introlisp.practice/src/classic
    (:documentation "Practice.Classic library")
    (:use :cl :log4cl)
    (:nicknames :introlisp.practice.classic)
    (:export :expt-r :expt-i :fact-r :fact-i)
    )
(in-package :introlisp.practice/src/classic)

(defun expt-r (b n)
    "Exponent b to n (recursively)"
    (if (>= 0.0 n)
        1.0
        (* b (expt-r b (1- n)))))

(defun expt-i (b n)
    "Exponent b to n (iteratively tail-call)"
    (labels (
        (iter (ct acc)
            (if (>= 0 ct)
                acc
                (iter (1- ct) (* b acc)))))
        (iter (floor n) 1.0)))

(defun numseq-math_r (op hi lo &key (init 0))
	"Number-sequence-math op hi lo &key init (recursively)"
	(if (< hi lo)
		init
		(funcall op hi (numseq-math_r op (1- hi) lo :init init))))

(defun numseq-math_i (op hi lo &key (init 0))
	"Number-sequence-math op hi lo &key init (iteratively tail-call)"
	(labels (
		(iter (start acc)
			(if (< start lo)
				acc
				(iter (1- start) (funcall op acc start)))))
		(iter hi init)))

(defun fact-r (n)
    "Factorial n (recursively)"
	(numseq-math_r #'* n 1 :init 1))

(defun fact-i (n)
    "Factorial n (iteratively tail-call)"
    (log:info '(prac) "fact-i()")
    (numseq-math_i #'* n 1 :init 1))


(defun lib-main (argv)
	(format t "(fact-i ~a): ~a~%" 5 (fact-i 5))
	
	);(uiop:quit)

;(if (member (pathname-name *load-truename*) '("src/classic.lisp")
;        :test #'(lambda (x y) (search x y :test #'equalp)))
;    (lib-main (uiop:command-line-arguments)))
