; Common Lisp - comments style conventions
; title       ;;;; Short title for block of code
; intro       ;;;  Description before block of code
; state       ;;   State of program or following code
; explanation ;    Regarding line on which it appears

(defpackage :introlisp.practice/src/sequenceops
    (:documentation "Practice.Sequenceops library")
    (:use :cl :log4cl)
    (:nicknames :introlisp.practice.sequenceops)
    (:export :index-r :index-i :reverse-r :reverse-i)
    )
(in-package :introlisp.practice/src/sequenceops)

(defun index-find_r (pred xs &optional (ndx 0))
	"Index-find pred xs &optional ndx (recursively)"
	(cond ((null xs) (cons nil nil))
		((funcall pred (car xs)) (cons ndx (car xs)))
		(t (index-find_r pred (cdr xs) (1+ ndx)))))

(defun index-find_i (pred xs &optional (ndx 0))
	"Index-find pred xs &optional ndx (iteratively tail-call)"
	(labels (
		(iter (idx rst)
			(cond ((null rst) (cons nil nil))
				((funcall pred (car rst)) (cons idx (car rst)))
				(t (iter (1+ idx) (cdr rst))))))
		(iter ndx xs)))

(defun index-r (pred xs)
	"Index pred xs (recursively)"
    (car (index-find_r pred xs)))

(defun index-i (pred xs)
	"Index pred xs (iteratively tail-call)"
    (car (index-find_i pred xs)))

(defun reverse-r (xs)
	"Reverse xs (recursively)"
    (if (null (cdr xs))
        xs
        (append (reverse-r (cdr xs)) (list (car xs)))))

(defun reverse-i (xs)
	"Reverse xs (iteratively tail-call)"
    (labels (
        (iter (rst acc)
            (if (null rst)
                acc
                (iter (cdr rst) (cons (car rst) acc)))))
        (log:info '(prac) "reverse-i()")
        (iter xs '())))


(defun lib-main (argv)
	(format t "(reverse-i ~a): ~a~%" (list 0 1 2) (reverse-i (list 0 1 2)))
	
	);(uiop:quit)

;(if (member (pathname-name *load-truename*) '("src/sequenceops.lisp")
;        :test #'(lambda (x y) (search x y :test #'equalp)))
;    (lib-main (uiop:command-line-arguments)))
