; Common Lisp - comments style conventions
; title       ;;;; Short title for block of code
; intro       ;;;  Description before block of code
; state       ;;   State of program or following code
; explanation ;    Regarding line on which it appears

(defpackage :introlisp.practice/src/classic-streams
    (:documentation "Practice.Classic.Streams library")
    (:use :cl :clazy)
    (:nicknames :introlisp.practice.classic.streams)
    (:export :squares-strm :squares-map2 :expts-strm :expts-map2 :sums-strm 
		:sums-map2 :facts-strm :facts-map2 :fibs-strm :fibs-map2
		:pascalrows-strm :pascalrows-map2
		
		:squares-su :expts-su :sums-su :facts-su :fibs-su :pascalrows-su
		)
    )
(in-package :introlisp.practice/src/classic-streams)

(defun squares-strm ()
	(labels (
		(iter (n)
			(lazy:cons-stream (* n n) (iter (1+ n)))))
		(iter 0.0)))

(defun expts-strm (b)
	(labels (
		(iter (acc)
			(lazy:cons-stream acc (iter (* acc b)))))
		(iter 1.0)))

(defun sums-strm (lo)
	(labels (
		(iter (oldval acc)
			(lazy:cons-stream acc
				(iter (1+ oldval) (+ (1+ oldval) acc)))))
		(iter lo lo)))

(defun facts-strm ()
	(labels (
		(iter (oldval acc)
			(lazy:cons-stream acc (iter (1+ oldval) (* (1+ oldval) acc)))))
		(iter 0 1)))

(defun fibs-strm ()
	(labels (
		(iter (s0 s1)
			(lazy:cons-stream s0 (iter s1 (+ s0 s1)))))
		(iter 0 1)))

(defun pascalrows-strm ()
	(labels (
		(iter (row)
			(lazy:cons-stream row
				(iter (mapcar #'+ (cons 0 row) (append row '(0)))))))
		(iter '(1))))


(defun tabulate-stream (init func)
	(lazy:cons-stream (funcall func init)
		(funcall #'tabulate-stream (funcall func init) func)))

(defun map2-stream (proc2 strm1 strm2)
	(if (some (lambda (s) (not (lazy:lazy-stream-p s))) (list strm1 strm2))
		'()
		(lazy:cons-stream (funcall proc2 (lazy:head strm1) (lazy:head strm2))
			(funcall #'map2-stream proc2 (lazy:tail strm1) (lazy:tail strm2)))))

(defun squares-map2 ()
	(lazy:cons-stream 0.0 (map2-stream (lambda (x y) (* y y))
		(funcall #'squares-map2) (tabulate-stream 0.0 #'1+))))

(defun expts-map2 (b)
	(lazy:cons-stream 1.0 (map2-stream (lambda (x y) (* x y))
		(funcall #'expts-map2 b) (tabulate-stream -1 (lambda (x) b)))))

(defun sums-map2 (lo)
	(lazy:cons-stream lo (map2-stream (lambda (x y) (+ x y lo))
		(funcall #'sums-map2 lo) (tabulate-stream 0 #'1+))))

(defun facts-map2 ()
	(lazy:cons-stream 1 (map2-stream #'*
		(funcall #'facts-map2) (tabulate-stream 0 #'1+))))

(defun fibs-map2 ()
	(lazy:cons-stream 0 (lazy:cons-stream 1 (map2-stream #'+
		(funcall #'fibs-map2) (lazy:tail (funcall #'fibs-map2))))))

(defun pascalrows-map2 ()
	(lazy:cons-stream '(1) (map2-stream (lambda (row i)
		(mapcar #'+ (cons 0 row) (append row '(0)))) (funcall #'pascalrows-map2)
			(tabulate-stream -1 #'identity))))


(defun unfold-stream (mapper gen seed)
	(lazy:cons-stream (funcall mapper seed)
		(funcall #'unfold-stream mapper gen (funcall gen seed))))

(defun squares-su ()
	(let ((func (lambda (seed) (* seed seed))))
		(unfold-stream func #'1+ 0.0)))

(defun expts-su (b)
	(let ((func (lambda (seed) (* seed b))))
		(unfold-stream #'identity (lambda (seed) (funcall func seed)) 1.0)))

(defun sums-su (lo)
	(let ((func (lambda (seed) (+ (car seed) (cdr seed)))))
		(unfold-stream #'car (lambda (seed) (cons (funcall func seed)
			(1+ (cdr seed)))) (cons lo (1+ lo)))))

(defun facts-su ()
	(let ((func (lambda (seed) (* (car seed) (cdr seed)))))
		(unfold-stream #'car (lambda (seed) (cons (funcall func seed)
			(1+ (cdr seed)))) (cons 1 1))))

(defun fibs-su ()
	(let ((func (lambda (seed) (+ (car seed) (cdr seed)))))
		(unfold-stream #'car (lambda (seed) (cons (cdr seed)
			(funcall func seed))) (cons 0 1))))

(defun pascalrows-su ()
	(let ((func (lambda (row) (mapcar #'+ (cons 0 row) (append row '(0))))))
		(unfold-stream #'identity func '(1))))
