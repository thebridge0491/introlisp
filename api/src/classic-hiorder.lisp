; Common Lisp - comments style conventions
; title       ;;;; Short title for block of code
; intro       ;;;  Description before block of code
; state       ;;   State of program or following code
; explanation ;    Regarding line on which it appears

(defpackage :introlisp.practice/src/classic-hiorder
    (:documentation "Practice.Classic.Hiorder library")
    (:use :cl)
    (:nicknames :introlisp.practice.classic.hiorder)
    (:export :square-f :expt-f :sum-to-f :fact-f :fib-f :pascaltri-f :gcd-f
		:lcm-f :base-expand-f :base-to10-f :range-step-f :range-f :compose-f
		
		:square-u :expt-u :sum-to-u :fact-u :fib-u :pascaltri-u :gcd-u :lcm-u
		:base-expand-u :base-to10-u :range-step-u :range-u :compose-u
		)
    )
(in-package :introlisp.practice/src/classic-hiorder)

(defun range-cnt (cnt &key (start 0) (step 1))
	(if (< 0 step)
		(loop for i from 0 below cnt by step collect (+ start i))
		(loop for i downfrom 0 above cnt by (abs step) collect (+ start i))))

(defun expt-f (b n)
    (reduce (lambda (a e) (* a b)) (range-cnt n :start 1) :initial-value 1))

(defun square-f (n) (expt-f n 2.0))

(defun numseq-math-f (op hi lo &key (init 0))
	(reduce op (range-cnt (- (1+ hi) lo) :start lo) :initial-value init))

(defun sum-to-f (hi lo) (numseq-math-f #'+ hi lo))

(defun fact-f (n) (numseq-math-f #'* n 1 :init 1))

(defun fib-f (n)
	(car (reduce (lambda (s0-s1 e)
		(cons (+ (car s0-s1) (cdr s0-s1)) (car s0-s1))) (range-cnt n)
		:initial-value (cons 0 1))))

(defun pascaltri-f (rows)
	(reverse (reduce (lambda (a e) (cons (mapcar #'+ (cons 0 (car a))
		(append (car a) '(0))) a)) (range-cnt rows) :initial-value '((1)))))

(defun gcd-f (m &rest args)
	(reduce #'gcd args :initial-value m))

(defun lcm-f (m &rest args)
	(reduce #'lcm args :initial-value m))

(defun base-expand-f (b n)
	(let ((corp (lambda (a e) (cons (cons (mod (cdr a) b) (car a))
			(floor (cdr a) b)))))
		(car (reduce corp (range-cnt (1+ (round (log n b))) :start 1) 	
			:initial-value (cons '() n)))))

(defun base-to10-f (b lst)
	(let ((corp (lambda (a tup) (+ a (* (cadr tup) (expt b (car tup)))))))
		(reduce corp (mapcar #'list (range-cnt (length lst)) (reverse lst))
			:initial-value 0)))

(defun range-step-f (start stop &key (step 1))
	(let* ((cmp (if (> step 0) #'>= #'<=)) (cnt (abs (/ (- stop start) step))))
		(reverse (reduce (lambda (a e) (if (not (funcall cmp e stop)) (cons e a) a))
			(range-cnt cnt :start start :step step) :initial-value '()))))

(defun range-f (start stop) (range-step-f start stop))

(defun compose-f (func &rest funcs)
	(reduce (lambda (a e) (lambda (x) (funcall a (funcall e x)))) funcs 
		:initial-value func))


(defun unfold-right (pred func gen seed)
	(loop for ini = seed then
		(funcall gen ini)
		until (funcall pred ini)
		for acc = (list (funcall func ini)) then
			(cons (funcall func ini) acc)
		finally (return acc)))

(defun expt-u (b n)
    (car (unfold-right (lambda (tup) (> 0 (cdr tup))) #'car (lambda (tup) 
		(cons (* (car tup) b) (1- (cdr tup)))) (cons 1 n))))

(defun square-u (n) (expt-u n 2.0))


(defun numseq-math-u (op hi lo &key (init 0))
	(car (unfold-right (lambda (tup) (< (1+ hi) (cdr tup))) #'car
		(lambda (tup) (cons (funcall op (car tup) (cdr tup)) (1+ (cdr tup))))
		(cons init lo))))

(defun sum-to-u (hi lo) (or (numseq-math-u #'+ hi lo) 0))

(defun fact-u (n) (numseq-math-u #'* n 1 :init 1))

(defun fib-u (n)
	(car (append (unfold-right (lambda (s0-s1-m) (> 0 (cddr s0-s1-m))) #'car
        (lambda (s0-s1-m) (cons (cadr s0-s1-m) (cons 
			(+ (car s0-s1-m) (cadr s0-s1-m)) (1- (cddr s0-s1-m)))))
			(cons 0 (cons 1 n)))
		(cons 0 (cons 1 n)))))

(defun pascaltri-u (rows)
	(reverse (unfold-right (lambda (tup) (> 0 (cdr tup))) #'car
        (lambda (tup) (cons (mapcar #'+ (cons 0 (car tup))
			(append (car tup) '(0))) (1- (cdr tup))))
		(cons '(1) rows))))
#|
(defun euclid-u (m n)
	(car (unfold-right (lambda (tup) (zerop (cdr tup))) #'cdr
		(lambda (tup) (cons (cdr tup) (rem (car tup) (cdr tup)))) (cons m n))))
|#
(defun gcd-u (m &rest args)
	(let ((func (lambda (tup) (gcd (car tup) (cadr tup)))))
        (or (car (unfold-right (lambda (tup) (null (cdr tup))) func
            (lambda (tup) (cons (funcall func tup) (cddr tup)))
            (cons m args))) m)))

(defun lcm-u (m &rest args)
	(let ((func (lambda (tup) (lcm (car tup) (cadr tup)))))
        (or (car (unfold-right (lambda (tup) (null (cdr tup))) func
            (lambda (tup) (cons (funcall func tup) (cddr tup)))
            (cons m args))) m)))

(defun base-expand-u (b n)
	(unfold-right (lambda (x) (= 0 x)) (lambda (x) (rem x b))
        (lambda (x) (truncate x b)) n))

(defun base-to10-u (b lst)
	(let ((func (lambda (tup)
            (+ (car tup) (* (cadr tup) (expt b (length (cddr tup))))))))
        (car (unfold-right (lambda (tup) (null (cdr tup))) func
            (lambda (tup) (cons (funcall func tup) (cddr tup))) (cons 0 lst)))))

(defun range-step-u (start stop &key (step 1))
	(let ((cmp (if (> step 0) #'>= #'<=)))
		(reverse (unfold-right (lambda (x) (funcall cmp x stop)) #'identity
            (lambda (x) (+ x step)) start))))

(defun range-u (start stop) (range-step-u start stop))

(defun compose-u (func &rest funcs)
	(let ((fn-func (lambda (tup) (lambda (x)
			(funcall (cdr tup) (funcall (caar tup) x))))))
		(or (car (unfold-right (lambda (tup) (null (car tup))) fn-func
            (lambda (tup) (cons (cdar tup) (funcall fn-func tup)))
            (cons funcs func))) func)))
