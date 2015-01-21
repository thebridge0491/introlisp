; Common Lisp - comments style conventions
; title       ;;;; Short title for block of code
; intro       ;;;  Description before block of code
; state       ;;   State of program or following code
; explanation ;    Regarding line on which it appears

(defpackage :introlisp.practice/src/classic
    (:documentation "Practice.Classic library")
    (:use :cl :log4cl)
    (:nicknames :introlisp.practice.classic)
    (:export :square-r :square-i :square-lp :expt-r :expt-i :expt-lp
		:fast-expt-r :fast-expt-i :fast-expt-lp :sum-to-r :sum-to-i :sum-to-lp
		:fact-r :fact-i :fact-lp :fib-r :fib-i :fib-lp :pascaltri-add
		:pascaltri-mult :quot-m :rem-m :div-m :mod-m :gcd-r :gcd-i :gcd-lp
		:lcm-r :lcm-i :lcm-lp :base-expand-r :base-expand-i :base-expand-lp
		:base-to10-r :base-to10-i :base-to10-lp :range-step-r :range-step-i
		:range-step-lp :range-r :range-i :range-lp :compose-r :compose-i
		:compose-lp
		)
    )
(in-package :introlisp.practice/src/classic)

(defun expt-r (b n)
    "Exponent b to n (recursively)"
    (if (>= 0.0 n)
        1.0
        (* (expt-r b (1- n)) b)))

(defun expt-i (b n)
    "Exponent b to n (iteratively tail-call)"
    (labels (
        (iter (ct acc)
            (if (>= 0 ct)
                acc
                (iter (1- ct) (* acc b)))))
        (iter (floor n) 1.0)))

(defun expt-lp (b n)
    "Exponent b to n (loop)"
    (loop for ct from n downto 0
		for acc = 1 then (* acc b)
		finally (return acc)))

(defun fast-expt-r (b n)
    (cond ((>= 0.0 n) 1.0)
        ((evenp (floor n)) (expt (fast-expt-r b (/ n 2.0)) 2.0))
        (t (* (fast-expt-r b (1- n)) b))))

(defun fast-expt-i (b n)
    (labels (
        (iter (ct acc)
            (cond ((>= 0 ct) acc)
                ((evenp ct) (iter (- ct 2) (* acc (expt b 2.0))))
                (t (iter (1- ct) (* acc b))))))
        (iter (floor n) 1.0)))

(defun fast-expt-lp (b n)
    (loop for ct-acc = (cons (floor n) 1.0) then
		(if (evenp (car ct-acc))
			(cons (- (car ct-acc) 2) (* (cdr ct-acc) (expt b 2.0)))
			(cons (1- (car ct-acc)) (* (cdr ct-acc) b)))
		until (>= 0 (car ct-acc))
		finally (return (cdr ct-acc))))

(defun square-r (n) (expt-r n 2.0))
(defun square-i (n) (expt-i n 2.0))
(defun square-lp (n) (expt-lp n 2.0))

(defun numseq-math-r (op hi lo &key (init 0))
	"Number-sequence-math op hi lo &key init (recursively)"
	(if (< hi lo)
		init
		(funcall op hi (numseq-math-r op (1- hi) lo :init init))))

(defun numseq-math-i (op hi lo &key (init 0))
	"Number-sequence-math op hi lo &key init (iteratively tail-call)"
	(labels (
		(iter (start acc)
			(if (< start lo)
				acc
				(iter (1- start) (funcall op acc start)))))
		(iter hi init)))

(defun numseq-math-lp (op hi lo &key (init 0))
	"Number-sequence-math op hi lo &key init (loop)"
	(loop for ct from hi downto lo
		for acc = hi then (funcall op acc ct)
		finally (return (or acc init))))

(defun sum-to-r (hi lo) (numseq-math-r #'+ hi lo))
(defun sum-to-i (hi lo) (numseq-math-i #'+ hi lo))
(defun sum-to-lp (hi lo) (numseq-math-lp #'+ hi lo))


(defun fact-r (n)
    "Factorial n (recursively)"
	(numseq-math-r #'* n 1 :init 1))

(defun fact-i (n)
    "Factorial n (iteratively tail-call)"
    (log:info '(prac) "fact-i()")
    (numseq-math-i #'* n 1 :init 1))

(defun fact-lp (n)
    "Factorial n (loop)"
	(numseq-math-lp #'* n 1 :init 1))

(defun fib-r (n)
    (if (or (= 0 n) (= 1 n))
        n
        (+ (fib-r (1- n)) (fib-r (- n 2)))))

(defun fib-i (n)
    (labels (
        (iter (sum-1 sum-0 ct)
            (if (= 0 ct)
                sum-0
                (iter (+ sum-1 sum-0) sum-1 (1- ct)))))
        (iter 1 0 n)))

(defun fib-lp (n)
	(loop for ct from n downto 0
		for sum-pr = (cons 1 0) then
			(cons (+ (car sum-pr) (cdr sum-pr)) (car sum-pr))
		finally (return (cdr sum-pr))))

(defun pascaltri-add (rows)
    (defun next-row (lst)
        (mapcar #'+ (cons 0 lst) (append lst '(0))))
    (defun triangle (lst rows)
        (if (= 0 rows)
            '()
            (cons lst (triangle (next-row lst) (1- rows)))))
    (triangle (list 1) (1+ rows)))

(defun pascaltri-mult (rows)
    (defun pascalrow (r)
        (labels (
            (iter (col lst)
                (if (= r col)
                    lst
                    (iter (1+ col) 
                        (cons (* (car lst) (- r col) (/ col)) lst)))))
            (iter 1 '(1))))
    (mapcar #'pascalrow (range-i 1 (+ 2 rows))))

(defun quot-rem (a b)
    (handler-case (assert (and (integerp a) (integerp b)))
        (error (exc) (format t "~a: ~a ~a~%" "not both integers" a b)))
    (let ((q (truncate (/ a b))))
        (values q (- a (* b q)))))

(defun quot-m (a b) (multiple-value-bind (q r) (quot-rem a b) q))
(defun rem-m (a b) (multiple-value-bind (q r) (quot-rem a b) r))

(defun div-mod (a b)
    (let ((q-floor (floor (/ a b))) (q-ceil (ceiling (/ a b))))
        (if (> b 0)
            (values q-floor (- a (* b q-floor)))
            (values q-ceil (- a (* b q-ceil))))))

(defun div-m (a b) (multiple-value-bind (d m) (div-mod a b) d))
(defun mod-m (a b) (multiple-value-bind (d m) (div-mod a b) m))

(defun euclid-r (m n)
    (if (= 0 n)
        m
        (euclid-r n (rem m n))))

(defun euclid-i (m n)
    (labels (
        (iter (a b)
            (if (= 0 b)
                a
                (iter b (rem a b)))))
        (iter m n)))

(defun euclid-lp (m n)
    (loop for tup = (cons m n) then
		(cons (cdr tup) (rem (car tup) (cdr tup)))
		until (= 0 (cdr tup))
		finally (return (car tup))))

(defun gcd-r (m &rest args)
    (if (null args)
        m
        (apply #'gcd-r (euclid-r m (car args)) (cdr args))))

(defun gcd-i (m &rest args)
    (labels (
        (iter (acc rst)
            (if (null rst)
                acc
                (iter (euclid-i acc (car rst)) (cdr rst)))))
        (iter m args)))

(defun gcd-lp (m &rest args)
	(or (loop for n in args
		for acc = (euclid-lp m n) then
			(euclid-lp acc n)
		finally (return acc))
		m))

(defun lcm-r (m &rest args)
    (if (null args)
        m
        (apply #'lcm-r (/ (* m (car args)) (euclid-r m (car args)))
            (cdr args))))

(defun lcm-i (m &rest args)
    (labels (
        (iter (acc rst)
            (if (null rst)
                acc
                (iter (/ (* acc (car rst)) (euclid-i acc (car rst)))
                    (cdr rst)))))
        (iter m args)))

(defun lcm-lp (m &rest args)
	(or (loop for n in args
		for acc = (/ (* m n) (euclid-lp m n)) then
			(/ (* acc n) (euclid-lp acc n))
		finally (return acc))
		m))

(defun base-expand-r (b n)
    (if (= 0 n)
        '()
        (append (base-expand-r b (truncate n b)) (list (rem n b)))))

(defun base-expand-i (b n)
    (labels (
        (iter (q lst)
            (if (= 0 q)
                lst
                (iter (truncate q b) (cons (rem q b) lst)))))
        (iter n '())))

(defun base-expand-lp (b n)
    (reverse (loop for i = n
        then (truncate i b)
        until (= 0 i)
        collect (rem i b))))

(defun base-to10-r (b lst)
    (if (null lst)
        0
        (+ (* (car lst) (expt b (length (cdr lst))))
            (base-to10-r b (cdr lst)))))

(defun base-to10-i (b lst)
    (labels (
        (iter (rst sum ct)
            (if (null rst)
                sum
                (iter (cdr rst) (+ sum (* (car rst) (expt b ct))) (1+ ct)))))
        (iter (reverse lst) 0 0)))

(defun base-to10-lp (b lst)
    (let ((func (lambda (tup)
            (+ (car tup) (* (cadr tup) (expt b (length (cddr tup))))))))
        (car (reverse (loop for i = (cons 0 lst)
            then (cons (funcall func i) (cddr i))
            until (null (cdr i))
            collect (funcall func i))))))

(defun range-step-r (start stop &key (step 1))
	(if (funcall (if (> step 0) #'>= #'<=) start stop)
		'()
		(cons start (range-step-r (+ start step) stop :step step))))

(defun range-step-i (start stop &key (step 1))
	(labels (
		(iter (cur acc)
			(if (funcall (if (> step 0) #'>= #'<=) cur stop)
				acc
				(iter (+ cur step) (cons cur acc)))))
		(reverse (iter start '()))))

(defun range-step-lp (start stop &key (step 1))
	(loop for i = start then
		(+ i step)
		until (funcall (if (> step 0) #'>= #'<=) i stop)
		collect i))

(defun range-r (start stop) (range-step-r start stop))
(defun range-i (start stop) (range-step-i start stop))
(defun range-lp (start stop) (range-step-lp start stop))

(defun compose-r (&rest funcs)
    (if (null funcs)
        (lambda (x) x)
        ; have to apply inner-most procedure to get value
        (lambda (x) (funcall (car funcs) (funcall (apply #'compose-r
            (cdr funcs)) x)))))

(defun compose-i (func &rest funcs)
    (labels (
        (iter (acc rst)
            (if (null rst)
                acc
                (iter (lambda (x) (funcall acc (funcall (car rst) x)))
                    (cdr rst)))))
        (iter func funcs)))

(defun compose-lp (func &rest funcs)
	(or (loop for fn in funcs
		for acc = (lambda (x) (funcall func (funcall fn x))) then
			(lambda (x) (funcall acc (funcall fn x)))
		finally (return acc))
		func))


(defun lib-main (argv)
	(format t "(fact-i ~a): ~a~%" 5 (fact-i 5))
	
	);(uiop:quit)

;(if (member (pathname-name *load-truename*) '("src/classic.lisp")
;        :test #'(lambda (x y) (search x y :test #'equalp)))
;    (lib-main (uiop:command-line-arguments)))
