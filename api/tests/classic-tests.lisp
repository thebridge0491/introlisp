; Common Lisp - comments style conventions
; title       ;;;; Short title for block of code
; intro       ;;;  Description before block of code
; state       ;;   State of program or following code
; explanation ;    Regarding line on which it appears

(in-package :introlisp.practice/test)

(5am:def-suite tc-classic
	:description "Classic Tests suite")
(5am:in-suite tc-classic)

(5am:def-fixture fixc-classic ()
	(format t "~tSetUp-->~%")
	(&body)
	(format t "~t <--TearDown~%"))

(5am:test (test-square :fixture fixc-classic)
	(mapcar (lambda (n)
		(let* ((ans (expt n 2.0)))
		(mapcar (lambda (f)
			(5am:is (util:in-epsilon ans (funcall f n) (* +epsilon+ ans))))
			'(classic:square-r classic:square-i classic:square-lp
			classic:square-f classic:square-u))))
		'(2.0 11.0 20.0)))

(5am:test (test-expt :fixture fixc-classic)
	(mapcar (lambda (tup)
		(let* ((b (car tup)) (n (cdr tup)) (ans (expt b n)))
		(mapcar (lambda (f)
			(5am:is (util:in-epsilon ans (funcall f b n) (* +epsilon+ ans))))
			'(classic:expt-r classic:expt-i classic:expt-lp
				classic:fast-expt-r classic:fast-expt-i classic:fast-expt-lp 
				classic:expt-f classic:expt-u))))
		(util:cartesian-prod '(2.0 11.0 20.0) '(3.0 6.0 10.0))))

(5am:test (test-sum-to :fixture fixc-classic)
	(mapcar (lambda (tup)
		(let* ((hi (car tup)) (lo (cdr tup))
			(ans (reduce #'+ (util:range-cnt (1+ lo) (- hi lo)) 
			:initial-value lo)))
		(5am:is (equal ans (classic:sum-to-u hi lo)))
		(mapcar (lambda (f)
			(5am:is (= ans (funcall f hi lo))))
			'(classic:sum-to-r classic:sum-to-i classic:sum-to-lp 
			classic:sum-to-f classic:sum-to-u))))
		(util:cartesian-prod '(15 0 150) '(-20 0 -10))))

(5am:test (test-fact :fixture fixc-classic)
	(mapcar (lambda (n)
		(let ((ans (reduce #'* (util:range-cnt 1 n) :initial-value 1)))
		(mapcar (lambda (f)
			(5am:is (= ans (funcall f n))))
			'(classic:fact-r classic:fact-i classic:fact-lp 
			classic:fact-f classic:fact-u))))
		'(0 9 18)))

(5am:test (test-fib :fixture fixc-classic)
	(mapcar (lambda (n)
		(let* ((corp (lambda (a e) (cons (+ (car a) (cdr a)) (car a))))
			(ans (car (reduce corp (util:range-cnt 0 n) 
				:initial-value '(0 . 1)))))
		(mapcar (lambda (f)
			(5am:is (= ans (funcall f n))))
			'(classic:fib-r classic:fib-i classic:fib-lp classic:fib-f
			classic:fib-u))))
		'(0 10 20)))

(5am:test (test-pascaltri :fixture fixc-classic)
	(mapcar (lambda (rows)
		(let* ((corp (lambda (a e) (cons (mapcar #'+ (cons 0 (car a))
                    (append (car a) '(0))) a)))
			(ans (reverse (reduce corp (util:range-cnt 0 rows) 
				:initial-value '((1))))))
		(mapcar (lambda (f)
			(5am:is (equalp ans (funcall f rows))))
			'(classic:pascaltri-mult classic:pascaltri-add 
			classic:pascaltri-f classic:pascaltri-u))))
		'(0 5 10)))

(5am:test (test-quot-rem :fixture fixc-classic)
	(mapcar (lambda (tup)
		(let* ((a (car tup)) (b (cdr tup))
				(ans-q (truncate a b)) (ans-r (rem a b)))
			(5am:is (equal ans-q (classic:quot-m a b)))
			(5am:is (equal ans-r (classic:rem-m a b)))))
		(util:cartesian-prod '(10 -10) '(3 -3))))
#|
(5am:test (test-div-mod :fixture fixc-classic)
	(mapcar (lambda (tup)
		(let* ((a (car tup)) (b (cdr tup))
				(ans-d (floor a b)) (ans-m (mod a b)))
			(5am:is (equal ans-d (classic:div-m a b)))
			;(5am:is (equal ans-m (classic:mod-m a b)))
			))
		(util:cartesian-prod '(10 -10) '(3 -3))))
|#
(5am:test (test-gcd-lcm :fixture fixc-classic)
	(mapcar (lambda (nums)
		(let ((ans-g (apply #'gcd nums)) (ans-l (apply #'lcm nums)))
		(mapcar (lambda (tup)
			(let ((fn-gcd (car tup)) (fn-lcm (cdr tup)))
				(5am:is (= ans-g (apply fn-gcd nums)))
				(5am:is (= ans-l (apply fn-lcm nums)))))
				'((classic:gcd-r . classic:lcm-r)
				(classic:gcd-i . classic:lcm-i)
				(classic:gcd-lp . classic:lcm-lp)
				(classic:gcd-f . classic:lcm-f)
				(classic:gcd-u . classic:lcm-u)))))
		'((24) (24 16) (24 16 12) (24 16 32))))

(5am:test (test-base-expand :fixture fixc-classic)
	(mapcar (lambda (tup)
		(let* ((base (car tup)) (num (cdr tup))
			(corp (lambda (a e) (cons (truncate (car a) base)
				(cons (truncate (rem (car a) base)) (cdr a)))))
			(ans (reduce corp (util:range-cnt 0 (round (log num base))) :initial-value (list (float num)))))
		(mapcar (lambda (f)
			(5am:is (equal ans (funcall f base num))))
			'(classic:base-expand-r classic:base-expand-i 
			classic:base-expand-lp classic:base-expand-f
			classic:base-expand-u))))
		'((2 . 11) (4 . 81) (3 . 243) (2 . 16))))

(5am:test (test-base-to10 :fixture fixc-classic)
	(mapcar (lambda (tup)
		(let* ((base (car tup)) (nums (cadr tup))
			(corp (lambda (a tup) (+ a (* (cadr tup) (expt base (car tup))))))
			(ans (reduce corp (mapcar #'list (util:range-cnt 0 (length nums)) (reverse nums)) :initial-value 0)))
		(mapcar (lambda (f)
			(5am:is (equal ans (funcall f base nums))))
			'(classic:base-to10-r classic:base-to10-i classic:base-to10-lp
			classic:base-to10-f classic:base-to10-u))))
		'((2 (1 0 1 1)) (4 (1 1 0 1)) (3 (1 0 0 0 0 0)) (2 (1 0 0 0 0)))))

(5am:test (test-range :fixture fixc-classic)
	(mapcar (lambda (stop-start)
		(let* ((stop (car stop-start)) (start (cdr stop-start))
			(diff (- stop start))
			(ans-pos (util:range-cnt start (if (> diff 0) diff 0)))
			(ans-neg (reverse (util:range-cnt (1+ stop) (if (< diff 0) (abs diff) 0)))))
		(mapcar (lambda (tup)
			(let ((fn-range (car tup)) (fn-rgStep (cdr tup)))
			(5am:is (equal ans-pos (funcall fn-range start stop)))
			(5am:is (equal ans-pos (funcall fn-rgStep start stop)))
			(5am:is (equal ans-neg (funcall fn-rgStep start stop :step -1)))
			))
			'((classic:range-r . classic:range-step-r)
				(classic:range-i . classic:range-step-i)
				(classic:range-lp . classic:range-step-lp)
				(classic:range-f . classic:range-step-f)
				(classic:range-u . classic:range-step-u)))))
		'((2 . -1) (11 . -5) (20 . -1))))

(5am:test (test-compose :fixture fixc-classic)
	(let ((fn-iota (lambda (ct) (util:range-cnt 0 ct)))
		(fn-square (lambda (n) (expt n 2.0))))
	(mapcar (lambda (f)
		(5am:is (util:in-epsilon 2 
			(funcall (funcall f fn-square #'sqrt) 2) (* +epsilon+ 2)))
		(5am:is (equal 5 (funcall (funcall f #'length fn-iota) 5)))
		(5am:is (equal '(0 1 2 3 4) (funcall (funcall f fn-iota) 5))))
		'(classic:compose-r classic:compose-i classic:compose-lp
		classic:compose-f classic:compose-u))))

