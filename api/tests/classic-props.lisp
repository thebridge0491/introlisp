; Common Lisp - comments style conventions
; title       ;;;; Short title for block of code
; intro       ;;;  Description before block of code
; state       ;;   State of program or following code
; explanation ;    Regarding line on which it appears

(in-package :introlisp.practice/test)

(5am:def-suite tp-classic
	:description "Classic Properties suite")
(5am:in-suite tp-classic)

(5am:def-fixture fixp-classic ()
	(format t "~tSetUp-->~%")
	(&body)
	(format t "~t <--TearDown~%"))

(5am:test (prop-square)
	(5am:for-all ((x (5am:gen-integer :min 1 :max 20)))
		(let* ((n (float x)) (ans (expt n 2.0)))
			(5am:is (reduce (lambda (a f)
				(and a (util:in-epsilon ans (funcall f n) (* +epsilon+ ans))))
				'(classic:square-r classic:square-i classic:square-lp 
				classic:square-f classic:square-u) 
				:initial-value t)))))

(5am:test (prop-expt)
	(5am:for-all ((x (5am:gen-integer :min 1 :max 20))
			(y (5am:gen-integer :min 2 :max 10)))
		(let* ((b (float x)) (n (float y)) (ans (expt b n)))
		(5am:is (reduce (lambda (a f)
			(and a (util:in-epsilon ans (funcall f b n) (* +epsilon+ ans))))
			'(classic:expt-r classic:expt-i classic:expt-lp
			classic:fast-expt-r classic:fast-expt-i 
			classic:fast-expt-lp classic:expt-f classic:expt-u) 
			:initial-value t)))))

(5am:test (prop-sum-to)
    (5am:for-all ((hi (5am:gen-integer :min 0 :max 18))
			(lo (5am:gen-integer :min -18 :max 0)))
		(let* ((ans (reduce #'+ (util:range-cnt (1+ lo) (- hi lo)) 
			:initial-value lo)))
		(5am:is (reduce (lambda (a f)
			(and a (= ans (funcall f hi lo))))
			'(classic:sum-to-r classic:sum-to-i classic:sum-to-lp 
			classic:sum-to-f classic:sum-to-u) :initial-value t)))))

(5am:test (prop-fact)
    (5am:for-all ((n (5am:gen-integer :min 0 :max 18)))
		(let* ((ans (reduce #'* (util:range-cnt 1 n) :initial-value 1)))
		(5am:is (reduce (lambda (a f)
			(and a (= ans (funcall f n))))
			'(classic:fact-r classic:fact-i classic:fact-lp classic:fact-f
			classic:fact-u)
			:initial-value t)))))

(5am:test (prop-fib)
    (5am:for-all ((n (5am:gen-integer :min 0 :max 20)))
		(let* ((corp (lambda (a e) (cons (+ (car a) (cdr a)) (car a))))
			(ans (car (reduce corp (util:range-cnt 0 n) 
				:initial-value '(0 . 1)))))
		(5am:is (reduce (lambda (a f)
			(and a (= ans (funcall f n))))
			'(classic:fib-r classic:fib-i classic:fib-lp classic:fib-f
			classic:fib-u) :initial-value t)))))

(5am:test (prop-pascaltri)
    (5am:for-all ((rows (5am:gen-integer :min 0 :max 10)))
		(let* ((corp (lambda (a e) (cons (mapcar #'+ (cons 0 (car a))
                    (append (car a) '(0))) a)))
			(ans (reverse (reduce corp (util:range-cnt 0 rows) 
				:initial-value '((1))))))
		(5am:is (reduce (lambda (a f)
			(and a (equal ans (funcall f rows))))
			'(classic:pascaltri-mult classic:pascaltri-add classic:pascaltri-f
			classic:pascaltri-u)
			:initial-value t)))))

(5am:test (prop-quot-rem)
    (5am:for-all ((a (5am:gen-integer :min -10 :max 10))
			(b (5am:gen-integer :min -10 :max 10) (/= 0 b)))
		(let* ((ans-q (truncate a b)) (ans-r (rem a b)))
		(5am:is (and (equal ans-q (classic:quot-m a b))
				(equal ans-r (classic:rem-m a b)))))))

(5am:test (prop-gcd-lcm)
	(5am:for-all ((nums (5am:gen-list :length (5am:gen-integer :min 1 :max 20)
			:elements (5am:gen-integer :min 2 :max 50))))
		(let* ((ans-g (apply #'gcd nums)) (ans-l (apply #'lcm nums)))
		(5am:is (reduce (lambda (a tup)
		(let ((fn-gcd (car tup)) (fn-lcm (cdr tup)))
			(and a (= ans-g (apply fn-gcd nums))
				(= ans-l (apply fn-lcm nums)))))
			'((classic:gcd-r . classic:lcm-r)
			(classic:gcd-i . classic:lcm-i)
			(classic:gcd-lp . classic:lcm-lp)
			(classic:gcd-f . classic:lcm-f)
			(classic:gcd-u . classic:lcm-u)) :initial-value t)))))

(5am:test (prop-base-expand)
    (5am:for-all ((base (5am:gen-integer :min 2 :max 16))
			(num (5am:gen-integer :min 2 :max 250)))
		(let* ((corp (lambda (a e) (cons (cons (mod (cdr a) base) (car a))
				(floor (cdr a) base))))
			(ans (car (reduce corp (util:range-cnt 0 (1+ (round (log num base)))) :initial-value (cons '() num)))))
		(5am:is (reduce (lambda (a f)
			(and a (or (equal ans (funcall f base num))
				(equal (cdr ans) (funcall f base num)))))
				'(classic:base-expand-r classic:base-expand-i 
				classic:base-expand-lp classic:base-expand-f
				classic:base-expand-u) :initial-value t)))))

(5am:test (prop-base-to10)
    (let ((base (+ (random 15) 2)))
    (5am:for-all ((nums (5am:gen-list :length (5am:gen-integer :min 1 :max 20)
			:elements (5am:gen-integer :min 0 :max (1- base)))))
		(let* ((corp (lambda (a tup) (+ a (* (cadr tup) (expt base (car tup))))))
			(ans (reduce corp (mapcar #'list (util:range-cnt 0 (length nums)) (reverse nums)) :initial-value 0)))
		(5am:is (reduce (lambda (a f)
			(and a (equal ans (funcall f base nums))))
			'(classic:base-to10-r classic:base-to10-i classic:base-to10-lp
			classic:base-to10-f classic:base-to10-u) :initial-value t))))))

(5am:test (prop-range)
    (5am:for-all ((stop (5am:gen-integer :min 0 :max 18))
			(start (5am:gen-integer :min -18 :max 0)))
		(let* ((diff (- stop start))
			(ans-pos (util:range-cnt start (if (> diff 0) diff 0)))
			(ans-neg (reverse (util:range-cnt (1+ stop) (if (< diff 0) (abs diff) 0)))))
		(5am:is (reduce (lambda (a tup)
			(let ((fn-range (car tup)) (fn-rgStep (cdr tup)))
			(and a (equal ans-pos (funcall fn-range start stop))
				(equal ans-pos (funcall fn-rgStep start stop))
				(equal ans-neg (funcall fn-rgStep start stop :step -1)))))
			'((classic:range-r . classic:range-step-r)
				(classic:range-i . classic:range-step-i)
				(classic:range-lp . classic:range-step-lp)
				(classic:range-f . classic:range-step-f)
				(classic:range-u . classic:range-step-u))
				:initial-value t)))))

(5am:test (prop-compose)
	(5am:for-all ((x (5am:gen-integer :min 0 :max 20)))
		(let* ((y (float x)) (fn-iota (lambda (ct) (util:range-cnt 0 ct)))
			(fn-square (lambda (n) (expt n 2.0)))
			(ans-len (length (funcall fn-iota x)))
			(ans-sqrt (sqrt (funcall fn-square y))))
		(5am:is (reduce (lambda (a f)
			(and a (util:in-epsilon ans-sqrt 
				(funcall (funcall f #'sqrt fn-square) y) (* +epsilon+ ans-sqrt))
				(equal ans-len (funcall (funcall f #'length fn-iota) x))
				(equal (util:range-cnt 0 x) (funcall (funcall f fn-iota) x))))
			'(classic:compose-r classic:compose-i classic:compose-lp
			classic:compose-f classic:compose-u) :initial-value t)))))
