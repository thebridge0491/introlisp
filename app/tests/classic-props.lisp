; Common Lisp - comments style conventions
; title       ;;;; Short title for block of code
; intro       ;;;  Description before block of code
; state       ;;   State of program or following code
; explanation ;    Regarding line on which it appears

(in-package :introlisp.intro/test)

(5am:def-suite tp-classic
	:description "Classic Properties suite")
(5am:in-suite tp-classic)

(5am:def-fixture fixp-classic ()
	(format t "~tSetUp-->~%")
	(&body)
	(format t "~t <--TearDown~%"))

(5am:test (prop-expt)
	(5am:for-all ((x (5am:gen-integer :min 1 :max 20))
			(y (5am:gen-integer :min 2 :max 10)))
		(let* ((b (float x)) (n (float y)) (ans (expt b n)))
			(5am:is (reduce (lambda (a f)
				(and a (in-epsilon ans (funcall f b n) (* +epsilon+ ans))))
				'(classic:expt-lp classic:expt-i) :initial-value t)))))

(5am:test (prop-fact)
    (5am:for-all ((n (5am:gen-integer :min 0 :max 18)))
		(let* ((ans (reduce #'* (loop for i from 0 below n collect (1+ i))
				:initial-value 1)))
			(5am:is (reduce (lambda (a f) (and a (= ans (funcall f n))))
				'(classic:fact-lp classic:fact-i) :initial-value t)))))
