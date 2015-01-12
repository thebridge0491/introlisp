; Common Lisp - comments style conventions
; title       ;;;; Short title for block of code
; intro       ;;;  Description before block of code
; state       ;;   State of program or following code
; explanation ;    Regarding line on which it appears

(in-package :introlisp.foreignc/test)

(5am:def-suite tc-classic
	:description "Classic Tests suite")
(5am:in-suite tc-classic)

(5am:def-fixture fixc-classic ()
	(format t "~tSetUp-->~%")
	(&body)
	(format t "~t <--TearDown~%"))

(5am:test (test-expt :fixture fixc-classic)
	(mapcar (lambda (tup)
		(let* ((b (car tup)) (n (cdr tup)) (ans (expt b n)))
		(mapcar (lambda (f)
			(5am:is (util:in-epsilon ans (funcall f b n) (* +epsilon+ ans))))
			'(classic:expt-lp classic:expt-i))))
		(mapcan (lambda (x) (mapcar (lambda (y) (cons x y)) '(2.0 11.0 20.0))) 
			'(3.0 6.0 10.0))))

(5am:test (test-fact :fixture fixc-classic)
    (mapcar (lambda (f)
		(5am:is (= 120 (funcall f 5))))
		'(classic:fact-lp classic:fact-i)))
