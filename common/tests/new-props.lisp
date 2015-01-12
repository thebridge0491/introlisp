; Common Lisp - comments style conventions
; title       ;;;; Short title for block of code
; intro       ;;;  Description before block of code
; state       ;;   State of program or following code
; explanation ;    Regarding line on which it appears

(in-package :introlisp.util/test)

(5am:def-suite tp-new
	:description "New Properties suite")
(5am:in-suite tp-new)

(5am:def-fixture fixp-new ()
	(format t "~tSetUp-->~%")
	(&body)
	(format t "~t <--TearDown~%"))

(5am:test (prop-commutadd)
    (5am:for-all ((a (5am:gen-integer)) (b (5am:gen-integer)))
		(5am:is (= (+ a b) (+ b a)))))

(5am:test (prop-assocadd)
    (5am:for-all ((x (5am:gen-integer)) (y (5am:gen-integer))
			(z (5am:gen-integer)))
		(let ((a (float x)) (b (float y)) (c (float z)))
			;(5am:is (= (+ (+ a b) c) (+ a (+ b c))))
			(5am:is (in-epsilon (+ (+ a b) c) (+ a (+ b c))
				(* +epsilon+ (+ (+ a b) c)))))))

(5am:test (prop-revrev)
	(5am:for-all ((xs (5am:gen-list :length (5am:gen-integer :min 1 :max 20))))
		(5am:is (equal xs (reverse (reverse xs))))))

(5am:test (prop-idrev)
	(5am:for-all ((xs (5am:gen-list :length (5am:gen-integer :min 1 :max 20))))
		(5am:is (equal xs (reverse xs)))))

(5am:test (prop-sortrev)
	(5am:for-all ((xs (5am:gen-list :length (5am:gen-integer :min 1 :max 20)
			:elements (5am:gen-float :bound 1000))))
		(let ((ys (sort (copy-seq xs) #'<)) (zs (sort (reverse xs) #'<)))
			(5am:is (equal ys zs)))))

(5am:test (prop-minsorthead)
	(5am:for-all ((xs (5am:gen-list :length (5am:gen-integer :min 1 :max 20)
			:elements (5am:gen-float :bound 1000))))
		(let ((ys (sort (copy-seq xs) #'<)))
			(5am:is (in-epsilon (apply #'min xs) (car ys)
				(* +epsilon+ (car ys)))))))
