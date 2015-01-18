; Common Lisp - comments style conventions
; title       ;;;; Short title for block of code
; intro       ;;;  Description before block of code
; state       ;;   State of program or following code
; explanation ;    Regarding line on which it appears

(in-package :introlisp.intro/test)

(5am:def-suite tp-collections
	:description "Collections Properties suite")
(5am:in-suite tp-collections)

(5am:def-fixture fixp-collections ()
	(format t "~tSetUp-->~%")
	(&body)
	(format t "~t <--TearDown~%"))

(5am:test (prop-cons)
	(5am:for-all ((xs (5am:gen-list :length (5am:gen-integer :min 1 :max 20)))
			(x (5am:gen-integer)))
		(5am:is (equal x (car (cons x xs))))))

(5am:test (prop-null)
	(5am:for-all ((xs (5am:gen-list :length (5am:gen-integer :min 1 :max 20))))
		(5am:is (equal (= 0 (length xs)) (null xs)))))

(5am:test (prop-equal)
	(5am:for-all ((xs (5am:gen-list :length (5am:gen-integer :min 1 :max 20))))
		(let ((ys (copy-list xs)))
			(5am:is (equal ys xs)))))

(5am:test (prop-append)
	(5am:for-all ((xs (5am:gen-list :length (5am:gen-integer :min 1 :max 20)))
			(ys (5am:gen-list :length (5am:gen-integer :min 1 :max 20))))
		(5am:is (equal (append xs ys) (reduce (lambda (e a) (cons e a)) 
			xs :initial-value ys :from-end t)))))

(5am:test (prop-revrev)
	(5am:for-all ((xs (5am:gen-list :length (5am:gen-integer :min 1 :max 20))))
		(5am:is (equal xs (reverse (reverse xs))))))

(5am:test (prop-filter)
	(5am:for-all ((xs (5am:gen-list :length (5am:gen-integer :min 1 :max 20))))
		(let ((pred1 (lambda (e) (= 0 (mod e 2)))))
			(5am:is (every pred1 (remove-if-not pred1 xs))))))

(5am:test (prop-map)
	(5am:for-all ((xs (5am:gen-list :length (5am:gen-integer :min 1 :max 20))))
		(let* ((proc1 (lambda (e) (1+ e))) (ys (mapcar proc1 xs)))
			(5am:is (equal ys (mapcar proc1 xs))))))

(defun is-ordered (xs &key (cmp #'<=))
	(cond ((null xs) t)
		((null (cdr xs)) t)
		(t (and (funcall cmp (car xs) (cadr xs)) (is-ordered (cdr xs) 
			:cmp cmp)))))

(5am:test (prop-sort-isordered)
	(5am:for-all ((xs (5am:gen-list :length (5am:gen-integer :min 1 :max 20))))
		(5am:is (is-ordered (sort xs #'<)))
		(5am:is (is-ordered (sort xs #'>) :cmp #'>=))
		))
