; Common Lisp - comments style conventions
; title       ;;;; Short title for block of code
; intro       ;;;  Description before block of code
; state       ;;   State of program or following code
; explanation ;    Regarding line on which it appears

(in-package :introlisp.practice/test)

(5am:def-suite tp-sequenceops
	:description "Sequenceops Properties suite")
(5am:in-suite tp-sequenceops)

(5am:def-fixture fixp-sequenceops ()
	(format t "~tSetUp-->~%")
	(&body)
	(format t "~t <--TearDown~%"))

(5am:test (prop-index-find)
	(5am:for-all ((xs (5am:gen-list :length (5am:gen-integer :min 1 :max 20)))
			(el (5am:gen-integer)))
		(let* ((ans (position el xs)) (pred (lambda (e) (equal el e))))
			(5am:is (reduce (lambda (a f)
				(and a (equal ans (funcall f pred xs)))) 
				'(seqops:index-r seqops:index-i) :initial-value t))
			)))

(5am:test (prop-reverse)
	(5am:for-all ((xs (5am:gen-list :length (5am:gen-integer :min 1 :max 20))))
		(let* ((ans (reverse xs)))
			(5am:is (reduce (lambda (a f)
				(equal ans (funcall f xs)))
				'(seqops:reverse-r seqops:reverse-i))))))
