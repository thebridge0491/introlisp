; Common Lisp - comments style conventions
; title       ;;;; Short title for block of code
; intro       ;;;  Description before block of code
; state       ;;   State of program or following code
; explanation ;    Regarding line on which it appears

(in-package :introlisp.practice/test)

(5am:def-suite tc-sequenceops
	:description "Sequenceops Tests suite")
(5am:in-suite tc-sequenceops)

(5am:def-fixture fixc-sequenceops ()
	(format t "~tSetUp-->~%")
	(&body)
	(format t "~t <--TearDown~%"))

(5am:test (test-index-find :fixture fixc-sequenceops)
	(mapcar (lambda (f)
		(let* ((el 3) (pred (lambda (e) (equal e el))))
			(5am:is (equal (position el lst) (funcall f pred lst)))
			(5am:is (equal (position el revlst) (funcall f pred revlst)))))
		'(seqops:index-r seqops:index-i)))

(5am:test (test-reverse :fixture fixc-sequenceops)
    (mapcar (lambda (f) (progn
		(5am:is (equal revlst (funcall f lst)))
		(5am:is (equal lst (funcall f revlst)))
		nil))
		'(seqops:reverse-r seqops:reverse-i)))
