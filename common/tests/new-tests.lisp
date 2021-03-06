; Common Lisp - comments style conventions
; title       ;;;; Short title for block of code
; intro       ;;;  Description before block of code
; state       ;;   State of program or following code
; explanation ;    Regarding line on which it appears

(in-package :introlisp.util/test)

(5am:def-suite tc-new
	:description "New Tests suite")
(5am:in-suite tc-new)

(5am:def-fixture fixc-new ()
	(format t "~tSetUp-->~%")
	(&body)
	(format t "~t <--TearDown~%"))

(5am:test (test-method :fixture fixc-new)
	(5am:is (= 10 (apply #'+ lst)))
    (5am:is (= 4 (* 2 2))))

(5am:test (test-dbl-method :fixture fixc-new)
    (5am:is (= 4.0 4.0)))

(5am:test (test-str-method :fixture fixc-new)
    (5am:is (string= "Hello" "Hello")))

(5am:test (test-skip-method :fixture fixc-new)
    (5am:skip "Skip"))

(5am:test (test-fail-method :fixture fixc-new)
    (5am:fail "Fail"))

(5am:test (test-exception-method :fixture fixc-new)
    (5am:signals arithmetic-error (let ((x 0)) (/ x))))
