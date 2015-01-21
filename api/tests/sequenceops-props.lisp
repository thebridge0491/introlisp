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

(5am:test (prop-tabulate)
	(5am:for-all ((len (5am:gen-integer :min 0 :max 20)))
		(let* ((func (lambda (i) (+ i 2)))
				(ans (mapcar func (util:range-cnt 0 len))))
			(5am:is (reduce (lambda (a f)
				(and a (equal ans (funcall f func len)))) 
				'(seqops:tabulate-r seqops:tabulate-i seqops:tabulate-lp)
				:initial-value t))
			)))

(5am:test (prop-length)
	(5am:for-all ((xs (5am:gen-list :length (5am:gen-integer :min 0 :max 20))))
		(let* ((ans (length xs)))
			(5am:is (reduce (lambda (a f)
				(and a (equal ans (funcall f xs)))) 
				'(seqops:length-r seqops:length-i seqops:length-lp) 
				:initial-value t))
			)))

(5am:test (prop-nth)
	(5am:for-all ((xs (5am:gen-list :length (5am:gen-integer :min 1 :max 20)))
			(ndx (5am:gen-integer :min 0 :max 20)))
		(if (< ndx (length xs))
		(let* ((ans (nth ndx xs)))
			(5am:is (reduce (lambda (a f)
				(and a (equal ans (funcall f ndx xs)))) 
				'(seqops:nth-r seqops:nth-i seqops:nth-lp) :initial-value t))
			)
		(5am:is (equal t t)))))

(5am:test (prop-index-find)
	(5am:for-all ((xs (5am:gen-list :length (5am:gen-integer :min 0 :max 20)))
			(el (5am:gen-integer)))
		(let* ((pred (lambda (e) (equal el e))) (ans-idx (position el xs))
				(ans-find (find-if pred xs)))
			(5am:is (reduce (lambda (a tup) (let* ((fn-idx (car tup))
					(fn-find (cdr tup)))
				(and a (equal ans-idx (funcall fn-idx pred xs))
					(equal ans-find (funcall fn-find pred xs)))))
				'((seqops:index-r . seqops:find-r)
				(seqops:index-i . seqops:find-i)
				(seqops:index-lp . seqops:find-lp)) :initial-value t))
			)))

(5am:test (prop-min-max)
	(5am:for-all ((xs (5am:gen-list :length (5am:gen-integer :min 1 :max 20))))
		(let* ((ans-min (apply #'min xs)) (ans-max (apply #'max xs)))
			(5am:is (reduce (lambda (a tup) (let* ((fn-min (car tup)) 
					(fn-max (cdr tup)))
				(and a (equal ans-min (apply fn-min xs))
					(equal ans-max (apply fn-max xs)))))
				'((seqops:min-r . seqops:max-r) (seqops:min-i . seqops:max-i) 
				(seqops:min-lp . seqops:max-lp)) :initial-value t)))))

(5am:test (prop-reverse)
	(5am:for-all ((xs (5am:gen-list :length (5am:gen-integer :min 0 :max 20))))
		(let* ((ans (reverse xs)))
			(5am:is (reduce (lambda (a f)
				(and a (equal ans (funcall f xs))))
				'(seqops:reverse-r seqops:reverse-i seqops:reverse-lp) 
				:initial-value t)))))

(5am:test (prop-copy)
	(5am:for-all ((xs (5am:gen-list :length (5am:gen-integer :min 0 :max 20))))
		(let* ((ans (copy-seq xs)))
			(5am:is (reduce (lambda (a f)
				(and a (equal ans (funcall f xs))))
				'(seqops:copy-r seqops:copy-i seqops:copy-lp)
				:initial-value t)))))

(5am:test (prop-take-drop)
	(5am:for-all ((xs (5am:gen-list :length (5am:gen-integer :min 0 :max 20)))
			(n (5am:gen-integer :min 0 :max 20)))
		(if (<= n (length xs))
		(let* ((ans-take (subseq xs 0 n)) (ans-drop (nthcdr n xs)))
			(5am:is (reduce (lambda (a tup) (let ((fn-take (car tup)) 
					(fn-drop (cdr tup)))
				(and a (equal ans-take (funcall fn-take n xs))
					(equal ans-drop (funcall fn-drop n xs)))))
				'((seqops:take-i . seqops:drop-i) 
				(seqops:take-lp . seqops:drop-lp)) :initial-value t))
			)
		(5am:is (equal t t)))))

(5am:test (prop-any-every)
	(5am:for-all ((xs (5am:gen-list :length (5am:gen-integer :min 0 :max 20))))
		(let* ((pred (lambda (e) (equal 0 (mod e 2)))) (ans-any (some pred xs))
				(ans-every (every pred xs)))
			(5am:is (reduce (lambda (a tup) (let* ((fn-any (car tup))
					(fn-every (cdr tup)))
				(and a (equal ans-any (funcall fn-any pred xs))
					(equal ans-every (funcall fn-every pred xs)))))
				'((seqops:any-r . seqops:every-r)
				(seqops:any-i . seqops:every-i)
				(seqops:any-lp . seqops:every-lp)) :initial-value t))
			)))

(5am:test (prop-map)
	(5am:for-all ((xs (5am:gen-list :length (5am:gen-integer :min 0 :max 20))))
		(let* ((proc (lambda (e) (+ e 2))) (ans (mapcar proc xs)))
			(5am:is (reduce (lambda (a f)
				(and a (equal ans (funcall f proc xs)))) 
				'(seqops:map-r seqops:map-i seqops:map-lp)
				:initial-value t))
			)))
#|
(5am:test (prop-for-each)
	(5am:for-all ((xs (5am:gen-list :length (5am:gen-integer :min 0 :max 20))))
		(let* ((proc (lambda (e) (format t "~a " e))) (ans (map nil proc xs)))
			(5am:is (reduce (lambda (a f)
				(and a (equal ans (funcall f proc xs)))) 
				'(seqops:for-each-r seqops:for-each-i seqops:for-each-lp)
				:initial-value t))
			)))
|#

(5am:test (prop-filter-remove)
	(5am:for-all ((xs (5am:gen-list :length (5am:gen-integer :min 0 :max 20))))
		(let* ((pred (lambda (e) (equal 0 (mod e 2)))) (ans-filter (remove-if-not pred xs))
				(ans-remove (remove-if pred xs)))
			(5am:is (reduce (lambda (a tup) (let* ((fn-filter (car tup))
					(fn-remove (cdr tup)))
				(and a (equal ans-filter (funcall fn-filter pred xs))
					(equal ans-remove (funcall fn-remove pred xs)))))
				'((seqops:filter-r . seqops:remove-r)
				(seqops:filter-i . seqops:remove-i)
				(seqops:filter-lp . seqops:remove-lp)) :initial-value t))
			)))

(5am:test (prop-fold-left)
	(5am:for-all ((xs (5am:gen-list :length (5am:gen-integer :min 0 :max 20))))
		(let* ((corp #'+) (ans (reduce corp xs :initial-value 0)))
			(5am:is (reduce (lambda (a f)
				(and a (equal ans (funcall f corp 0 xs)))) 
				'(seqops:fold-left-r seqops:fold-left-i seqops:fold-left-lp)
				:initial-value t))
			)))

(5am:test (prop-fold-right)
	(5am:for-all ((xs (5am:gen-list :length (5am:gen-integer :min 0 :max 20))))
		(let* ((proc #'-) (ans (reduce proc xs :initial-value 0 :from-end t)))
			(5am:is (reduce (lambda (a f)
				(and a (equal ans (funcall f proc 0 xs)))) 
				'(seqops:fold-right-r seqops:fold-right-i seqops:fold-right-lp)
				:initial-value t))
			)))

(5am:test (prop-unfold-right)
	(5am:for-all ((x (5am:gen-integer :min 0 :max 25)))
		(let* ((pred (lambda (tup) (< (cdr tup) (car tup))))
			(func (lambda (tup) (car tup)))
			(seed (cons 0 x)) (gen (lambda (tup)
				(cons (1+ (car tup)) (cdr tup))))
			(ans (unfold-right pred func gen seed)))
			(5am:is (reduce (lambda (a f)
				(and a (equal ans (funcall f pred func gen seed)))) 
				'(seqops:unfold-right-i seqops:unfold-right-lp)
				:initial-value t))
			)))

(5am:test (prop-unfold-left)
	(5am:for-all ((x (5am:gen-integer :min 0 :max 25)))
		(let* ((pred (lambda (tup) (< (cdr tup) (car tup))))
			(func (lambda (tup) (car tup)))
			(seed (cons 0 x)) (gen (lambda (tup)
				(cons (1+ (car tup)) (- (cdr tup) (car tup)))))
			(ans (reverse (unfold-right pred func gen seed))))
			(5am:is (reduce (lambda (a f)
				(and a (equal ans (funcall f pred func gen seed)))) 
				'(seqops:unfold-left-r seqops:unfold-left-lp)
				:initial-value t))
			)))

(5am:test (prop-is-ordered)
	(5am:for-all ((xs (5am:gen-list :length (5am:gen-integer :min 0 :max 20))))
		(let* ((cmp #'<=)
			(verify (lambda (lst cmp) (every cmp lst (cdr lst))))
			(ans (funcall verify xs cmp)))
			(5am:is (reduce (lambda (a f)
				(and a (equal ans (funcall f xs :cmp cmp)))) 
				'(seqops:is-ordered-r seqops:is-ordered-i seqops:is-ordered-lp)
				:initial-value t))
			)))


(5am:test (prop-append)
	(5am:for-all ((xs (5am:gen-list :length (5am:gen-integer :min 0 :max 20))) (ys (5am:gen-list :length (5am:gen-integer :min 0 :max 20))))
		(let* ((ans (append xs ys)))
			(5am:is (reduce (lambda (a f)
				(and a (equal ans (funcall f xs ys)))) 
				'(seqops:append-r seqops:append-i seqops:append-lp)
				:initial-value t))
			)))

(5am:test (prop-interleave)
	(5am:for-all ((xs (5am:gen-list :length (5am:gen-integer :min 0 :max 20))) (ys (5am:gen-list :length (5am:gen-integer :min 0 :max 20))))
		(let* ((extra (if (> (length xs) (length ys))
				(nthcdr (length ys) xs) (nthcdr (length xs) ys)))
				(ans (append (mapcan (lambda (x y) (list x y)) xs ys) extra)))
			(5am:is (reduce (lambda (a f)
				(and a (equal ans (funcall f xs ys)))) 
				'(seqops:interleave-r seqops:interleave-i seqops:interleave-lp)
				:initial-value t))
			)))

(5am:test (prop-map2)
	(5am:for-all ((xs (5am:gen-list :length (5am:gen-integer :min 0 :max 20))) (ys (5am:gen-list :length (5am:gen-integer :min 0 :max 20))))
		(let* ((proc (lambda (e1 e2) (+ e1 e2 2))) (ans (mapcar proc xs ys)))
			(5am:is (reduce (lambda (a f)
				(and a (equal ans (funcall f proc xs ys)))) 
				'(seqops:map2-r seqops:map2-i seqops:map2-lp)
				:initial-value t))
			)))

(5am:test (prop-zip)
	(5am:for-all ((xs (5am:gen-list :length (5am:gen-integer :min 0 :max 20))) (ys (5am:gen-list :length (5am:gen-integer :min 0 :max 20))))
		(let* ((ans (mapcar #'list xs ys)))
			(5am:is (reduce (lambda (a f)
				(and a (equal ans (funcall f xs ys)))) 
				'(seqops:zip-r seqops:zip-i seqops:zip-lp)
				:initial-value t))
			)))

(5am:test (prop-unzip)
	(5am:for-all ((xs (5am:gen-list :length (5am:gen-integer :min 0 :max 20))) (ys (5am:gen-list :length (5am:gen-integer :min 0 :max 20))))
		(let* ((zlst (mapcar #'list xs ys)) (ans (values (mapcar #'car zlst) (mapcar #'cadr zlst))))
			(5am:is (reduce (lambda (a f)
				(and a (equal ans (funcall f zlst)))) 
				'(seqops:unzip-i seqops:unzip-lp seqops:unzip-m)
				:initial-value t))
			)))

(5am:test (prop-concat)
	(5am:for-all ((nlst (5am:gen-list :length (5am:gen-integer :min 0 :max 20) :elements (5am:gen-list :length (5am:gen-integer :min 0 :max 10)))))
		(let* ((ans (apply #'concatenate 'list nlst)))
			(5am:is (reduce (lambda (a f)
				(and a (equal ans (funcall f nlst)))) 
				'(seqops:concat-r seqops:concat-i seqops:concat-lp seqops:concat-a)
				:initial-value t))
			)))
