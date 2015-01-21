; Common Lisp - comments style conventions
; title       ;;;; Short title for block of code
; intro       ;;;  Description before block of code
; state       ;;   State of program or following code
; explanation ;    Regarding line on which it appears

(defpackage :introlisp.practice/src/sequenceops
    (:documentation "Practice.Sequenceops library")
    (:use :cl :log4cl)
    (:nicknames :introlisp.practice.sequenceops)
    (:export :tabulate-r :tabulate-i :tabulate-lp :length-r :length-i 
		:length-lp :nth-r :nth-i :nth-lp :index-r :index-i :index-lp
		:find-r :find-i :find-lp :min-r :min-i :min-lp :max-r :max-i :max-lp
		:reverse-r :reverse-i :reverse-lp :copy-r :copy-i :copy-lp
		:split-at-i :split-at-lp :take-i :take-lp :drop-i :drop-lp
		:any-r :any-i :any-lp :every-r :every-i :every-lp :map-r :map-i
		:map-lp :for-each-r :for-each-i :for-each-lp :partition-r
		:partition-i :partition-lp :filter-r :filter-i :filter-lp
		:remove-r :remove-i :remove-lp
		:fold-left-r :fold-left-i
		:fold-left-lp :fold-right-r :fold-right-i :fold-right-lp
		
		:unfold-right-i :unfold-right-lp :unfold-left-r :unfold-left-lp
		
		:is-ordered-r :is-ordered-i :is-ordered-lp
		
		:append-r :append-i :append-lp :interleave-r :interleave-i
		:interleave-lp :map2-r :map2-i :map2-lp :zip-r :zip-i :zip-lp
		:unzip-i :unzip-lp :unzip-m :concat-r :concat-i :concat-lp
		:concat-a
		)
    )
(in-package :introlisp.practice/src/sequenceops)

(defun tabulate-r (func cnt)
	(if (> 1 cnt)
		'()
		(append (tabulate-r func (1- cnt)) (list (funcall func (1- cnt))))))

(defun tabulate-i (func cnt)
	(labels (
		(iter (idx acc)
			(if (> 1 idx)
				acc
				(iter (1- idx) (cons (funcall func (1- idx)) acc)))))
		(iter cnt '())))

(defun tabulate-lp (func cnt)
	(loop for i from 0 below cnt collect (funcall func i)))

(defun length-r (xs)
	(if (null xs)
		0
		(1+ (length-r (cdr xs)))))

(defun length-i (xs)
	(labels (
        (iter (rst acc)
            (if (null rst)
                acc
                (iter (cdr rst) (1+ acc)))))
        (iter xs 0)))

(defun length-lp (xs)
	(or (loop for el in xs
		for acc = 1 then
			(1+ acc)
		finally (return acc)) 0))

(defun nth-r (idx xs)
	(handler-case (assert (> (length xs) idx))
		(error (exc) (format t "index out of range: ~a~%" idx)))
	(if (= 0 idx)
		(car xs)
		(nth-r (1- idx) (cdr xs))))

(defun nth-i (idx xs)
	(handler-case (assert (> (length xs) idx))
		(error (exc) (format t "index out of range: ~a~%" idx)))
	(labels (
        (iter (n rst)
            (if (= 0 n)
                (car rst)
                (iter (1- n) (cdr rst)))))
        (iter idx xs)))

(defun nth-lp (idx xs)
	(handler-case (assert (> (length xs) idx))
		(error (exc) (format t "index out of range: ~a~%" idx)))
	(loop for ndx from 0 to idx
		for rst = xs then
			(cdr rst)
		finally (return (car rst))))

(defun index-find-r (pred xs &optional (ndx 0))
	"Index-find pred xs &optional ndx (recursively)"
	(cond ((null xs) (cons nil nil))
		((funcall pred (car xs)) (cons ndx (car xs)))
		(t (index-find-r pred (cdr xs) (1+ ndx)))))

(defun index-find-i (pred xs &optional (ndx 0))
	"Index-find pred xs &optional ndx (iteratively tail-call)"
	(labels (
		(iter (idx rst)
			(cond ((null rst) (cons nil nil))
				((funcall pred (car rst)) (cons idx (car rst)))
				(t (iter (1+ idx) (cdr rst))))))
		(iter ndx xs)))

(defun index-find-lp (pred xs &optional (ndx 0))
	"Index-find pred xs &optional ndx (loop)"
	(loop for idx-rst = (cons ndx xs) then
		(cons (1+ (car idx-rst)) (cddr idx-rst))
		until (or (funcall pred (cadr idx-rst)) (null (cdr idx-rst)))
		finally (return (if (funcall pred (cadr idx-rst))
				(cons (car idx-rst) (cadr idx-rst))
				(cons nil nil)))))

(defun index-r (pred xs)
	"Index pred xs (recursively)"
    (car (index-find-r pred xs)))

(defun index-i (pred xs)
	"Index pred xs (iteratively tail-call)"
    (car (index-find-i pred xs)))

(defun index-lp (pred xs)
	"Index pred xs (loop)"
    (car (index-find-lp pred xs)))

(defun find-r (pred xs) (cdr (index-find-r pred xs)))
(defun find-i (pred xs) (cdr (index-find-i pred xs)))
(defun find-lp (pred xs) (cdr (index-find-lp pred xs)))

(defun min-max-r (x &rest args)
	(defun helper (rst lo hi)
		(cond ((null rst) (list lo hi))
			((< (car rst) lo) (helper (cdr rst) (car rst) hi))
			((> (car rst) hi) (helper (cdr rst) lo (car rst)))
			(t (helper (cdr rst) lo hi))))
	(apply #'values (helper args x x)))

(defun min-max-i (x &rest args)
	(labels (
		(iter (rst lo hi)
			(cond ((null rst) (values lo hi))
				((< (car rst) lo) (iter (cdr rst) (car rst) hi))
				((> (car rst) hi) (iter (cdr rst) lo (car rst)))
				(t (iter (cdr rst) lo hi)))))
		(iter args x x)))

(defun min-max-lp (x &rest args)
	(loop for el in (cons x args)
		for acc = (list x x) then
			(cond
				((< el (car acc)) (list el (cadr acc)))
				((> el (cadr acc)) (list (car acc) el))
				(t acc))
		finally (return (apply #'values acc))))

(defun min-r (x &rest args)
	(multiple-value-bind (lo hi) (apply #'min-max-r x args) lo))
(defun min-i (x &rest args)
	(multiple-value-bind (lo hi) (apply #'min-max-i x args) lo))
(defun min-lp (x &rest args)
	(multiple-value-bind (lo hi) (apply #'min-max-lp x args) lo))

(defun max-r (x &rest args)
	(multiple-value-bind (lo hi) (apply #'min-max-r x args) hi))
(defun max-i (x &rest args)
	(multiple-value-bind (lo hi) (apply #'min-max-i x args) hi))
(defun max-lp (x &rest args)
	(multiple-value-bind (lo hi) (apply #'min-max-lp x args) hi))

(defun reverse-r (xs)
	"Reverse xs (recursively)"
    (if (null (cdr xs))
        xs
        (append (reverse-r (cdr xs)) (list (car xs)))))

(defun reverse-i (xs)
	"Reverse xs (iteratively tail-call)"
    (labels (
        (iter (rst acc)
            (if (null rst)
                acc
                (iter (cdr rst) (cons (car rst) acc)))))
        (log:info '(prac) "reverse-i()")
        (iter xs '())))

(defun reverse-lp (xs)
	"Reverse xs (loop)"
	(loop for el in xs
		for acc = (list el) then
			(cons el acc)
		finally (return acc)))

(defun copy-r (xs)
	(if (null xs)
		'()
		(cons (car xs) (copy-r (cdr xs)))))

(defun copy-i (xs)
	(labels (
		(iter (rst acc)
			(if (null rst)
				acc
				(iter (cdr rst) (cons (car rst) acc)))))
		(iter (reverse xs) '())))

(defun copy-lp (xs)
	(loop for el in xs collect el))

(defun split-at-i (n xs)
    (handler-case (assert (<= n (length xs)))
        (error (exc) (format t "~a: ~a~%" "argument out of range" n)))
    (labels (
        (iter (m ds ts)
            (if (= 0 m)
                (values (reverse ts) ds)
                (iter (1- m) (cdr ds) (cons (car ds) ts)))))
        (iter n xs '())))

(defun split-at-lp (n xs)
    (handler-case (assert (<= n (length xs)))
        (error (exc) (format t "~a: ~a~%" "argument out of range" n)))
	(loop for ndx from 0 to n
		for ds-ts = (cons xs '()) then
			(cons (cdar ds-ts) (cons (caar ds-ts) (cdr ds-ts)))
		finally (return (values (reverse (cdr ds-ts)) (car ds-ts)))))
		

(defun take-i (n xs) (multiple-value-bind (ts ds) (split-at-i n xs) ts))
(defun take-lp (n xs) (multiple-value-bind (ts ds) (split-at-lp n xs) ts))

(defun drop-i (n xs) (multiple-value-bind (ts ds) (split-at-i n xs) ds))
(defun drop-lp (n xs) (multiple-value-bind (ts ds) (split-at-lp n xs) ds))

(defun any-every-r (pred xs)
	(if (null xs)
		(cons nil t)
		(cons (or (car (any-every-r pred (cdr xs))) (funcall pred (car xs)))
			(and (cdr (any-every-r pred (cdr xs))) (funcall pred (car xs))))))

(defun any-every-i (pred xs)
	(labels (
		(iter (rst acc)
			(if (null rst)
				acc
				(iter (cdr rst) (cons (or (car acc) (funcall pred (car rst)))
					(and (cdr acc) (funcall pred (car rst))))))))
		(iter xs (cons nil t))))

(defun any-every-lp (pred xs)
	(if (null xs)
		(cons nil t)
		(loop for el in xs
			for acc = (cons (funcall pred el) (funcall pred el)) then
				(cons (or (car acc) (funcall pred el))
					(and (cdr acc) (funcall pred el)))
			finally (return acc))))

(defun any-r (pred xs) (car (any-every-r pred xs)))
(defun any-i (pred xs) (car (any-every-i pred xs)))
(defun any-lp (pred xs) (car (any-every-lp pred xs)))

(defun every-r (pred xs) (cdr (any-every-r pred xs)))
(defun every-i (pred xs) (cdr (any-every-i pred xs)))
(defun every-lp (pred xs) (cdr (any-every-lp pred xs)))

(defun map-r (proc xs)
    (if (null xs)
        '()
        (cons (funcall proc (car xs)) (map-r proc (cdr xs)))))

(defun map-i (proc xs)
    (labels (
        (iter (rst acc)
            (if (null rst)
                (reverse acc)
                (iter (cdr rst) (cons (funcall proc (car rst)) acc)))))
        (iter xs '())))

(defun map-lp (proc xs)
	(loop for el in xs collect (funcall proc el)))

(defun for-each-r (proc xs)
    (if (null (cdr xs))
        (funcall proc (car xs))
        (progn
            (funcall proc (car xs))
            (for-each-r proc (cdr xs)))))

(defun for-each-i (proc xs)
    (labels (
        (iter (rst)
            (if (null (cdr rst))
                (funcall proc (car rst))
                (progn
                    (funcall proc (car rst))
                    (iter (cdr rst))))))
        (iter xs)))

(defun for-each-lp (proc xs)
	(loop for el in xs do (funcall proc el)))

(defun partition-r (pred xs)
	(defun helper (norm prep rst)
		(cond ((null rst) '())
			((funcall norm (funcall pred (car rst)))
				(cons (car rst) (helper norm pred (cdr rst))))
			(t (helper norm pred (cdr rst)))))
	(values (helper #'identity pred xs) (helper #'not pred xs)))

(defun partition-i (pred xs)
	(labels (
		(iter (rst acc)
			(cond ((null rst) (values (reverse (car acc)) (reverse (cdr acc))))
				((funcall pred (car rst))
					(iter (cdr rst) (cons (cons (car rst) (car acc)) (cdr acc))))
				(t (iter (cdr rst) (cons (car acc) (cons (car rst) (cdr acc))))))))
		(iter xs (cons '() '()))))

(defun partition-lp (pred xs)
	(loop for el in xs
		for fs-rs = (if (funcall pred el) (cons (list el) '()) 
				(cons '() (list el))) then
			(if (funcall pred el)
				(cons (cons el (car fs-rs)) (cdr fs-rs))
				(cons (car fs-rs) (cons el (cdr fs-rs))))
		finally (return (values (reverse (car fs-rs)) (reverse (cdr fs-rs))))))

(defun filter-r (pred xs)
	(multiple-value-bind (fs rs) (partition-r pred xs) fs))
(defun filter-i (pred xs)
	(multiple-value-bind (fs rs) (partition-i pred xs) fs))
(defun filter-lp (pred xs)
	(multiple-value-bind (fs rs) (partition-lp pred xs) fs))

(defun remove-r (pred xs)
	(multiple-value-bind (fs rs) (partition-r pred xs) rs))
(defun remove-i (pred xs)
	(multiple-value-bind (fs rs) (partition-i pred xs) rs))
(defun remove-lp (pred xs)
	(multiple-value-bind (fs rs) (partition-lp pred xs) rs))

(defun fold-left-r (corp init xs)
    (if (null xs)
        init
        (fold-left-r corp (funcall corp init (car xs)) (cdr xs))))

(defun fold-left-i (corp init xs)
    (labels (
        (iter (rst acc)
            (if (null rst)
                acc
                (iter (cdr rst) (funcall corp acc (car rst))))))
        (iter xs init)))

(defun fold-left-lp (corp init xs)
	(or (loop for el in xs
		for acc = (funcall corp init el) then
			(funcall corp acc el)
		finally (return acc)) init))

(defun fold-right-r (proc init xs)
    (if (null xs)
        init
        (funcall proc (car xs) (fold-right-r proc init (cdr xs)))))

(defun fold-right-i (proc init xs)
    (labels (
        (iter (rst acc)
            (if (null rst)
                acc
                (iter (cdr rst) (funcall proc (car rst) acc)))))
        (iter (reverse xs) init)))

(defun fold-right-lp (proc init xs)
	(or (loop for el in (reverse xs)
		for acc = (funcall proc el init) then
			(funcall proc el acc)
		finally (return acc)) init))

(defun unfold-right-i (pred func gen seed)
    (labels (
        (iter (cur acc)
            (if (funcall pred cur)
                acc
                (iter (funcall gen cur) (cons (funcall func cur) acc)))))
        (iter seed '())))

(defun unfold-right-lp (pred func gen seed)
	(loop for ini = seed then
		(funcall gen ini)
		until (funcall pred ini)
		for acc = (list (funcall func ini)) then
			(cons (funcall func ini) acc)
		finally (return acc)))

(defun unfold-left-r (pred func gen seed)
    (if (funcall pred seed)
        '()
        (cons (funcall func seed) (unfold-left-r pred func gen
            (funcall gen seed)))))

(defun unfold-left-lp (pred func gen seed)
	(loop for ini = seed then
		(funcall gen ini)
		until (funcall pred ini)
		collect (funcall func ini)))

(defun is-ordered-r (xs &key (cmp #'<=))
	(cond ((> 2 (length xs)) t)
		(t (and (funcall cmp (car xs) (cadr xs))
			(is-ordered-r (cdr xs) :cmp cmp)))))

(defun is-ordered-i (xs &key (cmp #'<=))
	(labels (
		(iter (rst oldval acc)
			(if (null rst)
				acc
				(iter (cdr rst) (car rst)
					(and acc (funcall cmp oldval (car rst)))))))
		(iter (cdr xs) (car xs) t)))

(defun is-ordered-lp (xs &key (cmp #'<=))
	(if (> 2 (length xs))
		t
		(loop for rst = xs then
				(cdr rst)
			until (null (cdr rst))
			for acc = (funcall cmp (car rst) (cadr rst)) then
				(and acc (funcall cmp (car rst) (cadr rst)))
			finally (return acc))))

(defun append-r (xs ys)
    (if (null xs)
        ys
        (cons (car xs) (append-r (cdr xs) ys))))

(defun append-i (xs ys)
    (labels (
        (iter (rst acc)
            (if (null rst)
                acc
                (iter (cdr rst) (cons (car rst) acc)))))
        (iter (reverse xs) ys)))

(defun append-lp (xs ys)
	(or (loop for rst = (reverse xs) then
		(cdr rst)
		until (null rst)
		for acc = (cons (car rst) ys) then
			(cons (car rst) acc)
		finally (return acc)) ys))

(defun interleave-r (xs ys)
    (cond ((null xs) ys)
        ((null ys) xs)
        (t (cons (car xs) (interleave-r ys (cdr xs))))))

(defun interleave-i (xs ys)
    (labels (
        (iter (ws zs acc)
            (cond ((null ws) (append (reverse acc) zs))
                ((null zs) (append (reverse acc) ws))
                (t (iter (cdr ws) (cdr zs)
                    (cons (car zs) (cons (car ws) acc)))))))
        (iter xs ys '())))

(defun interleave-lp (xs ys)
	(loop for ws-zs = (cons xs ys) then
		(cons (cdr ws-zs) (cdar ws-zs))
		until (null (car ws-zs))
		for acc = (list (car xs)) then
			(cons (caar ws-zs) acc)
		finally (return (append (reverse acc) (cdr ws-zs)))))

(defun map2-r (proc xs ys)
	(if (or (null xs) (null ys))
		'()
		(cons (funcall proc (car xs) (car ys)) (map2-r proc (cdr xs) (cdr ys)))))

(defun map2-i (proc xs ys)
	(labels (
		(iter (ws zs acc)
			(if (or (null ws) (null zs))
				(reverse acc)
				(iter (cdr ws) (cdr zs) (cons (funcall proc (car ws) (car zs))
					acc)))))
		(iter xs ys '())))

(defun map2-lp (proc xs ys)
	(loop for ws = xs then
		(cdr ws)
		for zs = ys then
			(cdr zs)
		until (or (null ws) (null zs))
		collect (funcall proc (car ws) (car zs))))

(defun zip-r (xs ys) (map2-r #'list xs ys))
(defun zip-i (xs ys) (map2-i #'list xs ys))
(defun zip-lp (xs ys) (map2-lp #'list xs ys))

(defun unzip-i (zss)
    (labels (
        (iter (rst acc)
            (if (null rst)
                (apply #'values acc)
                (iter (cdr rst) (list (cons (caar rst) (car acc))
					(cons (cadar rst) (cadr acc)))))))
        (iter (reverse zss) '(() ()))))

(defun unzip-lp (zss)
	(loop for el in (reverse zss)
		for acc1 = (list (car el)) then
			(cons (car el) acc1)
		for acc2 = (list (cadr el)) then
			(cons (cadr el) acc2)
		finally (return (values acc1 acc2))))

(defun unzip-m (zss)
	(values (mapcar #'car zss) (mapcar #'cadr zss)))

(defun concat-r (nlsts)
	(if (null nlsts)
		'()
		(append (car nlsts) (concat-r (cdr nlsts)))))

(defun concat-i (nlsts)
	(labels (
		(iter (rst acc)
			(if (null rst)
				acc
				(iter (cdr rst) (append acc (car rst))))))
		(iter nlsts '())))

(defun concat-lp (nlsts)
	(loop for xs in nlsts
		for acc = xs then
			(append acc xs)
		finally (return acc)))

(defun concat-a (nlsts)
	(apply #'append nlsts))


(defun lib-main (argv)
	(format t "(reverse-i ~a): ~a~%" (list 0 1 2) (reverse-i (list 0 1 2)))
	
	);(uiop:quit)

;(if (member (pathname-name *load-truename*) '("src/sequenceops.lisp")
;        :test #'(lambda (x y) (search x y :test #'equalp)))
;    (lib-main (uiop:command-line-arguments)))
