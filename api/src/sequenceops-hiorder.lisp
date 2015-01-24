; Common Lisp - comments style conventions
; title       ;;;; Short title for block of code
; intro       ;;;  Description before block of code
; state       ;;   State of program or following code
; explanation ;    Regarding line on which it appears

(defpackage :introlisp.practice/src/sequenceops-hiorder
    (:documentation "Practice.Sequenceops.Hiorder library")
    (:use :cl)
    (:nicknames :introlisp.practice.sequenceops.hiorder)
    (:export :tabulate-f :length-f :nth-f :index-f :find-f :min-f :max-f
		:reverse-f :copy-f :split-at-f :take-f :drop-f :any-f :every-f
		:map-f :for-each-f :partition-f :filter-f :remove-f 
		
		:is-ordered-f
		
		:append-f :interleave-f :map2-f :zip-f :unzip-f :concat-f
		
		:tabulate-u :length-u :nth-u :index-u :find-u :min-u :max-u
		:reverse-u :copy-u :split-at-u :take-u :drop-u :any-u :every-u
		:map-u :for-each-u :partition-u :filter-u :remove-u 
		
		:is-ordered-u
		
		:append-u :interleave-u :map2-u :zip-u :unzip-u :concat-u
		)
    )
(in-package :introlisp.practice/src/sequenceops-hiorder)

(defun range-cnt (cnt &key (start 0) (step 1))
	(if (< 0 step)
		(loop for i from 0 below cnt by step collect (+ start i))
		(loop for i downfrom 0 above cnt by (abs step) collect (+ start i))))

(defun tabulate-f (func cnt)
	(reverse (reduce (lambda (a e) (cons (funcall func e) a)) (range-cnt cnt) :initial-value '())))

(defun length-f (xs)
	(reduce (lambda (a e) (1+ a)) xs :initial-value 0))

(defun nth-f (idx xs)
	(handler-case (assert (> (length xs) idx))
		(error (exc) (format t "index out of range: ~a~%" idx)))
	(cdr (reduce (lambda (a e) (if (= 0 (car a)) (cons (1- (car a)) e)
            (cons (1- (car a)) (cdr a)))) xs :initial-value (cons idx nil))))

(defun index-find-f (pred xs &optional (ndx 0))
	(reduce (lambda (a i-e) (if (and (not (cdr a)) (funcall pred (cdr i-e)))
		i-e a)) (mapcar #'cons (range-cnt (length xs)) xs) 
		:initial-value (cons nil nil)))

(defun index-f (pred xs) (car (index-find-f pred xs)))

(defun find-f (pred xs) (cdr (index-find-f pred xs)))

(defun min-max-f (x &rest args)
	(apply #'values (reduce (lambda (lo-hi e)
        (cond ((< e (car lo-hi)) (list e (cadr lo-hi)))
            ((> e (cadr lo-hi)) (list (car lo-hi) e))
            (t lo-hi))) args :initial-value (list x x))))

(defun min-f (x &rest args)
	(multiple-value-bind (lo hi) (apply #'min-max-f x args) lo))

(defun max-f (x &rest args)
	(multiple-value-bind (lo hi) (apply #'min-max-f x args) hi))

(defun reverse-f (xs)
	(reduce (lambda (a e) (cons e a)) xs :initial-value '()))

(defun copy-f (xs) (reduce #'cons xs :initial-value '() :from-end t))

(defun split-at-f (n xs)
	(handler-case (assert (<= n (length xs)))
        (error (exc) (format t "~a: ~a~%" "argument out of range" n)))
	(let ((res (reduce (lambda (t-d i) (list (cons (caadr t-d) (car t-d)) (cdadr t-d))) (range-cnt n) :initial-value (list '() xs))))
		(values (reverse (car res)) (cadr res))))
	

(defun take-f (n xs) (multiple-value-bind (ts ds) (split-at-f n xs) ts))

(defun drop-f (n xs) (multiple-value-bind (ts ds) (split-at-f n xs) ds))

(defun any-every-f (pred xs)
	(reduce (lambda (a e) (cons (or (car a) (funcall pred e)) (and (cdr a) (funcall pred e)))) xs :initial-value (cons nil t)))

(defun any-f (pred xs) (car (any-every-f pred xs)))

(defun every-f (pred xs) (cdr (any-every-f pred xs)))

(defun map-f (proc xs)
	(reduce (lambda (e a) (cons (funcall proc e) a)) xs :initial-value '() 
	:from-end t))

(defun for-each-f (proc xs)
	(reduce (lambda (a e) (funcall proc e)) xs :initial-value nil))

(defun partition-f (pred xs)
	(let ((res (reduce (lambda (f-r e)
            (if (funcall pred e)
                (list (cons e (car f-r)) (cadr f-r))
                (list (car f-r) (cons e (cadr f-r)))))
            xs :initial-value (list '() '()))))
        (values (reverse (car res)) (reverse (cadr res)))))

(defun filter-f (pred xs)
	(multiple-value-bind (fs rs) (partition-f pred xs) fs))

(defun remove-f (pred xs)
	(multiple-value-bind (fs rs) (partition-f pred xs) rs))

(defun is-ordered-f (xs &key (cmp #'<=))
	(if (> 2 (length xs))
		t
		(cdr (reduce (lambda (old-acc e) (cons e
                (and (cdr old-acc) (funcall cmp (car old-acc) e))))
			(cdr xs) :initial-value (cons (car xs) t)))))


(defun append-f (xs ys) (reduce #'cons xs :initial-value ys :from-end t))

(defun interleave-f (xs ys)
	(let ((extra (if (> (length xs) (length ys))
			(nthcdr (length ys) xs) (nthcdr (length xs) ys))))
		;(append (mapcan (lambda (x y) (list x y)) xs ys) extra)
		(reduce (lambda (x-y a) (append (list (car x-y) (cadr x-y)) a))
			(mapcar #'list xs ys) :initial-value extra :from-end t)
		))

(defun map2-f (proc xs ys)
	;(mapcar proc xs ys)
	(reduce (lambda (x-y a) (cons (funcall proc (car x-y) (cadr x-y)) a)) (mapcar #'list xs ys) :initial-value '() :from-end t)
	)

(defun zip-f (xs ys) (map2-f #'list xs ys))

(defun unzip-f (zss)
	(apply #'values (reduce (lambda (e a)
		(list (cons (car e) (car a)) (cons (cadr e) (cadr a)))) zss 
		:initial-value (list '() '()) :from-end t)))

(defun concat-f (nlsts)
	(reduce (lambda (a e) (append e a)) (reverse nlsts) :initial-value '()))



(defun unfold-right (pred func gen seed)
	(loop for ini = seed then
		(funcall gen ini)
		until (funcall pred ini)
		for acc = (list (funcall func ini)) then
			(cons (funcall func ini) acc)
		finally (return acc)))

(defun tabulate-u (func cnt)
	(unfold-right (lambda (idx) (> 0 idx))
        (lambda (idx) (funcall func idx)) (lambda (idx) (1- idx)) (1- cnt)))

(defun length-u (xs)
	(let ((func (lambda (tup) (1+ (car tup)))))
        (car (append (unfold-right (lambda (tup) (null (cdr tup))) func
            (lambda (tup) (cons (funcall func tup) (cddr tup))) (cons 0 xs))
            '(0)))))

(defun nth-u (idx xs)
	(handler-case (assert (> (length xs) idx))
		(error (exc) (format t "index out of range: ~a~%" idx)))
	(let ((func (lambda (i-e-rst) (if (= idx (car i-e-rst)) (caaddr i-e-rst) (cadr i-e-rst)))))
		(car (unfold-right (lambda (i-e-rst) (< idx (car i-e-rst))) func
			(lambda (i-e-rst) (list (1+ (car i-e-rst)) (funcall func i-e-rst)
				(cdaddr i-e-rst)))
			(list 0 nil xs)))))

(defun index-find-u (pred xs &optional (ndx 0))
	(let ((zlst (mapcar #'cons (range-cnt (length xs)) xs))
			(func (lambda (tup) (if (and (not (cdar tup))
				(funcall pred (cdadr tup))) (cadr tup) (car tup)))))
		(car (unfold-right (lambda (tup) (or (null (cdr tup)) (cdar tup)))
            func (lambda (tup) (cons (funcall func tup) (cddr tup)))
            (cons (cons nil nil) zlst)))))

(defun index-u (pred xs) (car (index-find-u pred xs)))

(defun find-u (pred xs) (cdr (index-find-u pred xs)))

(defun min-max-u (x &rest args)
	(let* ((func (lambda (trip)
                (cond ((< (caar trip) (cadr trip))
                        (cons (caar trip) (cddr trip)))
                    ((> (caar trip) (cddr trip))
                        (cons (cadr trip) (caar trip)))
                    (t (cons (cadr trip) (cddr trip))))))
            (res (or (car (unfold-right (lambda (trip) (null (car trip))) func
                (lambda (trip) (cons (cdar trip) (funcall func trip)))
                (cons args (cons x x)))) (cons x x))))
        (values (car res) (cdr res))))

(defun min-u (x &rest args)
	(multiple-value-bind (lo hi) (apply #'min-max-u x args) lo))

(defun max-u (x &rest args)
	(multiple-value-bind (lo hi) (apply #'min-max-u x args) hi))

(defun reverse-u (xs) (unfold-right #'null #'car #'cdr xs))

(defun copy-u (xs) (reverse (unfold-right #'null #'car #'cdr xs)))

(defun split-at-u (n xs)
	(handler-case (assert (<= n (length xs)))
        (error (exc) (format t "~a: ~a~%" "argument out of range" n)))
	(let* ((func (lambda (trip) (cons (cons (caddr trip) (cadr trip))
                (cdddr trip))))
            (res (or (car (unfold-right (lambda (trip)
				(or (null (caddr trip)) (= n (car trip)))) func
                (lambda (trip) (cons (+ (car trip) 1) (funcall func trip)))
                (cons 0 (cons '() xs)))) (cons '() xs))))
        (values (reverse (car res)) (cdr res))))

(defun take-u (n xs) (multiple-value-bind (ts ds) (split-at-u n xs) ts))

(defun drop-u (n xs) (multiple-value-bind (ts ds) (split-at-u n xs) ds))

(defun any-every-u (pred xs)
	(let ((func (lambda (tup) (cons (or (caar tup) (funcall pred (cadr tup)))
                (and (cdar tup) (funcall pred (cadr tup)))))))
        (or (car (unfold-right (lambda (tup) (null (cdr tup))) func
            (lambda (tup) (cons (funcall func tup) (cddr tup)))
            (cons (cons nil t) xs))) (cons nil t))))

(defun any-u (pred xs) (car (any-every-u pred xs)))

(defun every-u (pred xs) (cdr (any-every-u pred xs)))

(defun map-u (proc xs)
	(reverse (unfold-right #'null (lambda (lst)(funcall proc (car lst)))
		#'cdr xs)))

(defun for-each-u (proc xs)
	(car (unfold-right #'null (lambda (lst) (funcall proc (car lst)))
		#'cdr xs)))

(defun partition-u (pred xs)
	(let* ((func (lambda (trip) (if (funcall pred (caar trip))
                (cons (cons (caar trip) (cadr trip)) (cddr trip))
                (cons (cadr trip) (cons (caar trip) (cddr trip))))))
            (res (car (unfold-right (lambda (trip) (null (car trip))) func
                (lambda (trip) (cons (cdar trip) (funcall func trip)))
                (cons xs (cons '() '()))))))
        (values (reverse (car res)) (reverse (cdr res)))))

(defun filter-u (pred xs)
	(multiple-value-bind (fs rs) (partition-u pred xs) fs))

(defun remove-u (pred xs)
	(multiple-value-bind (fs rs) (partition-u pred xs) rs))

(defun is-ordered-u (xs &key (cmp #'<=))
	(let* ((func (lambda (trip) (and (caddr trip)
			(funcall cmp (car trip) (caadr trip))))))
        (if (> 2 (length xs))
            t
            (car (unfold-right (lambda (trip) (null (cadr trip))) func
                (lambda (trip) (list (caadr trip) (cdadr trip)
					(funcall func trip)))
                (list (car xs) (cdr xs) t))))))


(defun append-u (xs ys)
	(let ((func (lambda (tup) (cons (cadr tup) (car tup)))))
        (or (car (unfold-right (lambda (tup) (null (cdr tup))) func
            (lambda (tup) (cons (funcall func tup) (cddr tup)))
            (cons ys (reverse xs)))) ys)))

(defun interleave-u (xs ys)
	(let ((extra (if (> (length xs) (length ys))
			(nthcdr (length ys) xs) (nthcdr (length xs) ys)))
            (func (lambda (tup) (append (list (caadr tup) (cadadr tup))
                (car tup)))))
        (or (car (unfold-right (lambda (tup) (null (cdr tup))) func
            (lambda (tup) (cons (funcall func tup) (cddr tup)))
            (cons extra (reverse (mapcar #'list xs ys))))) extra)))

(defun map2-u (proc xs ys)
	(reverse (unfold-right (lambda (tup) (some #'null tup))
        (lambda (tup) (funcall proc (caar tup) (caadr tup)))
        (lambda (tup) (list (cdar tup) (cdadr tup))) (list xs ys))))

(defun zip-u (xs ys) (map2-u #'list xs ys))

(defun unzip-u (zss)
	(let ((yss (reverse zss)))
		(values (unfold-right #'null #'caar #'cdr yss)
			(unfold-right #'null #'cadar #'cdr yss))))

(defun concat-u (nlsts)
	(let ((func (lambda (tup) (append (cadr tup) (car tup)))))
        (car (unfold-right (lambda (tup) (null (cdr tup))) func
            (lambda (tup) (cons (funcall func tup) (cddr tup)))
            (cons '() (reverse nlsts))))))
