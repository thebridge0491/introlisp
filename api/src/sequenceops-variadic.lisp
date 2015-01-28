; Common Lisp - comments style conventions
; title       ;;;; Short title for block of code
; intro       ;;;  Description before block of code
; state       ;;   State of program or following code
; explanation ;    Regarding line on which it appears

(defpackage :introlisp.practice/src/sequenceops-variadic
    (:documentation "Practice.Sequenceops.Variadic library")
    (:use :cl)
    (:nicknames :introlisp.practice.sequenceops.variadic)
    (:export :any-rv :any-iv :any-lpv :every-rv :every-iv :every-lpv 
		:map-rv :map-iv :map-lpv :for-each-rv :for-each-iv :for-each-lpv
		:fold-left-rv :fold-left-iv :fold-left-lpv :fold-right-rv 
		:fold-right-iv :fold-right-lpv
		:append-rv :append-iv :append-lpv :zip-rv :zip-iv :zip-lpv
		
		:any-fv :every-fv :map-fv :for-each-fv :append-fv :zip-fv
		
		:any-uv :every-uv :map-uv :for-each-uv :append-uv :zip-uv
		)
    )
(in-package :introlisp.practice/src/sequenceops-variadic)

(defun any-every-rv (pred xs &rest yss)
	(let ((nlsts (cons xs yss)))
		(if (some #'null nlsts)
			(cons nil t)
			(cons (or (apply pred (mapcar #'car nlsts))
				(car (apply #'any-every-rv pred (mapcar #'cdr nlsts))))
				(and (apply pred (mapcar #'car nlsts))
				(cdr (apply #'any-every-rv pred (mapcar #'cdr nlsts))))))))

(defun any-rv (pred xs &rest yss) (car (apply #'any-every-rv pred xs yss)))

(defun every-rv (pred xs &rest yss) (cdr (apply #'any-every-rv pred xs yss)))

(defun any-every-iv (pred xs &rest yss)
    (labels (
		(iter (rst acc)
			(if (some #'null rst)
				acc
				(iter (mapcar #'cdr rst) (cons (or (car acc) (apply pred (mapcar #'car rst))) (and (cdr acc) (apply pred (mapcar #'car rst))))))))
		(iter (cons xs yss) (cons nil t))))

(defun any-iv (pred xs &rest yss) (car (apply #'any-every-iv pred xs yss)))

(defun every-iv (pred xs &rest yss) (cdr (apply #'any-every-iv pred xs yss)))

(defun any-every-lpv (pred xs &rest yss)
    (or (loop for rst = (cons xs yss) then
		(mapcar #'cdr rst)
		until (some #'null rst)
		for acc = (cons (apply pred (mapcar #'car rst)) 
				(apply pred (mapcar #'car rst))) then
			(cons (or (apply pred (mapcar #'car rst)) (car acc))
				(and (apply pred (mapcar #'car rst)) (cdr acc)))
		finally (return acc)) (cons nil t)))

(defun any-lpv (pred xs &rest yss) (car (apply #'any-every-lpv pred xs yss)))

(defun every-lpv (pred xs &rest yss) (cdr (apply #'any-every-lpv pred xs yss)))

(defun map-rv (proc xs &rest yss)
	(let ((nlsts (cons xs yss)))
		(if (some #'null nlsts)
			'()
			(cons (apply proc (mapcar #'car nlsts))
				(apply #'map-rv proc (mapcar #'cdr nlsts))))))

(defun map-iv (proc xs &rest yss)
    (labels (
		(iter (rst acc)
			(if (some #'null rst)
				(reverse acc)
				(iter (mapcar #'cdr rst) (cons (apply proc (mapcar #'car rst)) acc)))))
		(iter (cons xs yss) '())))

(defun map-lpv (proc xs &rest yss)
    (loop for rst = (cons xs yss) then
		(mapcar #'cdr rst)
		until (some #'null rst)
		for acc = (cons (apply proc (mapcar #'car rst)) '()) then
			(cons (apply proc (mapcar #'car rst)) acc)
		finally (return (reverse acc))))

(defun for-each-rv (proc xs &rest yss)
	(let ((nlsts (cons xs yss)))
		(if (some #'null nlsts)
			nil
			(progn
				(apply proc (mapcar #'car nlsts))
				(apply #'for-each-rv proc (mapcar #'cdr nlsts))))))

(defun for-each-iv (proc xs &rest yss)
    (labels (
		(iter (rst)
			(if (some #'null rst)
				nil
				(progn
					(apply proc (mapcar #'car rst))
					(iter (mapcar #'cdr rst))))))
		(iter (cons xs yss))))

(defun for-each-lpv (proc xs &rest yss)
    (loop for rst = (cons xs yss) then
		(mapcar #'cdr rst)
		until (some #'null rst)
		for acc = (apply proc (mapcar #'car rst)) then
			(apply proc (mapcar #'car rst))
		finally (return acc)))

(defun fold-left-rv (corp init xs &rest yss)
	(let ((nlsts (cons xs yss)))
		(if (some #'null nlsts)
			init
			(apply #'fold-left-rv corp (apply corp init (mapcar #'car nlsts))
				(mapcar #'cdr nlsts)))))

(defun fold-left-iv (corp init xs &rest yss)
    (labels (
		(iter (rst acc)
			(if (some #'null rst)
				acc
				(iter (mapcar #'cdr rst) (apply corp acc (mapcar #'car rst))))))
		(iter (cons xs yss) init)))

(defun fold-left-lpv (corp init xs &rest yss)
    (or (loop for rst = (cons xs yss) then
		(mapcar #'cdr rst)
		until (some #'null rst)
		for acc = (apply corp init (mapcar #'car rst)) then
			(apply corp acc (mapcar #'car rst))
		finally (return acc)) init))

(defun fold-right-rv (proc init xs &rest yss)
	(let ((nlsts (cons xs yss)))
		(if (some #'null nlsts)
			init
			(apply proc (append (mapcar #'car nlsts)
				(list (apply #'fold-right-rv proc init
					(mapcar #'cdr nlsts))))))))

(defun fold-right-iv (proc init xs &rest yss)
    (labels (
		(iter (rst acc)
			(if (null rst)
				acc
				(iter (cdr rst) (apply proc (append (car rst) (list acc)))))))
		(iter (reverse (apply #'mapcar #'list xs yss)) init)))

(defun fold-right-lpv (proc init xs &rest yss)
    (or (loop for rst = (reverse (apply #'mapcar #'list xs yss)) then
		(cdr rst)
		until (null rst)
		for acc = (apply proc (append (car rst) (list init))) then
			(apply proc (append (car rst) (list acc)))
		finally (return acc)) init))

(defun append-rv (xs &rest yss)
	(cond ((null yss) xs)
        ((null xs) (apply #'append-rv (car yss) (cdr yss)))
        ;((and (listp yss) (= 2 (length (apply #'list xs yss))))
        ;    (apply #'append xs yss))
        (t (cons (car xs) (apply #'append-rv (cdr xs) yss)))))

(defun append-iv (xs &rest yss)
    (labels (
		(iter (rst acc)
			(if (null rst)
				acc
				(iter (cdr rst) (append (car rst) acc)))))
		(iter (reverse (cons xs yss)) '())))

(defun append-lpv (xs &rest yss)
    (loop for rst = (reverse (cons xs yss)) then
		(cdr rst)
		until (null rst)
		for acc = (car rst) then
			(append (car rst) acc)
		finally (return acc)))

(defun zip-rv (xs &rest yss) (apply #'map-rv #'list xs yss))
(defun zip-iv (xs &rest yss) (apply #'map-iv #'list xs yss))
(defun zip-lpv (xs &rest yss) (apply #'map-lpv #'list xs yss))



(defun any-every-fv (pred xs &rest yss)
	(reduce (lambda (a els) (cons (or (car a) (apply pred els))
		(and (cdr a) (apply pred els)))) (apply #'mapcar #'list xs yss)
		:initial-value (cons nil t)))

(defun any-fv (pred xs &rest yss) (car (apply #'any-every-fv pred xs yss)))

(defun every-fv (pred xs &rest yss) (cdr (apply #'any-every-fv pred xs yss)))

(defun map-fv (proc xs &rest yss)
	(reduce (lambda (els a) (cons (apply proc els) a))
		(apply #'mapcar #'list xs yss) :initial-value '() :from-end t))

(defun for-each-fv (proc xs &rest yss)
	(reduce (lambda (a els) (apply proc els)) (apply #'mapcar #'list xs yss)
		:initial-value nil))

(defun append-fv (xs &rest yss)
	(reduce #'append (cons xs yss) :initial-value '() :from-end t))

(defun zip-fv (xs &rest yss) (apply #'map-fv #'list xs yss))


(defun unfold-right (pred func gen seed)
	(loop for ini = seed then
		(funcall gen ini)
		until (funcall pred ini)
		for acc = (list (funcall func ini)) then
			(cons (funcall func ini) acc)
		finally (return acc)))

(defun any-every-uv (pred xs &rest yss)
	(let ((func (lambda (tup) 
            (cons (or (caar tup) (apply pred (mapcar #'car (cdr tup)))) 
				(and (cdar tup) (apply pred (mapcar #'car (cdr tup))))))))
        (or (car (unfold-right (lambda (tup) (some #'null (cdr tup)))
            func (lambda (tup) (cons (funcall func tup) (mapcar #'cdr (cdr tup))))
            (cons (cons nil t) (cons xs yss)))) (cons nil t))))

(defun any-uv (pred xs &rest yss) (car (apply #'any-every-uv pred xs yss)))

(defun every-uv (pred xs &rest yss) (cdr (apply #'any-every-uv pred xs yss)))

(defun map-uv (proc xs &rest yss)
	(let ((func (lambda (tup) (cons (apply proc (mapcar #'car (cdr tup)))
            (car tup)))))
        (reverse (car (unfold-right
            (lambda (tup) (some #'null (cdr tup))) func
            (lambda (tup) (cons (funcall func tup) (mapcar #'cdr (cdr tup))))
            (cons '() (cons xs yss)))))))

(defun for-each-uv (proc xs &rest yss)
	(let ((func (lambda (tup) (apply proc (mapcar #'car (cdr tup))))))
        (car (unfold-right
            (lambda (tup) (some #'null (cdr tup))) func
            (lambda (tup) (cons (funcall func tup) (mapcar #'cdr (cdr tup))))
            (cons nil (cons xs yss))))))

(defun append-uv (xs &rest yss)
	(let ((func (lambda (tup) (append (cadr tup) (car tup)))))
        (car (unfold-right
            (lambda (tup) (null (cdr tup))) func
            (lambda (tup) (cons (funcall func tup) (cddr tup)))
            (cons '() (reverse (cons xs yss)))))))

(defun zip-uv (xs &rest yss) (apply #'map-uv #'list xs yss))
