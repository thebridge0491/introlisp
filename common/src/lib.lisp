; Common Lisp - comments style conventions
; title       ;;;; Short title for block of code
; intro       ;;;  Description before block of code
; state       ;;   State of program or following code
; explanation ;    Regarding line on which it appears

(defpackage :introlisp.util/src/lib
  (:documentation "Utilities sub-package for Common Lisp Intro examples project.")
  (:use :cl)
  (:nicknames :introlisp.util)
  (:export :date-to-string :display-nested :mkstring-nested :echo-invoke
    :range-cnt :in-epsilon :cartesian-prod :arr2d-to-nlsts :hashtbl->alist)
  )
(in-package :introlisp.util/src/lib)

;(defparameter *version* "X.Y.Z" "library version.")

(defun day-names ()
    "Day names list"
    '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))

(defun month-names ()
    "Month names list"
    '("Dec" "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov"))

(defun date-to-string (time1)
    "Format decoded time to string"
    (multiple-value-bind (second minute hour date month year wkday 
            dstp tz) 
        (decode-universal-time time1)
        (format nil "~a ~d ~2,'0d ~2,'0d:~2,'0d:~2,'0d (GMT~@d) ~d" 
            (nth wkday (day-names)) (nth month (month-names)) date hour 
            minute second (- (if dstp -1 0) tz) year)))

(defun echo-invoke (func &rest args)
    "Echo invocation of function call"
    (multiple-value-bind (ans &rest rst) (apply func args)
        (format nil "~a: ~a~%" (cons func args) 
            (if (null rst) ans (cons ans rst)))))

(defun display-nested (sep final lsts)
    "Display nested list of lists"
    (progn
        (mapcar (lambda (el) 
            (if (listp el) 
                (display-nested sep final el)
                (format t "~a~a" el sep))
            (format t "~a" final)) lsts)
        nil))
#|
(defun mkstring-nested (init sep final lsts)
    "Make string from nested list of lists"
    (concatenate 'string
        (reduce (lambda (acc el)
                (if (listp el)
                    (format nil "~a~a" acc 
                        (mkstring-nested "" sep (string #\Newline) el))
                    (format nil "~a~a~a" acc el sep)))
            lsts :initial-value init)
        final))
|#

(defun mkstring-nested (init sep final lsts)
    "Make string from nested list of lists"
    (concatenate 'string
        (reduce (lambda (acc el)
                (if (and (consp el) (not (atom (cdr el))))
                    (format nil "~a~a" acc 
                        (mkstring-nested "" sep (string #\Newline) el))
                    (format nil "~a~a~a" acc el sep)))
            lsts :initial-value init :from-end nil)
        final))

(defun range-cnt (start cnt)
	"Range of (cnt) numbers from start"
	;(loop for i = 0 then (1+ i) until (<= cnt i) collect (+ start i))
	(loop for i from 0 below cnt collect (+ start i))
	)

(defun in-epsilon (a b &optional (tolerance 0.001))
	(let ((delta (abs tolerance)))
		;(and (<= (- a delta) b) (>= (+ (a delta) b))))
		(and (not (< (+ a delta) b)) (not (< (+ b delta) a)))))

(defun cartesian-prod (xs ys)
	"Cartesian product of 2 lists"
	#|(loop for x in xs nconc (loop for y in ys collect (cons x y)))|#
	(mapcan (lambda (x) (mapcar (lambda (y) (cons x y)) ys)) xs)
	)

(defun arr2d-to-nlsts (arr2d)
	"Convert 2D array to nested list of lists"
    (let* ((x (array-dimension arr2d 0)) (y (/ (array-total-size arr2d) x)))
        (mapcar (lambda (row) (mapcar (lambda (col)
                (aref arr2d row col))
                    (loop for i from 0 below y collect i)))
            (loop for i from 0 below x collect i))))
#|
(defun mkstring-htbl (htbl)
	(with-hash-table-iterator (next-entry htbl)
		(loop
			collect (multiple-value-bind (more? k v) (next-entry)
				(unless more? (return lst))
				(format nil "{~a: ~a} " k v)) into lst))
	)
|#
(defun hashtbl->alist (htbl)
	#|
	(with-hash-table-iterator (next-entry htbl)
		(loop
			collect (multiple-value-bind (more? k v) (next-entry)
				(unless more? (return alst))
				(cons k v)) into alst))
	|#
	(mapcar (lambda (k) (cons k (gethash k htbl)))
		(loop for keyX being the hash-keys of htbl collect keyX))
	)

(defun lib-main (argv)
	(format t "(cartesian-prod ~a ~a): ~a~%" '(0 1 2) '(10 20 30)
		(cartesian-prod '(0 1 2) '(10 20 30)))
	
	);(uiop:quit)

;(if (member (pathname-name *load-truename*) '("src/lib.lisp")
;        :test #'(lambda (x y) (search x y :test #'equalp)))
;    (lib-main (uiop:command-line-arguments)))
