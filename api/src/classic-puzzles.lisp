; Common Lisp - comments style conventions
; title       ;;;; Short title for block of code
; intro       ;;;  Description before block of code
; state       ;;   State of program or following code
; explanation ;    Regarding line on which it appears

(defpackage :introlisp.practice/src/classic-puzzles
    (:documentation "Practice.Classic.Puzzles library")
    (:use :cl)
    (:nicknames :introlisp.practice.classic.puzzles)
    (:export :hanoi :hanoi-moves :nqueens :nqueens-grid)
    )
(in-package :introlisp.practice/src/classic-puzzles)

(defun hanoi (src dest spare num-disks)
    (if (= 0 num-disks)
        '()
        (append (hanoi src spare dest (1- num-disks))
            (list (cons src dest))
            (hanoi spare dest src (1- num-disks)))))

(defun hanoi-moves (src dest spare num-disks)
#|
	; mutable version using setf on vector
    (defun hanoi-pegs (res)
        (reverse (labels ( 
            (iter (vec-pegs lst acc)
                (let ((el1 (- (car (car lst)) 1)) (el2 (- (cdr (car lst)) 1)))
                    (setf (aref vec-pegs el2) (cons
                        (car (aref vec-pegs el1)) (aref vec-pegs el2)))
		            (setf (aref vec-pegs el1) (cdr (aref vec-pegs el1)))
		            (if (consp (cdr lst))
		                (iter vec-pegs (cdr lst) (cons
		                    (copy-seq vec-pegs) acc))
	                    (cons (copy-seq vec-pegs) acc)))))
            (iter (vector (loop for i from 1 to num-disks collect i) '() '())
                res '()))))
|#
    ; immutable version using alists
    (defun hanoi-pegs (res)
        (reverse (labels ( 
            (iter (lst-pegs lst acc)
                (let* ((el1 (- (caar lst) 1)) (el2 (- (cdar lst) 1))
                        (peg-uplst (acons el1 (cddr (assoc el1 lst-pegs))
                            (delete (lambda (el) (= el1 el)) lst-pegs)))
                        (peg-dnlst (acons el2 (cons (cadr (assoc el1 lst-pegs))
                            (cdr (assoc el2 peg-uplst)))
                            (delete (lambda (el) (= el2 el)) peg-uplst))))
		            (if (consp (cdr lst))
		                (iter peg-dnlst (cdr lst) (cons (vector (cdr 
		                    (assoc 0 peg-dnlst)) (cdr (assoc 1 peg-dnlst))
		                    (cdr (assoc 2 peg-dnlst))) acc))
	                    (cons (vector (cdr 
		                    (assoc 0 peg-dnlst)) (cdr (assoc 1 peg-dnlst))
		                    (cdr (assoc 2 peg-dnlst))) acc)))))
            (iter (list (cons 0 (loop for i from 1 to num-disks collect i))
                (cons 1 '()) (cons 2 '())) res '()))))

    (defun stat-txt (res-len)
        (let ((calc-len (- (expt 2 num-disks) 1)))
            (format nil (if (= calc-len res-len)
                "((n = ~a) 2^n - 1 = ~a) == (length(result) = ~a)~%"
                "((n = ~a) 2^n - 1 = ~a) != (length(result) = ~a)~%")
                num-disks calc-len res-len)))
    (let* ((txt-fmt "'move from ~a to ~a'")
            (res (hanoi src dest spare num-disks))
            (proc1 (lambda (el) (format nil txt-fmt (car el) (cdr el))))
            (proc2 (lambda (el) (format nil "#(~a ~a ~a)" (aref el 0)
                (aref el 1) (aref el 2)))))
        (values (list res) (list (stat-txt (length res))
                (make-string 40 :initial-element #\-))
            (mapcar #'list (mapcar proc1 res)
                (map 'list proc2 (hanoi-pegs res))))))

(defun nqueens (n)
    (defun threatp (q1 q2)
	    (or (= (car q1) (car q2))
		    (= (cdr q1) (cdr q2))
          	(= (abs (- (car q1) (car q2))) (abs (- (cdr q1) (cdr q2))))))

    (defun safep (pos placed-set)
	    (cond ((null placed-set) t)
        	((threatp pos (car placed-set)) nil)
            (t (safep pos (cdr placed-set)))))
  	(labels (
  	    (iter (col row placed-set board)
        	(cond ((< (1- n) col) (cons (reverse placed-set) board))
              	((< (1- n) row) board)
              	((safep (cons col row) placed-set)
              		(iter col (1+ row) placed-set
              		    (iter (1+ col) 0 (cons (cons col row) placed-set)
              		        board)))
              	(t (iter col (1+ row) placed-set board)))))
      	(iter 0 0 '() '())))
#|
; mutable version using setf on 2D array
(defun nqueens-grid (num-queens answer)
    (let* ((arr2d (make-array (list (1+ num-queens) (1+ num-queens))
                :initial-element " "))
            (calc-grid (lambda (el) (abs (- (1+ el) num-queens))))
            (lst (loop for i from 0 below num-queens collect i)))
        (progn
            (mapcar (lambda (el) (setf (aref arr2d (funcall calc-grid el) 0)
                (princ-to-string el))) lst)
            (mapcar (lambda (el) (setf (aref arr2d num-queens (1+ el))
                (code-char (+ el (char-code #\a))))) lst)
            
            (mapcar (lambda (el) (setf (aref arr2d
                    (funcall calc-grid (cdr el)) (1+ (car el))) "Q"))
                answer)
            nil)
        arr2d))
|#
; immutable version using lists; then convert to 2D array
(defun nqueens-grid (num-queens answer)
	(defun mk-row (acc tup)
		(let ((lst-blank (loop for i from 0 below num-queens 
				collect (cons i " "))))
			(cons (cons (cdr tup)
				(mapcar (lambda (pr) (if (equal (car tup) (car pr)) 
					"Q" (cdr pr)))
					lst-blank)) acc)))
	(let* ((lst-ltrs (cons " " (loop for i from 0 below num-queens
			collect (code-char (+ i (char-code #\a)))))))
        (apply #'make-array (list (1+ num-queens) (1+ num-queens))
            :initial-contents (list (reduce #'mk-row
                (sort answer (lambda (t1 t2)
                (< (cdr t1) (cdr t2))))
            :initial-value (list lst-ltrs))))))
