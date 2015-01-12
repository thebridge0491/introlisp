; Common Lisp - comments style conventions
; title       ;;;; Short title for block of code
; intro       ;;;  Description before block of code
; state       ;;   State of program or following code
; explanation ;    Regarding line on which it appears

(defpackage :introlisp.intro/src/lib
  (:documentation "Main app sub-package for Common Lisp Intro examples project")
  (:use :cl)
  (:export :greeting :delay-char)
  )
(in-package :introlisp.intro/src/lib)

;(defparameter *version* "X.Y.Z" "library version.")

(defun greeting (greet-path name)
    "Compose greeting"
    (with-open-file (file-in greet-path)
        ;(concatenate 'string '(#\Newline) (read-line file-in) name "!" 
        ;    '(#\Newline))))
        (format nil "~%~a~a!~%" (read-line file-in) name)))

(defun delay-char (delay-func)
    "Delay for user character input"
    (format t "~a" "Type any character when ready.")
    (force-output)
    (if (not (or (char= (peek-char) #\Newline) (char= (peek-char) #\Nul)))
        (aref (read-line) 0)
        (progn
            (read-line)
            (apply delay-func '())  ; (funcall delay-func)
            (delay-char delay-func))))


(defun lib-main (argv)
	(format t "(delay-char (lambda () (sleep 3)))~%" 
		(delay-char (lambda () (sleep 3))))
	
	);(uiop:quit)

;(if (member (pathname-name *load-truename*) '("src/lib.lisp")
;        :test #'(lambda (x y) (search x y :test #'equalp)))
;    (lib-main (uiop:command-line-arguments)))
