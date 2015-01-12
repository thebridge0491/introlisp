; Common Lisp - comments style conventions
; title       ;;;; Short title for block of code
; intro       ;;;  Description before block of code
; state       ;;   State of program or following code
; explanation ;    Regarding line on which it appears

(defpackage :introlisp.util/src/person
  (:documentation "Person class")
  (:use :cl :log4cl)
  (:nicknames :introlisp.util.person)
  (:export :<person> :person-name :person-age :person-p :name :age
    :peek-person)
  )
(in-package :introlisp.util/src/person)

(defclass <person> ()
    ((name :initform "John" :initarg :name :accessor person-name :type string 
            :documentation "Person's name")
        (age :initform 18 :initarg :age :accessor person-age :type fixnum 
            :documentation "Person's age"))
    (:documentation "Person Class"))

(defmethod initialize-instance :after ((p <person>) &key name age)
	(log:debug '(root) "Person()"))

(defgeneric person-p (p)
    (:documentation "Generic Person predicate"))

(defmethod person-p ((p <person>))
    "Person-p predicate"
    (cond
        ((typep p '<person>))
        (t nil)))

(defgeneric peek-person (p)
    (:documentation "Generic Person peek state"))

(defmethod peek-person ((p <person>))
    "Person peek state"
    (format nil "person: " (person-name p) (person-age p)))
