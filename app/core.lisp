; Common Lisp - comments style conventions
; title       ;;;; Short title for block of code
; intro       ;;;  Description before block of code
; state       ;;   State of program or following code
; explanation ;    Regarding line on which it appears

(defpackage :introlisp.intro/core
  (:use :cl)
  (:import-from :introlisp.intro/src/lib)
  (:import-from :introlisp.intro/src/person)
  (:import-from :introlisp.intro/src/main)
  
  )
