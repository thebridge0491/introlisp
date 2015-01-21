; Common Lisp - comments style conventions
; title       ;;;; Short title for block of code
; intro       ;;;  Description before block of code
; state       ;;   State of program or following code
; explanation ;    Regarding line on which it appears

(defpackage :introlisp.practice/core
  (:use :cl)
  (:import-from :introlisp.practice/src/lib)
  (:import-from :introlisp.practice/src/classic)
  (:import-from :introlisp.practice/src/sequenceops)
  (:import-from :introlisp.practice/src/classic-puzzles)
  
  )
(in-package :introlisp.practice/core)

(defvar rnd-state (make-random-state t))
