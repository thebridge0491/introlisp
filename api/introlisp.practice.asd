;; -*- Lisp -*-

#|
  This file is a part of introlisp.practice project.
|#
#|
  Practice sub-package for Common Lisp Intro examples project.
|#
#-asdf3.1 (error "Introlisp.Practice requires ASDF >= 3.1.2, please upgrade asdf")

(defsystem :introlisp.practice
  :description "Practice sub-package for Common Lisp Intro examples project."
  :long-description
  #.(read-file-string (subpathname *load-pathname* "README.rst"))
  :author "thebridge0491"
  :license "Apache-2.0"
  :homepage "https://bitbucket.org/thebridge0491/introlisp"
  :source-control (:git "https://bitbucket.org/thebridge0491/introlisp.git")
  ;:version (:read-file-form "package.lisp" :at (2 2))
  :version (:read-file-form "version.sexp")
  ;:defsystem-depends-on ((:version "asdf-package-system" "3.1.0.116"))
  
  :class :package-inferred-system
  :serial t
  :depends-on (:introlisp.practice/core
               :log4cl
               :clazy
              )
#|
  :components ((:module "src"
                :components ((:file "lib"))
               ))
|#
  :in-order-to ((test-op (test-op :introlisp.practice/test)))
  )

(defsystem :introlisp.practice/test
  :description "Test system for introlisp.practice"
  :author "thebridge0491"
  
  :pathname "tests/"
  :serial t
  :depends-on (:introlisp.practice
               :introlisp.util
               :cl-quickcheck
               :fiveam
               )
#|
  :components ((:file "test-suite") (:file "tc-new") (:file "tp-new")
			   )
|#  
  ;:perform (test-op :after (op c) (symbol-call :fiveam :run-all-tests))
  ;:perform (test-op :after (op c) (symbol-call :fiveam :run! (find-symbol* 'suite '#:introlisp.practice/test)))
  :perform (test-op :after (op c) (symbol-call :introlisp.practice/test :run-suites))
  )
#|
(defmethod perform ((op test-op)
	(c (eql (find-system '#:introlisp.practice/test))))
	;(funcall (intern "RUN!" :fiveam) (intern "SUITE" :introlisp.practice/test))
	(symbol-call :introlisp.practice/test :run-suites)
	)
|#

(defsystem :introlisp.practice/test-image
  :description "Test system image for introlisp.practice"
  :author "thebridge0491"
  
  :depends-on (:introlisp.practice/test)
  :build-operation program-op
  :build-pathname "build/ts_main"
  :entry-point "introlisp.practice/test:run-suites"
  :perform (test-op :after (op c) (run-program "build/ts_main" :output *standard-output*))
  )

