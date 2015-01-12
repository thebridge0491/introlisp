;; -*- Lisp -*-

#|
  This file is a part of introlisp.foreignc project.
|#
#|
  FFI sub-package for Common Lisp Intro examples project.
|#
#-asdf3.1 (error "Introlisp.Foreignc requires ASDF >= 3.1.2, please upgrade asdf")

(defsystem :introlisp.foreignc
  :description "FFI sub-package for Common Lisp Intro examples project."
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
  :depends-on (:introlisp.foreignc/core
               ;:log4cl
               :cffi
              )
#|
  :components ((:module "src"
                :components ((:file "lib"))
               ))
|#
  :in-order-to ((test-op (test-op :introlisp.foreignc/test)))
  )

(defsystem :introlisp.foreignc/test
  :description "Test system for introlisp.foreignc"
  :author "thebridge0491"
  
  :pathname "tests/"
  :serial t
  :depends-on (:introlisp.foreignc
               :introlisp.util
               :cl-quickcheck
               :fiveam
               )
#|
  :components ((:file "test-suite") (:file "tc-new") (:file "tp-new")
			   )
|#  
  ;:perform (test-op :after (op c) (symbol-call :fiveam :run-all-tests))
  ;:perform (test-op :after (op c) (symbol-call :fiveam :run! (find-symbol* 'suite '#:introlisp.foreignc/test)))
  :perform (test-op :after (op c) (symbol-call :introlisp.foreignc/test :run-suites))
  )
#|
(defmethod perform ((op test-op)
	(c (eql (find-system '#:introlisp.foreignc/test))))
	;(funcall (intern "RUN!" :fiveam) (intern "SUITE" :introlisp.foreignc/test))
	(symbol-call :introlisp.foreignc/test :run-suites)
	)
|#

(defsystem :introlisp.foreignc/test-image
  :description "Test system image for introlisp.foreignc"
  :author "thebridge0491"
  
  :depends-on (:introlisp.foreignc/test)
  :build-operation program-op
  :build-pathname "build/ts_main"
  :entry-point "introlisp.foreignc/test:run-suites"
  :perform (test-op :after (op c) (run-program "build/ts_main" :output *standard-output*))
  )

