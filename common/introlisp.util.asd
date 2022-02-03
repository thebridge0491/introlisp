;; -*- Lisp -*-

#|
  This file is a part of introlisp.util project.
|#
#|
  Utilities sub-package for Common Lisp Intro examples project.
|#
#-asdf3.1 (error "Introlisp.Util requires ASDF >= 3.1.2, please upgrade asdf")

(defsystem :introlisp.util
  :description "Utilities sub-package for Common Lisp Intro examples project."
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
  :depends-on (:introlisp.util/core
               ;:log4cl
              )
#|
  :components ((:module "src"
                :components ((:file "lib"))
               ))
|#
  :in-order-to ((test-op (test-op :introlisp.util/test)))
  )

(defsystem :introlisp.util/test
  :description "Test system for introlisp.util"
  :author "thebridge0491"
  
  :pathname "tests/"
  :serial t
  :depends-on (:introlisp.util
               :cl-quickcheck
               :fiveam
               )
#|
  :components ((:file "test-suite") (:file "tc-new") (:file "tp-new")
			   )
|#  
  ;:perform (test-op :after (op c) (symbol-call :fiveam :run-all-tests))
  ;:perform (test-op :after (op c) (symbol-call :fiveam :run! (find-symbol* 'suite '#:introlisp.util/test)))
  :perform (test-op :after (op c) (symbol-call :introlisp.util/test :run-suites))
  )
#|
(defmethod perform ((op test-op)
	(c (eql (find-system '#:introlisp.util/test))))
	;(funcall (intern "RUN!" :fiveam) (intern "SUITE" :introlisp.util/test))
	(symbol-call :introlisp.util/test :run-suites)
	)
|#

(defsystem :introlisp.util/test-image
  :description "Test system image for introlisp.util"
  :author "thebridge0491"
  
  :depends-on (:introlisp.util/test)
  :build-operation program-op
  :build-pathname "build/ts_main"
  :entry-point "introlisp.util/test:run-suites"
  :perform (test-op :after (op c) (run-program "build/ts_main" :output *standard-output*))
  )

