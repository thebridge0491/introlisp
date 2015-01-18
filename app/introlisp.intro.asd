;; -*- Lisp -*-

#|
  This file is a part of introlisp.intro project.
|#
#|
  Main app sub-package for Common Lisp Intro examples project.
|#
#-asdf3.1 (error "Introlisp.Intro requires ASDF >= 3.1.2, please upgrade asdf")

(defsystem :introlisp.intro
  :description "Main app sub-package for Common Lisp Intro examples project."
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
  :depends-on (:introlisp.intro/core
               :log4cl
               :unix-options
               :getopt
               :cli-parser
               :cl-ppcre
               :py-configparser
               :cl-json
               :cl-yaml
               :introlisp.util
               :introlisp.practice
              )
#|
  :components ((:module "src"
                :components ((:file "lib") (:file "person") (:file "main"))
               ))
|#
  :in-order-to ((test-op (test-op :introlisp.intro/test)))
  )

(defsystem :introlisp.intro/test
  :description "Test system for introlisp.intro"
  :author "thebridge0491"
  
  :pathname "tests/"
  :serial t
  :depends-on (:introlisp.intro
               :introlisp.util
               :cl-quickcheck
               :fiveam
               :queues.simple-queue
               :queues.priority-queue
               )
#|
  :components ((:file "test-suite") (:file "tc-new") (:file "tp-new")
			   )
|#  
  ;:perform (test-op :after (op c) (symbol-call :fiveam :run-all-tests))
  ;:perform (test-op :after (op c) (symbol-call :fiveam :run! (find-symbol* 'suite '#:introlisp.intro/test)))
  :perform (test-op :after (op c) (symbol-call :introlisp.intro/test :run-suites))
  )
#|
(defmethod perform ((op test-op)
	(c (eql (find-system '#:introlisp.intro/test))))
	;(funcall (intern "RUN!" :fiveam) (intern "SUITE" :introlisp.intro/test))
	(symbol-call :introlisp.intro/test :run-suites)
	)
|#

(defsystem :introlisp.intro/test-image
  :description "Test system image for introlisp.intro"
  :author "thebridge0491"
  
  :depends-on (:introlisp.intro/test)
  :build-operation program-op
  :build-pathname "build/ts_main"
  :entry-point "introlisp.intro/test:run-suites"
  :perform (test-op :after (op c) (run-program "build/ts_main" :output *standard-output*))
  )

(defsystem :introlisp.intro/run
  :description "(App) Main system for introlisp.intro"
  :author "thebridge0491"
  
  :depends-on (:introlisp.intro)
  :perform (test-op :after (op c) (symbol-call :introlisp.intro :run-main))
  )
#|
(defmethod perform ((op test-op)
	(c (eql (find-system '#:introlisp.intro))))
	(symbol-call :introlisp.intro :run-main)
	)
|#

(defsystem :introlisp.intro/run-image
  :description "(App) Main system image for introlisp.intro"
  :author "thebridge0491"
  
  :depends-on (:introlisp.intro)
  :build-operation program-op
  :build-pathname "build/main"
  :entry-point "introlisp.intro:run-main"
  :perform (test-op :after (op c) (run-program "build/main" :output *standard-output*))
  )
