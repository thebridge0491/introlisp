":"; exec sbcl --script $0 $@
":"; exit
;#!/usr/bin/env sbcl --script

; Common Lisp - comments style conventions
; title       ;;;; Short title for block of code
; intro       ;;;  Description before block of code
; state       ;;   State of program or following code
; explanation ;    Regarding line on which it appears

; single-line comment
#| multi-line comment
    --- package manager - init/configure package ---
    (asdf:load-systems :cl-project)
    (cl-project:make-project "./" :name "pkg" :author "imcomputer"
      :license "..." :depends-on '(...)' :email "..." :description "...")
    
    --- generate docs ---
    (asdf:load-systems {:codex | :documentation-template} :pkg)
    ;(codex:document :pkg)
    (documentation-template:create-template :pkg :subtitle "Title"
        :target "docpath/dir/file.html")
    
    --- [native|byte]code - compile ; then run app ---
    ;[sbcl | clisp] [--load | -i] main.lisp [--eval | -x] '(pkg:save-image)'
    ;buildapp --load main.lisp --entry pkg:main --output main
    [sbcl | clisp] [--load | -i] main.lisp [--eval | -x] '(asdf:make :pkg/run-image)'
    ./main arg1 argN
    
    --- start REPL w/ loaded script ; then run app ---
    ; [sbcl | clisp] [--load | -i] main.lisp [--eval | -x] '(pkg:run-main)' -- arg1 argN
    ;[sbcl | clisp] [--load | -i] main.lisp
    ;  --eval '(asdf:test-system :pkg/run)' -- arg1 argN
    [sbcl | clisp] [--load | -i] main.lisp [--eval | -x] '(main (list arg1 argN))'
    * > (main '("arg1" "argN"))
    
    --- help/info tools in REPL ---
    (apropos 'PATTERN [:PKG]) (describe 'SYMBOL) *features*
    (documentation 'NAME 'DOC-TYPE)
    ; --- DOC-TYPE(s): 'T|'TYPE|'FUNCTION|'VARIABLE|'STRUCTURE|'SETF
|#

;(load "~/quicklisp/setup.lisp")
;(ql:register-local-projects)

#|
;(require :asdf)
(eval-when (:compile-toplevel :load-toplevel :execute)
	(require "asdf"))
|#

(asdf:load-systems :log4cl :unix-options :getopt :cli-parser 
    :py-configparser :cl-ppcre :cl-json :cl-yaml :introlisp.util 
    :introlisp.practice :introlisp.intro/src/lib 
    :introlisp.intro/src/person)

(defpackage :introlisp.intro/src/main
  (:documentation "Main app")
  (:use :cl :log4cl :unix-options :py-configparser :cl-ppcre :cl-json
    :cl-yaml)
  (:nicknames :introlisp.intro)
  (:export :main :run-main :save-image)
  )
(in-package :introlisp.intro/src/main)

(rename-package :introlisp.util :introlisp.util '(:util))
(rename-package :introlisp.practice.classic :introlisp.practice.classic
	'(:classic))
(rename-package :introlisp.practice.sequenceops :introlisp.practice.sequenceops
	'(:sequenceops :seqops))
(rename-package :introlisp.intro/src/lib :introlisp.intro/src/lib '(:lib))
(rename-package :introlisp.intro/src/person :introlisp.intro/src/person '(:person))
(rename-package :introlisp.practice.classic.puzzles :introlisp.practice.classic.puzzles
	'(:puzzles))

(defstruct (user 
        (:constructor make-user)
        (:constructor create-user (name num timein)) 
        (:conc-name user-)
        (:predicate user-p))
    "User structure"
    
    (name "NoName" :type string :read-only nil)
    (num 0 :type fixnum :read-only nil)
    (timein (encode-universal-time 0 0 0 1 1 1970) :type fixnum 
        :read-only nil))

(defun run-intro (rsrc-dir name num is-expt2)
	"Run intro"
	(let* ((time-in (get-internal-real-time)) (pat "(?i:^quit$)") (ch #\Nul)
			(greet-path (merge-pathnames "greet.txt" rsrc-dir))
            (delay-secs (ceiling (/ 25 10)))
			(user1 (make-user :name name :num num :timein (get-universal-time)))
			(person1 (make-instance 'person:<person> :name "I. M. Computer" 
                :age 32))
            (num-vec (vector #b1011 #o13 #xb 11)) ; (bin oct hex dec)
            (num-val 0) (time-dur 0.0) (lst '(2 1 0 4 3))
            (rnd-state (make-random-state t))
            (num-disks 4) (num-queens 8)
            (res-queens (puzzles:nqueens num-queens))
            (queens-ndx (random (length res-queens) rnd-state))
            (queens-answer (nth queens-ndx res-queens))
			)
		(setf num-val (reduce (lambda (a e) (+ a e)) num-vec 
            :initial-value 0))
        ;(if (not (= num-val
        ;    (* (length num-vec) (aref num-vec 0))))
        ;    (error "Assert fails: len * 1st elem == num-val->~a" num-val))
		(handler-case
            (assert (= num-val (* (length num-vec) (aref num-vec 0))))
            (error (exc) (format t "~a~%" exc)))
        
        (setf ch (lib:delay-char (lambda () (sleep delay-secs))))
        
        (if (= 0 (user-num user1))
            (setf (user-num user1) (+ (random 18 rnd-state) 2)))
        
        (format t "~a match ~a to ~a~%" (if (cl-ppcre:scan pat name) 
			"Good" "Does not") (user-name user1) pat)
		(format t "~a~%" (util:date-to-string (user-timein user1)))
		
        #|
        (catch 'prac
            (handler-bind (
                (warning #'(lambda (exc) 
                    (format t "Warning Condition: ~a~%" exc) (throw 'prac t)))
                (file-error #'(lambda (exc) 
                    (format t "File Error Condition: ~a~%" exc) 
                        (throw 'prac t)))
                (error #'(lambda (exc) 
                    (format t "Error Condition: ~a~%" exc) (throw 'prac t))))
                
                (format t "~a~%" (lib:greeting greet-path 
                    (user-name user1)))))
        |#
		(handler-case
            (format t "~a~%" (lib:greeting greet-path 
                (user-name user1)))
            
            (warning (exc) (format t "Warning Condition: ~a~%" exc))
            (file-error (exc) (format t "File Error Condition: ~a~%" exc))
            (error (exc) (format t "Error Condition: ~a~%" exc)))
		
		(setf time-dur (/ (- (get-internal-real-time) time-in) 
			internal-time-units-per-second))
		(format t "(program ~a) Took ~,2f seconds.~%" (uiop:argv0) time-dur)
		(format t "~a~%" (make-string 40 :initial-element #\#))
		
		(if is-expt2
			(progn
				(format t "expt 2.0 ~,1f: ~,1f~%" (user-num user1)
					(classic:expt-i 2.0 (user-num user1)))
				(format t "reverse ~a: ~a~%" lst (seqops:reverse-i lst))
				(format t "sort ~a #'<: ~a~%" (append '(9 9 9 9) lst)
					(sort (append '(9 9 9 9) lst) #'<))
				)
			(progn
				(format t "fact ~a: ~a~%" (user-num user1)
					(classic:fact-i (user-num user1)))
				(format t "index (= 3 e) ~a: ~a~%" lst (seqops:index-i 
					(lambda (e) (equal e 3)) lst))
				(format t "append ~a ~a: ~a~%" '(9 9 9 9) lst
					(append '(9 9 9 9) lst))
				))
		(format t "~a~%" (make-string 40 :initial-element #\#))
		
		(format t "pascaltri ~a: ~a~%" 5 (classic:pascaltri-add 5))
		(format t "~a~%" (util:mkstring-nested (string #\newline) " " 
			"" (classic:pascaltri-add 5)))
		(format t "~a~%" (make-string 40 :initial-element #\#))
		
		(multiple-value-bind (res stats moves)
			(puzzles:hanoi-moves 1 2 3 num-disks)
			(format t "hanoi-moves (result: ~a)~%(stats: ~a)" res stats))
		(format t "~a~%" (util:mkstring-nested "" " " 
			"" (multiple-value-bind (res stats moves)
				(puzzles:hanoi-moves 1 2 3 num-disks) (list moves))))
		(format t "~a~%" (make-string 40 :initial-element #\#))
		
		(format t "nqueens-grid ~a (idx: ~a): ~a" num-queens queens-ndx
			queens-answer)
		(format t "~a~%" (util:mkstring-nested (string #\newline) "-" ""
			 (util:arr2d-to-nlsts (puzzles:nqueens-grid num-queens queens-answer))))
		(format t "~a~%" (make-string 40 :initial-element #\#))
		
		(format t "~a~%"
			#+ccl (ccl::class-slots (find-class 'person:<person>))
			#+sbcl (sb-mop:class-slots (find-class 'person:<person>))
			#+clisp (clos:class-slots (find-class 'person:<person>))
			)
		;(format t "person-age person1: ~a~%" (slot-value person1 'person:age))
		;(setf (slot-value person1 'person:age) 33)
		(format t "person-age person1: ~a~%" (person:person-age person1))
		(setf (person:person-age person1) 33)
		(format t "setf (person-age person1) 33:~%")
		(format t "~a~%" (person:peek-person person1))
		(format t "type-of person1: ~a~%class-of person1: ~a~%" 
			(type-of person1) (class-of person1))
		(format t "~a~%" (make-string 40 :initial-element #\#))
		
	))

(defun parse-cmdopts-unix-options (argv)
	"Parse cmdopts using unix-options"
    (unix-options:with-cli-options (argv t) 
        ((2expt "Expt 2 n vice Fact n")
            (verbose (verbose "VERBOSITY" "Verbosity level"))
            (user (user "USER" "User name"))
            (num (num "NUM" "Number to factorialize")))
        
        (values (or verbose 0) (or user "World") (or num "0") 2expt)))

(defun show-help (progname)
    "Usage help"
    (format t "Usage: ~a [-h][-2][-v VERBOSITY][-u USER][-n NUM]~%" 
        progname))

(defparameter *opts* '(("help" :none) ("verbose" :required)
	("user" :required) ("num" :required) ("is-expt2" :none))
	"Options for getopt")

(defun parse-cmdopts-getopt (argv)
	"Parse cmdopts using getopt"
    (let ((verbose 0) (user "World") (num "0") (is-expt2 nil))
        (multiple-value-bind (args opts)
            (getopt:getopt argv *opts*)
            
            ;(progn
            ;   (mapcar (lambda (opt)
            ;       (case (car opt)
            ;           ("help" (show-help (uiop:argv0)) (uiop:quit))
            ;           (t      ; {t|otherwise}
            ;               (format t "??? No matching options.~%"))))
            (progn
                (mapcar (lambda (opt) 
                    (cond
                        ((equal (car opt) "help")
                            (show-help (uiop:argv0)) (uiop:quit))
                        ((equal (car opt) "is-expt2") (setf is-expt2 (cdr opt)))
                        ((equal (car opt) "verbose") (setf verbose (cdr opt)))
                        ((equal (car opt) "user") (setf user (cdr opt)))
                        ((equal (car opt) "num") (setf num (cdr opt)))
                        (t (format t "??? No matching options.~%"))))
                    opts)
                nil)
            
            (values verbose user num is-expt2))))

(defparameter *option-conf* (list 
	(make-instance 'cli-parser:cli-option :abbr "h" :full "help"
		:requires-arguments nil :description "Usage help"
		:example "--help")
	(make-instance 'cli-parser:cli-option :abbr "v" :full "verbose"
		:requires-arguments t :description "Verbosity level"
		:example "--verbose=3")
	(make-instance 'cli-parser:cli-option :abbr "u" :full "user"
		:requires-arguments t :description "User name"
		:example "--user=Name1")
	(make-instance 'cli-parser:cli-option :abbr "n" :full "num"
		:requires-arguments t :description "Number to factorialize"
		:example "--num=5")
	(make-instance 'cli-parser:cli-option :abbr "2" :full "is-expt2"
		:requires-arguments nil :description "Expt 2 n vice Fact n"
		:example "--is-expt2")
	)
	"Options for cli-parser")

(defun parse-cmdopts-cli-parser (argv)
	"Parse cmdopts using cli-parser"
    (let ((opt-hash (cli-parser:cli-parse argv *option-conf*)))
        
        (if (not (gethash "help" opt-hash t))
            (progn
                (show-help (uiop:argv0))
                (uiop:quit)))
            
        (values (car (gethash "verbose" opt-hash '(0)))
            (car (gethash "user" opt-hash '("World"))) 
            (car (gethash "num" opt-hash '("0")))
            (not (gethash "is-expt2" opt-hash t))
            )))

(defun parse-cmdopts (argv)
    "Parse command-line options"
    (log:info '(root) "parse-cmdopts()")
    
    (cond
        (nil (parse-cmdopts-getopt argv))
        (nil (parse-cmdopts-cli-parser argv))
        (t (parse-cmdopts-unix-options argv))
        ))

(defun main (argv)
	"Entry point"
	(let* ((sys-dir (asdf:system-source-directory :introlisp.intro))
			(rsrc-dir (pathname (or (uiop:getenv "RSRC_PATH")
				(merge-pathnames "resources/" sys-dir))))
			(ini-cfg (py-configparser:make-config))
			(alst-cfg (cl-json:decode-json-from-source
				(merge-pathnames "prac.json" rsrc-dir)))
			(cl-json:*json-symbols-package* nil)
			(obj-cfg (cl-json:with-decoder-simple-clos-semantics
				(cl-json:decode-json-from-source (merge-pathnames 
					"prac_htbl.json" rsrc-dir))))
			(htbl (make-hash-table :test 'equal))
			(yaml-htbl (yaml:parse (merge-pathnames "prac.yaml" rsrc-dir)))
			)
		(log:config :properties (merge-pathnames "log4cl.conf"
			rsrc-dir))
		
		(py-configparser:read-files ini-cfg (list 
			(merge-pathnames "prac.conf" rsrc-dir)))
		
		(maphash (lambda (k v) (setf (gethash (princ-to-string k) htbl) v)) 
			obj-cfg)
		(maphash (lambda (k v) (setf (gethash (princ-to-string k)
			(gethash "USER-1" htbl)) v)) (gethash "USER-1" htbl))
		
		(let* ((tup-vec (vector 
			(cons (slot-value ini-cfg 'py-configparser:sections) 
				(cons (py-configparser:get-option ini-cfg "default" "domain")
					(py-configparser:get-option ini-cfg "user1" "name")))
			(cons alst-cfg (cons (cdr (assoc :domain alst-cfg))
				(cdr (assoc :name (cdr (assoc :user-1 alst-cfg))))))
			(cons (util:hashtbl->alist obj-cfg) (cons (gethash "DOMAIN" htbl)
				(gethash "NAME" (gethash "USER-1" htbl))))
			(cons (util:hashtbl->alist yaml-htbl) (cons (gethash "domain" yaml-htbl)
				(gethash "name" (gethash "user1" yaml-htbl))))
			)))
			(map nil (lambda (tup3) (progn 
				(format t "config: ~a~%" (car tup3))
				(format t "domain: ~a~%" (cadr tup3))
				(format t "user1Name: ~a~%" (cddr tup3))
				)) tup-vec)
			)
		
		(multiple-value-bind (verbose user num is-expt2)
			(parse-cmdopts argv)
			
			(handler-case 
				(run-intro rsrc-dir user (or (parse-integer num :junk-allowed t) 5) is-expt2)
				(type-error (condition) (uiop:quit))))
		
    )) ;(uiop:quit))

;(if (member (pathname-name *load-truename*) (uiop:command-line-arguments)
;        :test #'(lambda (x y) (search x y :test #'equalp)))
;    (main (uiop:command-line-arguments)))
;(main (uiop:command-line-arguments))

(defun run-main ()
	(main (uiop:command-line-arguments))
	(uiop:quit)
	)

(defun save-image ()
	#+sbcl (sb-ext:save-lisp-and-die "build/main" :executable t
		:toplevel 'introlisp.intro:run-main)
	#+clisp (ext:saveinitmem "build/main" :executable t
		:init-function 'introlisp.intro:run-main)
	#+ccl (ccl:save-application "build/main" :prepend-kernel t
		:toplevel-function 'introlisp.intro:run-main)
	)
