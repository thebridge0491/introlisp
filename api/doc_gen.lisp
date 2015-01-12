(load #p"~/quicklisp/setup.lisp")

(asdf:load-system :introlisp.practice)


(asdf:load-system :documentation-template)
(documentation-template:create-template :introlisp.practice 
	:subtitle (asdf:system-description (asdf:find-system :introlisp.practice))
    :target #p"build/html/file_doctmplt.html")

#|
(asdf:load-system :codex)
(codex:document :introlisp.practice)
|#
