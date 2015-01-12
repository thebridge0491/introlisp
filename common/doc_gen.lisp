(load #p"~/quicklisp/setup.lisp")

(asdf:load-system :introlisp.util)


(asdf:load-system :documentation-template)
(documentation-template:create-template :introlisp.util 
	:subtitle (asdf:system-description (asdf:find-system :introlisp.util))
    :target #p"build/html/file_doctmplt.html")

#|
(asdf:load-system :codex)
(codex:document :introlisp.util)
|#
