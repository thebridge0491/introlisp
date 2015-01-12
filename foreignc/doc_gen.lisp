(load #p"~/quicklisp/setup.lisp")

(asdf:load-system :introlisp.foreignc)


(asdf:load-system :documentation-template)
(documentation-template:create-template :introlisp.foreignc 
	:subtitle (asdf:system-description (asdf:find-system :introlisp.foreignc))
    :target #p"build/html/file_doctmplt.html")

#|
(asdf:load-system :codex)
(codex:document :introlisp.foreignc)
|#
