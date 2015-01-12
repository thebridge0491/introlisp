(load #p"~/quicklisp/setup.lisp")

(asdf:load-system :introlisp.intro)


(asdf:load-system :documentation-template)
(documentation-template:create-template :introlisp.intro 
	:subtitle (asdf:system-description (asdf:find-system :introlisp.intro))
    :target #p"build/html/file_doctmplt.html")

#|
(asdf:load-system :codex)
(codex:document :introlisp.intro)
|#
