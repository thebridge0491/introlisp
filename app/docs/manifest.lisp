(:docstring-markup-format :scriba
  :systems (:introlisp.intro)
  :documents ((:title (asdf:system-description (asdf:find-system :introlisp.intro))
				:authors ("thebridge0491")
				:output-format (:type :multi-html
								:template :minima)
				:sources ("manual.scr"))))
