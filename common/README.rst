Introlisp.Util
===========================================
.. .rst to .html: rst2html5 foo.rst > foo.html
..                pandoc -s -f rst -t html5 -o foo.html foo.rst

Utilites sub-package for Common Lisp Intro examples project.

Installation
------------
source code tarball download:
    
        # [aria2c --check-certificate=false | wget --no-check-certificate | curl -kOL]
        
        FETCHCMD='aria2c --check-certificate=false'
        
        $FETCHCMD https://bitbucket.org/thebridge0491/introlisp/[get | archive]/master.zip

version control repository clone:
        
        git clone https://bitbucket.org/thebridge0491/introlisp.git

note quicklisp required for install

cd <path> ; ln -sf $PWD introlisp.util ; mv introlisp.util ~/quicklisp/local-projects/

sbcl --eval "(progn (ql:register-local-projects) (uiop:quit))"

sbcl --load tests/test-suite.lisp --eval "(introlisp.util/test:main '())"

Usage
-----
	rlwrap sbcl
	
	 > (asdf:load-systems :introlisp.util)
	 
	 > (rename-package :introlisp.util :introlisp.util '(:util))

	 > (util:cartesian-prod '(0 1 2) '(10 20 30))

Author/Copyright
----------------
Copyright (c) 2015 by thebridge0491 <thebridge0491-codelab@yahoo.com>

License
-------
Licensed under the Apache-2.0 License. See LICENSE for details.
