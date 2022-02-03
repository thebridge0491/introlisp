Introlisp.Practice
===========================================
.. .rst to .html: rst2html5 foo.rst > foo.html
..                pandoc -s -f rst -t html5 -o foo.html foo.rst

Practice sub-package for Common Lisp Intro examples project.

Installation
------------
source code tarball download:
    
        # [aria2c --check-certificate=false | wget --no-check-certificate | curl -kOL]
        
        FETCHCMD='aria2c --check-certificate=false'
        
        $FETCHCMD https://bitbucket.org/thebridge0491/introlisp/[get | archive]/master.zip

version control repository clone:
        
        git clone https://bitbucket.org/thebridge0491/introlisp.git

note quicklisp required for install to $HOME/quicklisp/local-projects

build example with rake:

        [sudo] rake install

        [COMPILER=sbcl] rake all [test]

build example with ninja:

        [sudo] ninja install

        [COMPILER=sbcl] ninja [test]

build example with make:

        [sudo] make install

        [COMPILER=sbcl] make all [test]

Usage
-----
	rlwrap [sbcl | ccl]
	
	 > (asdf:load-systems :introlisp.practice)
	 
	 > (rename-package :introlisp.practice.classic :introlisp.practice.classic '(:classic))

	 > (classic:fact-i 5)

Author/Copyright
----------------
Copyright (c) 2015 by thebridge0491 <thebridge0491-codelab@yahoo.com>

License
-------
Licensed under the Apache-2.0 License. See LICENSE for details.
