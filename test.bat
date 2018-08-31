rem \ccl\wx86cl -Q -b -l main.lisp -- %*
rem if errorlevel 1 goto :eof
rem type \t\1
rem echo %errorlevel%

sbcl --script main.lisp %*
