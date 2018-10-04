sbcl --noinform --disable-ldb --lose-on-corruption --non-interactive --load main.lisp --eval "(sb-ext:save-lisp-and-die \"lisp-format.exe\" :toplevel 'main :executable t)"
