(in-package :parsex-cl.symbol-util)

;; TODO: may use Alexandria's ENSURE-SYMBOL
(defun reintern (symbol package)
  "Intern a symbol SYMBOL into a package PACKAGE, and return the newly interned symbol.
The difference from the standard INTERN function is that it receives a symbol, not a symbol name. So
practically it copies a symbol from a package into another one."
  (intern (symbol-name symbol) (find-package package)))

(defmacro with-preserve-symbol-case (&body body)
  "Configures the reader to preserve symbol case for all symbols that will be read within the BODY.
Note that it works on the read operations that follow the expansion, not during it. So for example,
it will not work in the following example:
(sexp::with-preserve-symbol-case
    (print '(to be or not to be)))
This is since the body is read during the expansion, including the list passed to PRINT. On the
other hand, it will work for symbols that are read within calls to READ within the body."
  `(let ((*readtable* (copy-readtable *readtable*)))
     (setf (readtable-case *readtable*) :preserve)
     ,@body))
