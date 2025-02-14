#|
  This file is a part of parsex-cl project.
  Copyright (c) 2018 - 2024 John Badie (johnkirollos@gmail.com)
|#

#|
  Experimental project for tokenizer, regular expressions, lexer and parser design.

  Author: John Badie (johnkirollos@gmail.com)
|#

(in-package :cl-user)
(defpackage parsex-cl-asd
  (:use :cl :asdf))
(in-package :parsex-cl-asd)

(defsystem "parsex-cl"
  :version "0.1"
  :author "John Badie"
  :license ""
  :depends-on (:iterate :alexandria)
  :pathname "src"
  :serial t
  :components ((:file "packages")
               (:module "util"
                :components ((:file "symbol-util")
                             (:file "char-util")))
               (:file "regex-input")
               (:file "regex-element")
               (:file "regex-nfa")
               (:file "regex")
               (:file "regex-sexp")
               (:file "regex-fsm")
               (:file "tokenizer-states")
               (:file "tokenizer-transitions")
               (:file "tokenizer")
               (:file "common-atom-matchers")
               (:file "common-transition-finders")
               (:file "basic-string-tokenizer")
               (:module "addons"
                :components ((:file "graphviz-export"))))
  :description "Experimental project for tokenizer, regular expressions, lexer and parser design."
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq))))

(defsystem "parsex-cl/tests"
  :depends-on ("fiveam" "parsex-cl")
  :pathname "test/"
  :components ((:file "packages")
               (:file "test")
               (:file "tokenizer-test")
               (:file "regex-test")
               (:file "chars-test")
               (:file "nfa-element-test"))
  :in-order-to ((test-op (test-op parsex-cl.test))))
