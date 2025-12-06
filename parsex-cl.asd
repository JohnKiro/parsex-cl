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
                             (:file "char-util")
                             (:file "class-util")))
               (:module "regex"
                :components ((:file "regex-input")
                             (:file "regex-element")
                             (:file "regex-nfa")
                             (:file "regex-nfa-state")
                             (:file "regex-nfa-transition")
                             (:file "regex-dfa")
                             (:file "regex-match")
                             (:file "regex-sexp")
                             (:file "regex-fsm")
                             ))
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
          seq)))
  :in-order-to ((test-op (test-op "parsex-cl/test"))))

(defsystem "parsex-cl/test"
  :depends-on ("fiveam" "parsex-cl")
  :pathname "test"
  :components ((:file "packages")
               (:file "test")
               (:file "regex-test")
               (:file "regex-input-test")
               (:file "chars-test")
               (:file "nfa-element-test"))
  :perform (test-op (o c)
                    (uiop:symbol-call :fiveam '#:runnn! :parsex-cl.test-suite)))
