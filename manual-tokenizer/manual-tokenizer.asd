#|
  This file is a part of parsex-cl project.
  Copyright (c) 2018 - 2024 John Badie (johnkirollos@gmail.com)
|#

#|
  Obsolete tokenizer that is based on manually constructing the state machine (DFA). Superseded by
  regex-based tokenizer (to be created).

  Author: John Badie (johnkirollos@gmail.com)
|#

(in-package :cl-user)
(defpackage :parsex-cl/manual-tokenizer-asd
  (:use :cl :asdf))
(in-package :parsex-cl/manual-tokenizer-asd)

(defsystem "parsex-cl/manual-tokenizer"
  :version "0.1"
  :author "John Badie"
  :license ""
  :depends-on (:iterate :alexandria)
  :pathname "src"
  :serial t
  :components ((:file "packages")
               (:module "tokenizer"
                :components ((:file "tokenizer-states")
                             (:file "tokenizer-transitions")
                             (:file "tokenizer")
                             (:file "basic-string-tokenizer")
                             (:module "commons"
                              :components ((:file "common-atom-matchers")
                                           (:file "common-transition-finders"))))))
  :description "Obsolete tokenizer with manual DFA construction."
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
  :in-order-to ((test-op (test-op "parsex-cl/manual-tokenizer/test"))))

(defsystem "parsex-cl/manual-tokenizer/test"
  :depends-on ("fiveam" "parsex-cl/manual-tokenizer")
  :pathname "test"
  :components ((:file "packages")
               (:file "test")
               (:file "tokenizer-test"))
  :perform (test-op (o c)
                    (uiop:symbol-call :fiveam '#:runnn! :parsex-cl/manual-tokenizer/test-suite)))
