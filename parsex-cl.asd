#|
  This file is a part of parsex-cl project.
  Copyright (c) 2018 John Badie (johnkirollos@gmail.com)
|#

#|
  Experimental project for regular expressions, lexer and parser design.

  Author: John Badie (johnkirollos@gmail.com)
|#

(in-package :cl-user)
(defpackage parsex-cl-asd
  (:use :cl :asdf))
(in-package :parsex-cl-asd)

(defsystem parsex-cl
  :version "0.1"
  :author "John Badie"
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "parsex-cl"))))
  :description "Experimental project for regular expressions, lexer and parser design."
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
  :in-order-to ((test-op (test-op parsex-cl-test))))
