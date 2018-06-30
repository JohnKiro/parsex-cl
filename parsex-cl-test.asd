#|
  This file is a part of parsex-cl project.
  Copyright (c) 2018 John Badie (johnkirollos@gmail.com)
|#

(in-package :cl-user)
(defpackage parsex-cl-test-asd
  (:use :cl :asdf))
(in-package :parsex-cl-test-asd)

(defsystem parsex-cl-test
  :author "John Badie"
  :license ""
  :depends-on (:parsex-cl
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "parsex-cl"))))
  :description "Test system for parsex-cl"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
