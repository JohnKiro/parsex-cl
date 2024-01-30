(in-package :parsex-cl.test)
 
(def-suite :parsex-cl.test-suite
  :description "Root test suite - see https://lispcookbook.github.io/cl-cookbook/testing.html")

(defun test-it ()
  "Run all tests in the suite (and child suites)."
  (run! :parsex-cl.test-suite))
