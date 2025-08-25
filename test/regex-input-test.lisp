(in-package :parsex-cl.test/regex-input.test)

(fiveam:def-suite :parsex-cl/regex-input.test-suite
  :description "Tests the RegEx input"
  :in :parsex-cl.test-suite)

(fiveam:in-suite :parsex-cl/regex-input.test-suite)

(setf fiveam:*on-failure* nil)
(setf fiveam:*on-error* :debug)

(fiveam:test with-regex-input-handler-funcall-macros-test
  "Basic test for the `with-regex-input-handler-funcall-macros` macro"
  (let ((input-regex (input:create-basic-regex-input "ABCD")))
    (input:with-regex-input-handler-funcall-macros (input:source-empty-p input:remaining-length
                                                     input:read-next-item
                                                     input:advance-reading-position) input-regex
      (fiveam:is (equal (list (input:source-empty-p)
                              (input:remaining-length)
                              (input:read-next-item)
                              (input:advance-reading-position)
                              (input:read-next-item)
                              (input:advance-reading-position)
                              (input:read-next-item))
                        '(nil 4 #\A 1 #\B 2 #\C))))))

;; TODO: may add a test for case of passing invalid operation names.
