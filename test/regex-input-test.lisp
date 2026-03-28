(in-package :parsex-cl.test/regex-input.test)

(fiveam:def-suite regex-input.test-suite
  :description "Tests the RegEx input"
  :in :parsex-cl.test-suite)

(fiveam:in-suite regex-input.test-suite)

(setf fiveam:*on-failure* nil)
(setf fiveam:*on-error* :debug)

(fiveam:test basic-regex-input-test
  "Test basic regex input component: empty predicate, remaining length, advancing, reading."
  (let ((input (input:create-basic-regex-input "ABCD")))
    (fiveam:is (equal (list (input:source-empty-p input)
                            (input:remaining-length input)
                            (input:read-next-item input)
                            (input:advance-reading-position input)
                            (input:read-next-item input)
                            (input:advance-reading-position input)
                            (input:read-next-item input))
                      '(nil 4 #\A 1 #\B 2 #\C)))))
(fiveam:test basic-regex-input-test-matching-1
  "Test basic regex input component: candidate matching registration and effect on accumulation."
  (let ((input (input:create-basic-regex-input "ABCDEF")))
    (fiveam:is (char= (input:read-next-item input) #\A))
    (fiveam:is (= (input:advance-reading-position input) 1))
    (fiveam:is (char= (input:read-next-item input) #\B))
    (fiveam:is (= (input:advance-reading-position input) 2))
    (fiveam:is (char= (input:read-next-item input) #\C))
    (fiveam:is (= (input:advance-reading-position input) 3))
    (input:register-candidate-matching-point input)
    (input:notify-match-termination input)
    ;; notice that it won't affect accumulated value
    (input:advance-reading-position input)
    ;; not affected by advancing (accumulation affected only by registering a candidate matching point) 
    (fiveam:is (equal (input:retrieve-last-accumulated-value input) "ABC"))
    ;; affected only by calls to notify-match-termination
    (fiveam:is (equal (input:retrieve-last-consumed-value input) "ABC"))
    (input:advance-reading-position input)
    (input:register-candidate-matching-point input)
    (input:advance-reading-position input)
    (fiveam:is (equal (input:retrieve-last-accumulated-value input) "ABC"))
    ;; backtracks to last candidate matching point
    (input:notify-match-termination input)
    (fiveam:is (equal (input:retrieve-last-accumulated-value input) "DE"))
    (fiveam:is (equal (input:retrieve-last-consumed-value input) "DE"))))

;; exactly same as previous one, but retrieving values using slices (to test slice behavior)
(fiveam:test basic-regex-input-test-matching-2
  "Test basic regex input component: use of slices to retrieve accumulated and consumed values."
  (let ((input (input:create-basic-regex-input "ABCDEF")))
    (fiveam:is (char= (input:read-next-item input) #\A))
    (fiveam:is (= (input:advance-reading-position input) 1))
    (fiveam:is (char= (input:read-next-item input) #\B))
    (fiveam:is (= (input:advance-reading-position input) 2))
    (fiveam:is (char= (input:read-next-item input) #\C))
    (fiveam:is (= (input:advance-reading-position input) 3))
    (input:register-candidate-matching-point input)
    (input:notify-match-termination input)
    ;; notice that it won't affect accumulated value
    (input:advance-reading-position input)
    ;; not affected by advancing (accumulation affected only by registering a candidate matching point) 
    (fiveam:is (equal (input:retrieve-subrange input (input:retrieve-last-accumulated-indices input))
                      "ABC"))
    ;; affected only by calls to notify-match-termination
    (fiveam:is (equal (input:retrieve-subrange input (input:retrieve-last-consumed-indices input))
                      "ABC"))
    (input:advance-reading-position input)
    (input:register-candidate-matching-point input)
    (input:advance-reading-position input)
    (fiveam:is (equal (input:retrieve-subrange input (input:retrieve-last-accumulated-indices input))
                      "ABC"))
    (input:notify-match-termination input)
    (fiveam:is (equal (input:retrieve-subrange input (input:retrieve-last-accumulated-indices input))
                      "DE"))
    (fiveam:is (equal (input:retrieve-subrange input (input:retrieve-last-consumed-indices input))
                      "DE"))))

(fiveam:test basic-regex-input-test-matching-3
  "Test basic regex input component: consumption of just 1 char in case of no accumulation (no match)."
  (let ((input (input:create-basic-regex-input "ABCDEF")))
    (input:advance-reading-position input)
    (input:advance-reading-position input)
    (input:advance-reading-position input)
    (input:notify-match-termination input)
    (input:advance-reading-position input)
    (fiveam:is (equal (input:retrieve-last-accumulated-value input) nil))
    ;; note that no matter how many advance operations we did, only 1 char is consumption (due to
    ;; advancement on no match flag)
    ;; TODO: may enhance flag to allow option of consumption of all advancements done
    (fiveam:is (equal (input:retrieve-last-consumed-value input) "A"))
    (fiveam:is (equal (input:retrieve-subrange input (input:retrieve-last-consumed-indices input))
                      "A"))
    (input:advance-reading-position input)
    (input:register-candidate-matching-point input)
    (input:advance-reading-position input)
    (input:notify-match-termination input)
    (fiveam:is (equal (input:retrieve-last-accumulated-value input) "BC"))
    (fiveam:is (equal (input:retrieve-last-consumed-value input) "BC"))))

(fiveam:test basic-regex-input-test-matching-4
  "Test basic regex input component: skipping of 1 char in case of match of zero chars."
  (let ((input (input:create-basic-regex-input "ABCDEF")))
    (input:register-candidate-matching-point input)
    (input:notify-match-termination input)
    (input:advance-reading-position input)
    (input:advance-reading-position input)
    (input:advance-reading-position input)
    (fiveam:is (equal (input:retrieve-last-accumulated-value input) ""))
    (input:register-candidate-matching-point input)
    (input:notify-match-termination input)
    ;; note that the 'C' char is skipped due to advancement on match flag
    (fiveam:is (equal (input:retrieve-last-accumulated-value input) "BCD"))
    (fiveam:is (equal (input:retrieve-last-consumed-value input) "BCD"))))

(fiveam:test basic-regex-input-test-matching-5
  "Test basic regex input component: accumulating empty string in empty input."
  (let ((input (input:create-basic-regex-input "")))
    (input:register-candidate-matching-point input)
    (input:notify-match-termination input)
    (fiveam:is (equal (input:retrieve-last-accumulated-value input) ""))))

;; TODO: MORE TESTS!
