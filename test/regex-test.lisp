(in-package :parsex-cl.regex.test)

(def-suite :parsex-cl.regex.test-suite
  :description "Tests the RegEx"
  :in :parsex-cl.test-suite)

(in-suite :parsex-cl.regex.test-suite)

(setf fiveam:*on-failure* nil)
(setf fiveam:*on-error* nil)

(test char-splitting-test
  (let* ((regex '(:or
                  (:char-range #\a #\d)
                  (:char-range #\c #\f)
                  "lmn"
                  #\x
                  #\y
                  #\z))
         (root-state (parsex-cl.regex::parse-and-produce-nfa regex))
         (root-closure (parsex-cl.regex::prepare-nfa-state-closure nil root-state))
         (splitting-points (parsex-cl.regex::collect-char-range-splitting-points root-closure)))
    (format t "~&Splitting chars for root state's closure: ~a~&" splitting-points)
    (is (equal splitting-points '(#\` #\b #\d #\f #\k #\l #\w #\x #\y #\z)))))

(defun run-regex-matching-test (regex input-string
                                &key
                                  expected-matching-status
                                  (expected-accumulator-value input-string)
                                  (generate-nfa-dotgraphviz t)
                                  (generate-dfa-dotgraphviz t))
  "Reusable function that tests matching of specified input (INPUT-STRING) against a specified
regex (REGEX)."
  (let* ((input-source (make-instance 'basic-regex-input  :initial-input-text input-string))
         (nfa (parsex-cl.regex:parse-and-produce-nfa regex))
         (dfa (parsex-cl.regex:produce-dfa nfa))
         (result (match-regex input-source dfa))
         (matching-status (regex-matching-result-status result))
         (updated-acc (retrieve-last-accumulated-value input-source)))
    (format t "~%Matching the text ~s against the regex ~a..~%" input-string regex)
    (format t "~%Updated accumulator is ~a~%" updated-acc)
    (when generate-nfa-dotgraphviz
      (format t "~%Graphviz for NFA:~%~a~%" (parsex-cl.graphviz-util:fsm-to-graphvizdot nfa)))
    (when generate-dfa-dotgraphviz
      (format t "~%Graphviz for DFA:~%~a~%" (parsex-cl.graphviz-util:fsm-to-graphvizdot dfa)))
    (is (equal matching-status expected-matching-status))
    (is (equal updated-acc expected-accumulator-value))))

(defmacro define-regex-matching-test (test-name
                                      &key
                                        (description nil)
                                        regex
                                        input-text
                                        (expected-matching-status :regex-not-matched)
                                        (expected-accumulator-value input-text))
  (let ((thefuncall `(run-regex-matching-test
                      ',regex
                      ,input-text
                      :expected-matching-status ,expected-matching-status
                      :expected-accumulator-value ,expected-accumulator-value)))
    (if description
        `(test ,test-name ,description ,thefuncall)
        `(test ,test-name ,thefuncall))))


;;; TODO: need to put more tests in a loop (matching token by token)
(define-regex-matching-test basic1-regex-matching-test
  :description "Tests +, OR, char range, char range splitting."
  :regex (:+ (:or
              (:seq #\a #\n)
              (:seq #\a #\m)
              (:seq #\a #\t)
              (:seq #\a #\s)
              (:char-range #\w #\z)))
  :input-text "anamatasxzwy"
  :expected-matching-status :regex-matched)

(define-regex-matching-test basic2-regex-matching-test
  :description "Also tests char splitting. May remove it later (redundant)."
  :regex (:+ (:or
              (:char-range #\a #\d)
              (:char-range #\b #\e)))
  :input-text "abcacdaecccaabeadde"
  :expected-matching-status :regex-matched)

(define-regex-matching-test basic3-regex-matching-test
  :description "Tests :any-char"
  :regex (:+ (:or
              (:seq #\a #\n)
              (:seq :any-char #\M)
              (:seq :any-char #\P)))
  :input-text "anxMyPanzMvPan"
  :expected-matching-status :regex-matched)

(define-regex-matching-test basic31-regex-matching-test
  :description "Also tests :any-char. This one is simpler to inspect visually."
  :regex (:or
          (:seq #\a #\n)
          (:seq :any-char #\M)
          (:seq :any-char #\P))
  :input-text "xManxMyPanzMvPan"
  :expected-matching-status :regex-matched
  :expected-accumulator-value "xM")

;; (a|b)*abb.
(define-regex-matching-test basic4-regex-matching-test
  :description "Tests SEQ, OR, Kleene Closure, string."
  :regex (:seq (:* (:or #\a #\b)) "abb")
  :input-text "abbbababbababbabbbbbabb"
  :expected-matching-status :regex-matched)


(define-regex-matching-test basic5-regex-matching-test
  :description "Tests stopping at candidate matching point."
  :regex (:seq (:* #\x) #\y)
  :input-text "xxyz"
  :expected-matching-status :regex-matched
  :expected-accumulator-value "xxy")

(define-regex-matching-test basic6-regex-matching-test
  :description "Tests backtracking to last candidate match (upon no match)."
  :regex (:seq (:* (:seq #\X #\Y)))
  :input-text "XYXYXw"
  :expected-matching-status :regex-matched
  :expected-accumulator-value "XYXY")

(define-regex-matching-test basic7-regex-matching-test
  :description "Tests backtracking to last candidate match (upon input exhaustion)."
  :regex (:seq (:* (:seq #\X #\Y)))
  :input-text "XYXYX"
  :expected-matching-status :regex-matched
  :expected-accumulator-value "XYXY")

(define-regex-matching-test basic8-regex-matching-test
  :description "Tests backtracking to beginning (match empty string)."
  :regex (:seq (:* (:seq #\X #\Y)))
  :input-text "X"
  ;; note that * is zero-or-more, that's why we backtrack
  :expected-matching-status :regex-matched
  :expected-accumulator-value "")

(define-regex-matching-test basic9-regex-matching-test
  :description "Tests no backtracking (no match)."
  :regex (:seq (:+ (:seq #\X #\Y)))
  :input-text "X"
  :expected-matching-status :regex-not-matched
  :expected-accumulator-value "X")

(test detailed-regex-matching-test
  (loop with inputs = '("The problem was resolved."
                        "A problem was solved."
                        "A problem is resolved."
                        "The problem is solved?")
        for input in inputs
        for input-source = (make-instance 'basic-regex-input :initial-input-text input)
        do (is (equal input
                      (reduce #'(lambda (x y) (concatenate 'string x y))
                              (loop for regex in (list '(:+ (:or
                                                             (:seq #\A)
                                                             "The"))
                                                       #\space
                                                       "problem"
                                                       #\space
                                                       '(:or
                                                         "is"
                                                         "was")
                                                       #\space
                                                       '(:seq (:? (:seq #\r #\e)) "solved")
                                                       '(:or #\. #\?))
                                    for dfa = (parse-and-produce-dfa regex)
                                    for result = (match-regex input-source dfa)
                                    for matching-status = (regex-matching-result-status result)
                                    for updated-acc = (retrieve-last-accumulated-value input-source)
                                    do (format t "~%Updated accumulator is ~a~%" updated-acc)
                                       (is (eq matching-status :regex-matched))
                                    collect updated-acc))))))


(test parse-and-produce-nfa-test-LATER
  (is (= 10 10)))
