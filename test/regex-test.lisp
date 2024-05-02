(in-package :parsex-cl.regex.test)

(def-suite :parsex-cl.regex.test-suite
  :description "Tests the RegEx"
  :in :parsex-cl.test-suite)

(in-suite :parsex-cl.regex.test-suite)

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
  (let* ((input-handler (create-basic-input input-string))
         (accumulator-handler (create-basic-accumulator))
         (accumulator-retrieval-fn (nth-value 0 (funcall accumulator-handler)))
         (nfa (parsex-cl.regex:parse-and-produce-nfa regex))
         (dfa (parsex-cl.regex:produce-dfa nfa))
         (result (match-regex input-handler dfa :accumulator-interface-fn accumulator-handler))
         (matching-status (regex-matching-result-status result))
         (updated-acc (funcall accumulator-retrieval-fn)))
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

(test detailed-regex-matching-test
  (loop with inputs = '("The problem was resolved."
                        "A problem was solved."
                        "A problem is resolved."
                        "The problem is solved?")
        for input in inputs
        for input-handler = (create-basic-input input)
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
                                    for acc-handler = (create-basic-accumulator)
                                    for acc-retrieval-fn = (nth-value 0 (funcall acc-handler))
                                    for dfa = (parse-and-produce-dfa regex)
                                    for result = (match-regex input-handler dfa
                                                              :accumulator-interface-fn acc-handler)
                                    for matching-status = (regex-matching-result-status result)
                                    for updated-acc = (funcall acc-retrieval-fn)
                                    do (format t "~%Updated accumulator is ~a~%" updated-acc)
                                       (is (eq matching-status :regex-matched))
                                    collect updated-acc))))))


(test parse-and-produce-nfa-test-LATER
  (let ((root-state (parsex-cl.regex::parse-and-produce-nfa sample-regex)))
    (is (eq token :TOKEN-ENDING-WITH-DOLLAR-SIGN))
    (is (equal accumulator '(#\A #\A #\A #\$)))
    (is (= input-reading-index 4))))
