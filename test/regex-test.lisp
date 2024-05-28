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

(defun run-regex-matching-test-loop (regex
                                     input-string
                                     expected-matching-result-details
                                     &key
                                       (generate-nfa-dotgraphviz t)
                                       (generate-dfa-dotgraphviz t)
                                       (report-upcoming-char t)
                                       (report-remaining-input-length t))
  "Reusable function that runs a loop of regex matching operations for the INPUT-STRING against a
single REGEX, and tests the result of each matching operation. The indication for the loop to stop
is when input is empty. The EXPECTED-MATCHING-RESULT-DETAILS is expected as a list having each
element in the form (matching-status accumulator-value consumed-value)."
  (let* ((input-source (make-instance 'basic-regex-input :initial-input-text input-string))
         (nfa (parsex-cl.regex:parse-and-produce-nfa regex))
         (dfa (parsex-cl.regex:produce-dfa nfa)))
    (when generate-nfa-dotgraphviz
      (format t "~%Graphviz for NFA:~%~a~%" (parsex-cl.graphviz-util:fsm-to-graphvizdot nfa)))
    (when generate-dfa-dotgraphviz
      (format t "~%Graphviz for DFA:~%~a~%" (parsex-cl.graphviz-util:fsm-to-graphvizdot dfa)))
    (loop for result = (match-regex input-source dfa)
          for matching-status = (regex-matching-result-status result)
          for updated-acc = (retrieve-last-accumulated-value input-source)
          for consumed = (retrieve-last-consumed-value input-source)
          for (exp-matching-status exp-acc exp-consumed) in expected-matching-result-details
          do (format t "~%Input empty? (~a)~%" (source-empty-p input-source))
          do (is (equal matching-status exp-matching-status))
          do (is (equal updated-acc exp-acc))
          do (is (equal consumed exp-consumed))
          until (source-empty-p input-source)
          ;; note that the placement of these clauses after the UNTIL clause is important!
          when report-remaining-input-length
            do (format t "~%Remaining characters in input: ~a~%" (remaining-length input-source))
          when report-upcoming-char
            do (format t "~%Upcoming character in input: ~a~%" (read-next-item input-source)))))

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

(defmacro define-regex-matching-loop-test (test-name
                                           &key
                                             (description nil)
                                             regex
                                             input-text
                                             expected-matching-result-details)
  (let ((thefuncall `(run-regex-matching-test-loop
                      ',regex
                      ,input-text
                      ',expected-matching-result-details)))
    (if description
        `(test ,test-name ,description ,thefuncall)
        `(test ,test-name ,thefuncall))))


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
  :expected-accumulator-value nil)

(define-regex-matching-test basic9-regex-matching-test
  :description "Tests no backtracking (no match)."
  :regex (:seq (:+ (:seq #\X #\Y)))
  :input-text "X"
  :expected-matching-status :regex-not-matched
  :expected-accumulator-value nil)

(define-regex-matching-test any-char-test-1
  :description "Tests successful matching of the any-char element."
  :regex (:or (:seq :any-char #\z) "hello")
  :input-text "wz"
  :expected-matching-status :regex-matched
  :expected-accumulator-value "wz")

(define-regex-matching-test any-char-test-2
  :description "Tests failure matching the any-char element, when the meaning is any other char."
  :regex (:or (:seq :any-char #\z) "hello")
  :input-text "hz"
  :expected-matching-status :regex-not-matched
  :expected-accumulator-value nil)

(define-regex-matching-loop-test regex-matching-loop-test-1
  :description "Tests a loop of matching operations against a simple regex. It also tests
consumption of invalid characters (consumed, but not accumulated)."
  :regex (:or
          (:seq #\X #\Y)
          (:seq #\A #\B))
  :input-text "XYABXYABZZZZZ"
  :expected-matching-result-details ((:regex-matched "XY" "XY")
                                     (:regex-matched "AB" "AB")
                                     (:regex-matched "XY" "XY")
                                     (:regex-matched "AB" "AB")
                                     (:regex-not-matched nil "Z")
                                     (:regex-not-matched nil "Z")
                                     (:regex-not-matched nil "Z")
                                     (:regex-not-matched nil "Z")
                                     (:regex-not-matched nil "Z")
                                     ('DOES-NOT-MATTER 'DOES-NOT-MATTER)))

(define-regex-matching-loop-test regex-matching-loop-test-2
  :description "Tests a loop of matching operations against a regex, with backtracking."
  :regex (:+
          (:seq #\X #\Y))
  :input-text "XYXYXZZZ"
  :expected-matching-result-details ((:regex-matched "XYXY" "XYXY")
                                     (:regex-not-matched nil "X")
                                     (:regex-not-matched nil "Z")
                                     (:regex-not-matched nil "Z")
                                     (:regex-not-matched nil "Z")
                                     ('DOES-NOT-MATTER 'DOES-NOT-MATTER)))

(define-regex-matching-loop-test regex-matching-loop-test-3
  :description "Tests a loop of unsuccessful matching operations, till match (finally)."
  :regex (:or
          (:seq #\X #\Y)
          (:seq #\A #\B))
  :input-text "XzAcXwAdXXXYooo"
  :expected-matching-result-details ((:regex-not-matched nil "X")
                                     (:regex-not-matched nil "z")
                                     (:regex-not-matched nil "A")
                                     (:regex-not-matched nil "c")
                                     (:regex-not-matched nil "X")
                                     (:regex-not-matched nil "w")
                                     (:regex-not-matched nil "A")
                                     (:regex-not-matched nil "d")
                                     (:regex-not-matched nil "X")
                                     (:regex-not-matched nil "X")
                                     (:regex-matched "XY" "XY")
                                     (:regex-not-matched nil "o")
                                     (:regex-not-matched nil "o")
                                     (:regex-not-matched nil "o")
                                     ('DOES-NOT-MATTER 'DOES-NOT-MATTER)))

(define-regex-matching-loop-test regex-matching-loop-test-4
  :description "Tests correct matching after skipping a series of bad characters. Notice how the BB
has been matched, although at some point, the cursor was after the AB, thanks to backtracking to
just after the #\A."
  :regex (:or
          (:seq #\A #\B #\C)
          (:seq #\B #\B)
          (:seq #\C #\C (:+ #\D)))
  :input-text "ABBXXXXCCDDDD"
  :expected-matching-result-details ((:regex-not-matched nil "A")
                                     (:regex-matched "BB" "BB")
                                     (:regex-not-matched nil "X")
                                     (:regex-not-matched nil "X")
                                     (:regex-not-matched nil "X")
                                     (:regex-not-matched nil "X")
                                     (:regex-matched "CCDDDD" "CCDDDD")))

(define-regex-matching-loop-test regex-matching-loop-test-5
  :description "Another loop test. Notice the difference between it and test-6/test-7 (+ VS *)."
  :regex (:or
          (:+ #\X)
          (:seq #\A #\B))
  :input-text "XXXXXACABXXXXXZ"
  :expected-matching-result-details ((:regex-matched "XXXXX" "XXXXX")
                                     (:regex-not-matched nil "A")
                                     (:regex-not-matched nil "C")
                                     (:regex-matched "AB" "AB")
                                     (:regex-matched "XXXXX" "XXXXX")
                                     (:regex-not-matched nil "Z")))

(define-regex-matching-loop-test regex-matching-loop-test-6
  :description "Another loop test. Notice the difference between it and test-5 (+ VS *): effect of
* is that a match takes place even on invalid characters, which are consumed but not accumulated.
So regex matches, char is consumed, but not accumulated."
  :regex (:* #\X)
  :input-text "XXXXXABC"
  ;;TODO: note that due to the *, the A matches, but input position does not advance (we're stuck!)
  ;;probably need to advance in case nothing gets consumed.
  :expected-matching-result-details ((:regex-matched "XXXXX" "XXXXX")
                                     (:regex-matched nil "A")
                                     (:regex-matched nil "B")
                                     (:regex-matched nil "C")))


(define-regex-matching-loop-test regex-matching-loop-test-7
  :description "Another loop test. Notice the difference between it and test-5 (+ VS *)."
  :regex (:or
          (:* #\X)
          (:seq #\A #\B))
  :input-text "XXXXXACABXXXXXZ"
  :expected-matching-result-details ((:regex-matched "XXXXX" "XXXXX")
                                     (:regex-matched nil "A")
                                     (:regex-matched nil "C")
                                     (:regex-matched "AB" "AB")
                                     (:regex-matched "XXXXX" "XXXXX")
                                     (:regex-matched nil "Z")))

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
