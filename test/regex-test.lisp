(in-package :parsex-cl.regex.test)

(def-suite :parsex-cl.regex.test-suite
  :description "Tests the RegEx"
  :in :parsex-cl.test-suite)

(in-suite :parsex-cl.regex.test-suite)

(setf fiveam:*on-failure* nil)
(setf fiveam:*on-error* :debug)

;;; TODO: EXPORT THESE DYNAMIC PARAMETERS!
(defparameter *verbose* nil "Switch verbose printing of test progress on/off (for debugging).")
(defparameter *graphvizdot-nfa* nil "Switch generation of Graphviz Dot diagram for NFA.")
(defparameter *graphvizdot-dfa* nil "Switch generation of Graphviz Dot diagram for DFA.")

(defmacro deftest (name &key desc regex inp match-details-list)
  "Define a test case for matching a specific input string (INP) repeatedly against a specific regex
(REGEX). The test is given the name NAME and the description DESC, which are both expected to be
literal values. The REGEX should evaluate to a literal regex, since it is quoted in the body. The
expected matching details are specified in the MATCH-DETAILS-LIST parameter, which is a list of
tuples, one for each matching iteration. The elements in each tuple are as follows (in order):
* matching status (T / NIL), specifying whether match should be successful or failing;
* accumulator value (string or NIL or absent);
* consumed value (string or NIL or absent);
Note: only the matching status is mandatory, the accumulated and consumed values are optional, and
only the present ones will be considered in testing.
Note: NIL (for either accumulated or consumed) means empty string. I.E. can use \"\" and NIL
interchangeably, and normalization will take place in a call to MATCH-RESULT.
Note: it is user's responsibility to ensure no problem caused by the order of arguments passed
(since we're using keyword arguments,allowing to change order)."
  `(test ,name ,desc
     (run-regex-matching-test-loop ',regex ,inp (match-results ',match-details-list))))

(defmacro deftest-1 (name &key desc regex inp match (acc (and match inp)) (consum acc))
  "Provides a simpler interface to DEFTEST, in case we are testing a single regex matching
operation. The only change in the arguments is for the match details, with the MATCH specifing the
expected matching status (T / NIL), the ACC specifing the expected accumulator's value, and CONSUM
specifies the expected consumed value. The accumulated value defaults to the input
(INP) in case of match, and NIL in case of no match. The consumed value defaults to the accumulated
value. These are sensible defaults."
  `(deftest ,name :desc ,desc :regex ,regex :inp ,inp
     :match-details-list ((,match ,acc ,consum))))

(defun run-regex-matching-test-loop (regex input match-details-list)
  "Reusable function that runs a loop of regex matching operations for the input string INPUT
against a single REGEX, and tests the result of each matching operation against successive elements
of the MATCH-DETAILS-LIST list. The loop stops when the match-details is fully traversed."
  (let* ((input-source (make-instance 'input:basic-regex-input :initial-input-text input))
         (regex-obj-tree (sexp:prepare-regex-tree regex))
         (nfa (nfa:parse-and-produce-nfa regex-obj-tree))
         (dfa (parsex-cl.regex:produce-dfa nfa)))
    (when *graphvizdot-nfa*
      (format t "~%Graphviz for NFA:~%~a~%" (parsex-cl.graphviz-util:fsm-to-graphvizdot nfa)))
    (when *graphvizdot-dfa*
      (format t "~%Graphviz for DFA:~%~a~%" (parsex-cl.graphviz-util:fsm-to-graphvizdot dfa)))
    (dolist (match-details match-details-list)
      (let* ((result (match-regex input-source dfa))
             (matching-status (regex-matching-result-status result))
             (updated-acc (input:retrieve-last-accumulated-value input-source))
             (consumed (input:retrieve-last-consumed-value input-source)))
        (assert-match-result match-details (match-result matching-status updated-acc consumed))
        (when *verbose*
          (format t "~%Remaining characters in input: ~a~%" (input:remaining-length input-source))
          (format t "~%Upcoming character in input: ~a~%" (input:read-next-item input-source)))))))

(defun match-result (match &optional (accumul nil acc-supplied-p) (consum nil cons-supplied-p))
  "Prepare matching result (whether expected or actual) in the form of a plist (GETF-friendly),
with only the MATCH value mandatory, and accumulated and consumed values are both optional. Note
that only the present arguments are included in the returned plist.
Note: it normalizes the value of MATCH by accepting either boolean or keyword.
Note: it also normalizes the accumulator and consumed values by changing NIL into empty string.
This is in order to simplify equality test (within the ASSERT-MATCH-RESULT call)."
  (declare (type (or boolean (member :regex-matched :regex-not-matched)) match))
  (append `(:match ,(if (member match '(:regex-matched t))
                        :regex-matched
                        :regex-not-matched))
          (and acc-supplied-p `(:accumulated ,(or accumul "")))
          (and cons-supplied-p `(:consumed ,(or consum "")))))

(defun match-results (match-result-list)
  "Prepares a list of match result plists for the input list of match results (MATCH-RESULT-LIST).
This is done by applying MATCH-RESULT to each element of the input list."
  (loop for r in match-result-list
        collect (apply #'match-result r)))

(defun assert-match-result (expected actual)
  "Tests equality of two plists of matching result. Equality is satisfied only if the key is found
in both EXPECTED and ACTUAL plists, and the corresponding values are equal (using EQUAL).
Note: this function is generic and is not specific to certain keys. Rather, it extracts the keys
from the EXPECTED plist. TODO: consider moving to generic utils.
Note: it assumes results are already normalized (for example, empty strings should be represented
in a single way, such as NIL or \"\"."
  (loop for (k . _) on expected by #'cddr
        do (is (equal (getf expected k) (getf actual k '#:not-found)))))


(deftest-1 single-regex-matching-test-1
  :desc "Tests +, OR, char range, char range splitting."
  :regex (+ (or (seq #\a #\n) (seq #\a #\m) (seq #\a #\t) (seq #\a #\s) (char-range #\w #\z)))
  :inp "anamatasxzwy"
  :match t)

(deftest-1 single-regex-matching-test-2
  :desc "Also tests char splitting. May remove it later (redundant)."
  :regex (+ (or (char-range #\a #\d) (char-range #\b #\e)))
  :inp "abcacdaecccaabeadde"
  :match t)

(deftest-1 single-regex-matching-test-3
  :desc "Tests :any-char"
  :regex (+ (or (seq #\a #\n) (seq :any-char #\M) (seq :any-char #\P)))
  :inp "anxMyPanzMvPan"
  :match t)

(deftest-1 single-regex-matching-test-4
  :desc "Also tests :any-char. This one is simpler to inspect visually."
  :regex (or (seq #\a #\n) (seq :any-char #\M) (seq :any-char #\P))
  :inp "xManxMyPanzMvPan"
  :match t
  :acc "xM")

(deftest-1 single-regex-matching-test-5
  :desc "Tests :or (another test)"
  :regex (+ (or (char-range #\a #\d) (char-range #\c #\f) "lmn" #\x #\y #\z))
  :inp "adcflmnxzlmn"
  :match t)

(deftest-1 single-regex-matching-test-6
  :desc "Tests SEQ, OR, Kleene Closure, string."
  :regex (seq (* (or #\a #\b)) "abb")
  :inp "abbbababbababbabbbbbabb"
  :match t)

(deftest-1 single-regex-matching-test-7
  :desc "Tests stopping at candidate matching point."
  :regex (seq (* #\x) #\y)
  :inp "xxyz"
  :match t
  :acc "xxy")

(deftest-1 single-regex-matching-test-8
  :desc "Tests backtracking to last candidate match (upon no match)."
  :regex (seq (* (seq #\X #\Y)))
  :inp "XYXYXw"
  :match t
  :acc "XYXY")

(deftest-1 single-regex-matching-test-9
  :desc "Tests backtracking to last candidate match (upon input exhaustion)."
  :regex (seq (* (seq #\X #\Y)))
  :inp "XYXYX"
  :match t
  :acc "XYXY")

(deftest-1 single-regex-matching-test-10
  :desc "Tests backtracking to beginning (match empty string)."
  :regex (seq (* (seq #\X #\Y)))
  :inp "X"
  :match t
  :acc nil
  :consum "X")

(deftest-1 single-regex-matching-test-11
  :desc "Tests no backtracking (no match)."
  :regex (seq (+ (seq #\X #\Y)))
  :inp "X"
  :match nil
  :acc nil
  :consum "X")

(deftest-1 single-regex-matching-test-12
  :desc "Tests successful matching of the any-char element."
  :regex (or (seq :any-char #\z) "hello")
  :inp "wz"
  :match t
  :acc "wz")

(deftest-1 single-regex-matching-test-13
  :desc "Tests successful matching the any-char element, even in presence of OR."
  :regex (or (seq :any-char #\z) "hello")
  :inp "hz"
  :match t
  :acc "hz")

(deftest-1 single-regex-matching-test-14
  :desc "Tests backtracking to beginning (match empty string)."
  :regex (seq (* (seq #\X #\Y)))
  :inp "X"
  :match t
  :acc nil
  :consum "X")

(deftest regex-matching-loop-test-1
  :desc "Tests: OR, SEQ of chars, invalid chars consumed but not accumulated."
  :regex (or (seq #\X #\Y) (seq #\A #\B))
  :inp "XYABXYABZZZZZ"
  :match-details-list ((t "XY" "XY")
                       (t "AB" "AB")
                       (t "XY" "XY")
                       (t "AB" "AB")
                       (nil nil "Z")
                       (nil nil "Z")
                       (nil nil "Z")
                       (nil nil "Z")
                       (nil nil "Z")))

(deftest regex-matching-loop-test-2
  :desc "Tests: ONE-OR-MORE, backtracking."
  :regex (+ (seq #\X #\Y))
  :inp "XYXYXZZZ"
  :match-details-list ((t "XYXY" "XYXY")
                       (nil nil "X")
                       (nil nil "Z")
                       (nil nil "Z")
                       (nil nil "Z")))

(deftest regex-matching-loop-test-3
  :desc "Tests: consumption during unsuccessful matching operations, till match (finally)."
  :regex (or (seq #\X #\Y) (seq #\A #\B))
  :inp "XzAcXwAdXXXYooo"
  :match-details-list ((nil nil "X")
                       (nil nil "z")
                       (nil nil "A")
                       (nil nil "c")
                       (nil nil "X")
                       (nil nil "w")
                       (nil nil "A")
                       (nil nil "d")
                       (nil nil "X")
                       (nil nil "X")
                       (t "XY" "XY")
                       (nil nil "o")
                       (nil nil "o")
                       (nil nil "o")))

(deftest regex-matching-loop-test-4
  ;; Notice how the BB has been matched, although at some point, the cursor was after the AB,
  ;; thanks to backtracking to just after the #\A.
  :desc "Tests: consumption of one bad char, followed by backtracking."
  :regex (or (seq #\A #\B #\C) (seq #\B #\B) (seq #\C #\C (+ #\D)))
  :inp "ABBXXXXCCDDDD"
  :match-details-list ((nil nil "A")
                       (t "BB" "BB")
                       (nil nil "X")
                       (nil nil "X")
                       (nil nil "X")
                       (nil nil "X")
                       (t "CCDDDD" "CCDDDD")))

(deftest regex-matching-loop-test-5
  :desc "Notice the difference between it and test-6/test-7 (+ VS *)."
  :regex (or (+ #\X) (seq #\A #\B))
  :inp "XXXXXACABXXXXXZ"
  :match-details-list ((t "XXXXX" "XXXXX")
                       (nil nil "A")
                       (nil nil "C")
                       (t "AB" "AB")
                       (t "XXXXX" "XXXXX")
                       (nil nil "Z")))

(deftest regex-matching-loop-test-6
  :desc "Tests: effect of * (invalid characters are matched, consumed, but not accumulated)."
  :regex (* #\X)
  :inp "XXXXXABC"
  :match-details-list ((t "XXXXX" "XXXXX")
                       (t nil "A")
                       (t nil "B")
                       (t nil "C")))

(deftest regex-matching-loop-test-7
  :desc "Notice the difference between this and test-5 (ONE-OR-MORE VS ZERO-OR-MORE)."
  :regex (or (* #\X) (seq #\A #\B))
  :inp "XXXXXACABXXXXXZ"
  :match-details-list ((t "XXXXX" "XXXXX")
                       (t nil "A")
                       (t nil "C")
                       (t "AB" "AB")
                       (t "XXXXX" "XXXXX")
                       (t nil "Z")))
