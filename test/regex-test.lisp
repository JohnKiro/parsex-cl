(in-package :parsex-cl.test/regex.test)

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
  `(test ,name ,@(when desc (list desc))
     (run-regex-matching-test-loop ',regex ,inp (match-results ',match-details-list))))

(defmacro deftest-1 (name &key desc regex inp match (acc (and match inp)) (consum acc))
  "Provides a simpler interface to DEFTEST, in case we are testing a single regex matching
operation. The only change in the arguments is for the match details, with the MATCH specifing the
expected matching status (T / NIL), the ACC specifing the expected accumulator's value, and CONSUM
specifies the expected consumed value. The accumulated value defaults to the input
(INP) in case of match, and NIL in case of no match. The consumed value defaults to the accumulated
value. These are sensible defaults.
TODO: I think consumed value better default to ACC if matched, and to first char of input if no
match."
  `(deftest ,name :desc ,desc :regex ,regex :inp ,inp
     :match-details-list ((,match ,acc ,consum))))

(defmacro deftest-n (name &key desc regex test-details-list)
  "Allows defining multiple tests at once, all of them are against a single regex but with different
inputs (and hence different expected matching results. This helps avoid redundancy in case we need
to test matching against a single regex.
Note: I'm defining all as a single test, rather than one test per matching operation. This is since
we have a single description for simplicity (otherwise, I'd have to supply description per test)."
  `(test ,name ,@(when desc (list desc))
     (progn
       ,@(loop for (inp . match-details) in test-details-list
               collect `(run-regex-matching-test-loop ',regex ,inp
                                                      (match-results '(,match-details)))))))

(defun run-regex-matching-test-loop (regex input match-details-list)
  "Reusable function that runs a loop of regex matching operations for the input string INPUT
against a single REGEX, and tests the result of each matching operation against successive elements
of the MATCH-DETAILS-LIST list. The loop stops when the match-details is fully traversed."
  (let* ((input-source (input:create-basic-regex-input input
                                                       :advance-on-no-consumption-on-match t))
         (regex-obj-tree (sexp:prepare-regex-tree regex))
         (nfa (nfa:produce-nfa regex-obj-tree))
         (dfa (dfa:produce-dfa nfa)))
    (input:with-regex-input-handler-funcall-macros (input:retrieve-last-accumulated-value
                                                    input:retrieve-last-consumed-value
                                                    input:remaining-length
                                                    input:read-next-item
                                                    input:source-empty-p) input-source
      (when *graphvizdot-nfa*
        (format t "~%Graphviz for NFA:~%~a~%" (graphviz:fsm-to-graphvizdot nfa)))
      (when *graphvizdot-dfa*
        (format t "~%Graphviz for DFA:~%~a~%" (graphviz:fsm-to-graphvizdot dfa)))
      (dolist (match-details match-details-list)
        (let* ((result (regex:match-regex input-source dfa))
               (matching-status (regex:regex-matching-result-status result))
               (updated-acc (input:retrieve-last-accumulated-value))
               (consumed (input:retrieve-last-consumed-value)))
          (assert-match-result match-details (match-result matching-status
                                                           updated-acc consumed))
          (when *verbose*
            (format t "~%~%Consumed value: ~a." consumed)
            (format t "~&Accumulated value: ~a." updated-acc)
            (format t "~&Result: ~a." result)
            (format t "~&Remaining characters in input: ~a" (input:remaining-length))
            (format t "~&Upcoming character in input: ~a~%" (and (not (input:source-empty-p))
                                                                 (input:read-next-item)))))))))

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

(deftest-1 single-regex-matching-test-15
  :desc "Tests backtracking, consumption (note: depending on input
:advance-on-no-consumption-on-match creation flag)."
  :regex (* "xy")
  :inp "z"
  :match t
  :acc nil
  :consum "z")

(deftest-1 single-regex-matching-test-16
  :desc "Tests backtracking, consumption (2) - see previous TC."
  :regex (* "xy")
  :inp "xz"
  :match t
  :acc nil
  :consum "x")

(deftest-1 single-regex-matching-test-17
  :desc "Tests backtracking for a closure within a seq."
  :regex (seq "AB" (* "xy") "CD")
  :inp "ABCD"
  :match t
  :acc "ABCD"
  :consum "ABCD")

(deftest-1 single-regex-matching-test-18
  :desc "Tests failure to match a closure within a seq."
  :regex (seq "AB" (* "xy") "CD")
  :inp "ABzCD"
  :match nil
  :acc nil
  :consum "A")

;; TODO: compare with the non-greedy negation (the comparison shows that the greedy negation is
;; more reasonable (expected by user)
(deftest-1 single-regex-matching-test-19
  :desc "Tests succes to match a closure of any-char within a seq."
  :regex (seq "AB" (* :any-char) "CD")
  :inp "ABCDCD"
  :match t
  :acc  "ABCDCD"
  :consum  "ABCDCD")

(deftest-1 single-regex-matching-test-20
  :desc "Tests succes to match one-or-more any-char within a seq."
  :regex (seq "AB" (+ :any-char) "CD")
  :inp "ABCDCD"
  :match t
  :acc  "ABCDCD"
  :consum  "ABCDCD")

(deftest-1 single-regex-matching-test-21
  :desc "Tests succes to match one-or-more char found in ending."
  :regex (seq "AB" (+ #\C) "CD")
  :inp "ABCCCCD"
  :match t
  :acc  "ABCCCCD"
  :consum  "ABCCCCD")

(deftest-1 single-regex-matching-test-22
  :desc "Tests succes to match one-or-more seq, repeated in ending."
  :regex (seq "AB" (+ "CD") "CD")
  :inp "ABCDCDCD"
  :match t
  :acc  "ABCDCDCD"
  :consum  "ABCDCDCD")

(deftest-n negation-tests-0
  :desc "Simple test to familiarize with negation. Note that (not #\B) accepts all characters other
than #\B, and also accepts the empty string (since empty string is NOT #\B)."
  :regex (not #\B)
  :test-details-list (("Ax" t "A" "A")
                      ("Bx" t nil "B") ;'B' consumed based on flag (advance on match on no consump)
                      ("" t nil nil)))

(deftest-n negation-tests-1
  :desc "Tests success for negation of ORing of single char and char range elements, negation set
includes empty string."
  :regex (not (or #\B #\D (char-range #\L #\S) #\W #\Y))
  :test-details-list (("Ax" t "A" "A")
                      ("Bx" t nil "B")
                      ("Cx" t "C" "C")
                      ("Dx" t nil "D")
                      ("Kx" t "K" "K")
                      ("Lx" t nil "L")
                      ("Sx" t nil "S")
                      ("Vx" t "V" "V")
                      ("Wx" t nil "W")
                      ("Xx" t "X" "X")
                      ("Yx" t nil "Y")
                      ("Zx" t "Z" "Z")
                      ("" t "" "")))

(deftest-n negation-tests-2
  :desc "Tests success and failure for negation of ORing of single char and char range elements, the
zero-or-one wrapper element removes the empty string from the negation set."
  :regex (not (? (or #\B #\D (char-range #\L #\S) #\W #\Y)))
  :test-details-list (("Ax" t "A" "A")
                      ("Bx" nil nil "B")
                      ("Cx" t "C" "C")
                      ("Dx" nil nil "D")
                      ("Kx" t "K" "K")
                      ("Lx" nil nil "L")
                      ("Sx" nil nil "S")
                      ("Vx" t "V" "V")
                      ("Wx" nil nil "W")
                      ("Xx" t "X" "X")
                      ("Yx" nil nil "Y")
                      ("Zx" t "Z" "Z")
                      ("" nil nil nil)))

;; Note that (not #\M) matches empty string, that's why "abyz" gets matched
;; Note that upon match failure, only one char is consumed, according to how the basic input handler
;; operates (which is not specific to the negation element). In another implementation, the whole
;; text up to the point of failure detection would be consumed (depending on the specific
;; application's desired behavior)
(deftest-n negation-tests-3
  :desc "Tests negation part of a larger regex (sequence including negated part)."
  :regex (seq "ab" (not #\M) "yz")
  :test-details-list (("abcyz" t "abcyz" "abcyz")
                      ("abyz" t "abyz" "abyz")
                      ("abMyz" nil nil "a")))

(deftest-n negation-tests-4
  :desc "Tests ORing of two negations (leading to acceptance of all chars)."
  :regex (or (not #\B) (not #\D))
  :test-details-list (("Ax" t "A" "A")
                      ("Bx" t "B" "B")
                      ("Cx" t "C" "C")
                      ("Dx" t "D" "D")
                      ("Ex" t "E" "E")
                      ("" t nil nil)))

(deftest-n negation-tests-5
  :desc "Tests negation of closure: default implementation is 'tolerant', i.e. 'ABA' accepts. I will
later implement the non-tolerant case (I see would mark the red states to disallow backtracking)."
  :regex (not (* "AB"))
  :test-details-list (("AB" t "A" "A")  ;tolerance case (user would expect NO MATCH)
                      ("ABx" t "ABx" "ABx") ; same note
                      ("ABA" t "ABA" "ABA") ; same note
                      ("ABABC" t "ABABC" "ABABC") ; same note, but missing "any-other"? See also next TC
                      ("ABAB" t "ABA" "ABA") ; same note
                      ("ABAC" t "ABAC" "ABAC") ; same note
                      ("AA" t "AA" "AA")
                      ("AC" t "AC" "AC")
                      ("ACD" t "AC" "AC")
                      ("B" t "B" "B")
                      ("BAB" t "B" "B")
                      ("" nil nil nil)))

;; TODO: recheck "xyABABCwv" (missing any-other?), comparing with NFA and DFA generated with old
;; algo (see chapter 8 in ODT).
(deftest-n negation-tests-6
  :desc "Tests negation of closure, inside a sequence. See notes about tolerance in previous TC,
however, note that in this case, there is no backtracking to 'A', due to the trailing 'wv'."
  :regex (seq "xy" (not (* "AB")) "wv")
  :test-details-list (("xyABwv" nil nil "x")
                      ("xyAwv" t "xyAwv" "xyAwv")
                      ("xyAwwv" t "xyAwwv" "xyAwwv")
                      ("xyAxwv" t "xyAxwv" "xyAxwv")
                      ("xyABAwv" t "xyABAwv" "xyABAwv") ; note the tolerance (default)
                      ("xyABABCwv" t "xyABABCwv" "xyABABCwv") ;TODO: tolerance is broken :( (missing "any-other"?)
                      ("xyABABwv" nil nil "x")
                      ("xyAB" nil nil "x")
                      ("xyABx" nil nil "x")
                      ("xywv" nil nil "x") ;match set of the NOT part does not include empty string
                      ("xywwv" t "xywwv" "xywwv")
                      ("xyxwv" t "xyxwv" "xyxwv")
                      ("xywvwv" nil nil "x") ;TODO: may surprise the user (the non-greedy option)
                      ("xywABwv" nil nil "x") ;same
                      ("" nil nil nil)))

(deftest-n negation-tests-7
  :desc "Tests double negation of closure. Functionally, simplification by cancelling the two
negations out should not affect the result (to be verified, also for greedy and intolerance)."
  :regex (not (not (* "AB")))
  :test-details-list (("AB" t "AB" "AB")
                      ("ABAB" t "ABAB" "ABAB")
                      ("ABx" t "AB" "AB")
                      ("ABABx" t "ABAB" "ABAB")
                      ("" t "" "")
                      ("ABX" t "AB" "AB")
                      ("ABAX" t "AB" "AB")
                      ("xABwwv" t nil "x") ;consumption due to input advance-on-.... flag
                      ("AxAB" t nil "A"))) ;same

(deftest-n negation-tests-8
  :desc "Tests negation of sequence, inside a sequence, and including any-char."
  :regex (seq "xy" (not (seq #\m :any-char)) "wv")
  :test-details-list (("xywv" t "xywv" "xywv") ;note that the NOT match set includes empty string
                      ("xyAwv" t "xyAwv" "xyAwv")
                      ("xyAwwv" nil nil "x") ;TODO: this and the next: need any-other? (TODO above)
                      ("xyAxwv" nil nil "x")
                      ("xymAwv" nil nil "x")
                      ("xymwv" t "xymwv" "xymwv")))

(deftest-n negation-tests-9
  :desc "Tests negation of one-or-more, inside a sequence. Compare with `negation-tests-6`."
  :regex (seq "xy" (not (+ "AB")) "wv")
  :test-details-list (("xyABwv" nil nil "x")
                      ("xyAwv" t "xyAwv" "xyAwv")
                      ("xyAwwv" t "xyAwwv" "xyAwwv")
                      ("xyAxwv" t "xyAxwv" "xyAxwv")
                      ("xyABAwv" t "xyABAwv" "xyABAwv") ; note the tolerance (default)
                      ("xyABABCwv" t "xyABABCwv" "xyABABCwv") ;TODO: tolerance is broken :( (missing "any-other"?)
                      ("xyABABwv" nil nil "x")
                      ("xyAB" nil nil "x")
                      ("xyABx" nil nil "x")
                      ("xywv" t "xywv" "xywv") ;match set of the NOT part includes empty string
                      ("xywwv" t "xywwv" "xywwv")
                      ("xyxwv" t "xyxwv" "xyxwv")
                      ("xywvwv" t "xywv" "xywv") ;may surprise the user (the non-greedy option)
                      ("xywABwv" nil nil "x") ;same
                      ("" nil nil nil)))

(deftest-n negation-tests-10
  :desc "Tests triple negation of closure: Expecting same behavior as single negation (see
`negation-tests-5"
  :regex (not (not (not (* "AB"))))
  :test-details-list (("AB" t "A" "A") ;tolerance case (user would expect NO MATCH)
                      ("ABx" t "ABx" "ABx") ; same note
                      ("ABA" t "ABA" "ABA") ; same note
                      ("ABABC" t "ABABC" "ABABC") ; same note, but missing "any-other"? See also next TC
                      ("ABAB" t "ABA" "ABA") ; same note
                      ("ABAC" t "ABAC" "ABAC") ; same note
                      ("AA" t "AA" "AA")
                      ("AC" t "AC" "AC")
                      ("ACD" t "AC" "AC")
                      ("B" t "B" "B")
                      ("BAB" t "B" "B")
                      ("" nil nil nil)))

(deftest-n negation-tests-11
  :desc "Tests negation of one-or-more. See also `negation-tests-9`."
  :regex (not (+ "AB"))
  :test-details-list (("AB" t "A" "A")
                      ("xAB" t "x" "x")
                      ("AA" t "AA" "AA")
                      ("ABAB" t "ABA" "ABA")
                      ("ABA" t "ABA" "ABA")
                      ("ABABC" t "ABABC" "ABABC")
                      ("ABAC" t "ABAC" "ABAC")
                      ("ACB" t "AC" "AC")
                      ("BB" t "B" "B") ;same
                      ("" t nil nil)))


(deftest-n negation-tests-12
  :desc "Tests double negation of one-or-more."
  :regex (not (not (+ "AB")))
  :test-details-list (("AB" t "AB" "AB")
                      ("xAB" nil nil "x")
                      ("AA" nil nil "A")
                      ("ABAB" t "ABAB" "ABAB")
                      ("ABA" t "AB" "AB")
                      ("ABABC" t "ABAB" "ABAB")
                      ("ABAC" t "AB" "AB")
                      ("ACB" nil nil "A")
                      ("BB" nil nil "B") ;same
                      ("" nil nil nil)))

(deftest-n negation-tests-13
  :desc "Tests success for negation of ORing of single char and char range elements. See also
negation test 2."
  :regex (not (or #\B #\D (char-range #\L #\S) #\W #\Y))
  :test-details-list (("Ax" t "A" "A")
                      ("Bx" t nil "B")
                      ("Cx" t "C" "C")
                      ("Dx" t nil "D")
                      ("Kx" t "K" "K")
                      ("Lx" t nil "L")
                      ("Sx" t nil "S")
                      ("Vx" t "V" "V")
                      ("Wx" t nil "W")
                      ("Xx" t "X" "X")
                      ("Yx" t nil "Y")
                      ("Zx" t "Z" "Z")
                      ("" t nil nil)))

(deftest-n negation-tests-14
  :desc "Tests negation of ORing, including inner negation."
  :regex (not (or #\B (not #\E)))
  :test-details-list (("BA" nil nil "B")
                      ("E" t "E" "E")
                      ("Ex" t "E" "E")
                      ("xE" nil nil "x")
                      ("" nil nil nil)))

(deftest-n inv-matching-tests
  :desc "Tests for the INV element, including match/no match, with SEQ and OR elements."
  :regex (inv #\a #\c (char-range #\l #\s) #\x #\z)
  :test-details-list (("a" nil nil "a")
                      ("l" nil nil "l")
                      ("a" nil nil "a")
                      ("s" nil nil "s")
                      ("d" t)
                      ("dyy" t)
                      ("123" t)
                      ("syy" nil nil "s")))

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
  :desc "Tests: ONE-OR-MORE, backtracking to last match position."
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
  ;; thanks to backtracking to just after the #\A. Also remember that we can control even the
  ;; consumption of a single char upon failure to match (flag in regex input interface)
  :desc "Tests: consumption of one bad char, followed by backtracking, and other tests."
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
  :desc "Tests: effect of * (invalid characters are matched, consumed (due to input's
:advance-on-no-consumption-on-match flag), but not accumulated)."
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
