(in-package :parsex-cl.regex.test)

(def-suite :parsex-cl.regex.test-suite
  :description "Tests the RegEx"
  :in :parsex-cl.test-suite)

(in-suite :parsex-cl.regex.test-suite)

;;; sample regex: decimal or hexa, the decimal can be negative, and can have fraction
(defparameter sample-regex '(:or
                             (:seq
                              (:? #\-)
                              (:+ (:char-range #\0 #\9))
                              #\.
                              (:+ (:char-range #\0 #\9)))
                             (:seq "0x" (:+ (:char-range #\0 #\9)))))

(defparameter root-state (parsex-cl.regex::parse-and-produce-nfa sample-regex))
(defparameter root-closure (parsex-cl.regex::prepare-nfa-state-closure nil root-state))
(defparameter char-range-splitting-points
  (parsex-cl.regex::collect-char-range-splitting-points root-closure))

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

(:* (:or
                      (:char-range #\a #\d)
                      (:char-range #\b #\e)
                      "xyz"))

(test graphvizdot-generation
  (let* ((regex '(:* (:or
                      (:char-range #\a #\d)
                      (:char-range #\x #\z))))
         (dfa (parsex-cl.regex::parse-and-produce-dfa regex)))
    (format t "~%Graphviz for DFA:~%~a~%" (parsex-cl.regex::dfa-to-graphvizdot dfa))
    (is (eq t t))))

;;; TODO: need to put more tests in a loop (matching token by token)
(test basic-regex-matching-test
  (let* ((regex '(:+ (:or
                      (:seq #\a #\n)
                      (:seq #\a #\m)
                      (:seq #\a #\t)
                      (:seq #\a #\s)
                      (:char-range #\w #\z))))
         (input-handler (create-basic-input "anamatasxzwy"))
         (accumulator-handler (create-basic-accumulator))
         (accumulator-retrieval-fn (nth-value 0 (funcall accumulator-handler)))
         (dfa (parsex-cl.regex::parse-and-produce-dfa regex))
         (result (match-regex input-handler dfa :accumulator-interface-fn accumulator-handler))
         (matching-status (regex-matching-result-status result))
         (updated-acc (funcall accumulator-retrieval-fn)))
    (format t "~%Updated accumulator is ~a~%" updated-acc)
    (is (equal matching-status :regex-matched))
    (is (equal updated-acc "anamatasxzwy"))))

(test basic2-regex-matching-test
  (let* ((regex '(:+ (:or
                      (:char-range #\a #\d)
                      (:char-range #\b #\e))))
         (input-handler (create-basic-input "abcacdaecccaabeadde"))
         (accumulator-handler (create-basic-accumulator))
         (accumulator-retrieval-fn (nth-value 0 (funcall accumulator-handler)))
         (dfa (parsex-cl.regex::parse-and-produce-dfa regex))
         (result (match-regex input-handler dfa :accumulator-interface-fn accumulator-handler))
         (matching-status (regex-matching-result-status result))
         (updated-acc (funcall accumulator-retrieval-fn)))
    (format t "~%Updated accumulator is ~a~%" updated-acc)
    (format t "~%Graphviz for DFA:~%~a~%" (parsex-cl.regex::dfa-to-graphvizdot dfa))
    (is (equal matching-status :regex-matched))
    (is (equal updated-acc "abcacdaecccaabeadde"))))

(test basic3-regex-matching-test
  (let* ((regex '(:+ (:or
                      (:seq #\a #\n)
                      (:seq :any-char #\M)
                      (:seq :any-char #\P)
                      )))
         (input-handler (create-basic-input "anxMyManzMvMsPiMan"))
         (accumulator-handler (create-basic-accumulator))
         (accumulator-retrieval-fn (nth-value 0 (funcall accumulator-handler)))
         (dfa (parsex-cl.regex::parse-and-produce-dfa regex))
         (result (match-regex input-handler dfa :accumulator-interface-fn accumulator-handler))
         (matching-status (regex-matching-result-status result))
         (updated-acc (funcall accumulator-retrieval-fn)))
    (format t "~%Updated accumulator is ~a~%" updated-acc)
    (format t "~%Graphviz for DFA:~%~a~%" (parsex-cl.regex::dfa-to-graphvizdot dfa))
    (is (equal matching-status :regex-matched))
    (is (equal updated-acc "anxMyManzMvMsPiMan"))))

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
