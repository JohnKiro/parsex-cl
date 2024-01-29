(in-package :parsex-cl.tokenizer.test)

;; Test suite to contain all tokenizer tests
(def-suite tokenizer-tests
  :description "Tests the tokenizer"
  :in parsex-cl.test:parsex-cl-tests)

;;; All tests below to be part of tokenizer-tests
(in-suite tokenizer-tests)

(test warming-up
  (is (= 9 (+ 5 4)))
  (is (equalp #\a #\A)))

;;; To use it, simply call it, and inspect output.
(let* ((acceptance-state (make-instance 'terminal-state
                                        :terminal-token :TOKEN-ENDING-WITH-DOLLAR-SIGN))
       (trans-to-acceptance (make-instance 'transition-to-other
                                           :next-state acceptance-state
                                           :atom-handling :use-atom))
       (self-trans (make-instance 'transition-to-self :atom-handling :use-atom))
       (initial-state (make-instance 'normal-state
                                     :token-on-no-input :INPUT-EMPTY-TOKEN
                                     :transition-finder-func (lambda (c)
                                                               (if (char= c #\$)
                                                                   trans-to-acceptance
                                                                   self-trans)))))
  (defun tokenize-and-collect-tokens ()
    (let* ((input (make-instance 'string-input :input-text "one$two$three$"))
           (sample-tokenizer (create-tokenizer initial-state input #'init-accumulator)))
      (loop for i from 1 to 4
         collecting (funcall sample-tokenizer))))

  (defun tokenize-just-one-time ()
    (let* ((input (make-instance 'string-input :input-text "AAA$BBB$CCC$"))
           (sample-tokenizer (create-tokenizer initial-state input #'init-accumulator)))
      (funcall sample-tokenizer))))

;;; TODO: May try to create a loop of "is" statements instead (if possible)
;;; this may help to see any error easily.
(test simple-string-tokenization-test-loop
  (let* ((tokenization-result-list (tokenize-and-collect-tokens))
         (tokens (mapcar #'token tokenization-result-list))
         (accumulators (mapcar #'accumulator tokenization-result-list)))
    (is (equal tokens '(:TOKEN-ENDING-WITH-DOLLAR-SIGN
                        :TOKEN-ENDING-WITH-DOLLAR-SIGN
                        :TOKEN-ENDING-WITH-DOLLAR-SIGN
                        :INPUT-EMPTY-TOKEN)))
    (is (equal accumulators '((#\o #\n #\e #\$)
                              (#\t #\w #\o #\$)
                              (#\t #\h #\r #\e #\e #\$)
                              NIL)))))

(test simple-string-tokenization-test-single
  (multiple-value-bind (result input) (tokenize-just-one-time)
    (let* ((token (token result))
           (accumulator (accumulator result))
           (input-reading-index (reading-index input)))
      (is (eq token :TOKEN-ENDING-WITH-DOLLAR-SIGN))
      (is (equal accumulator '(#\A #\A #\A #\$)))
      (is (= input-reading-index 4)))))
