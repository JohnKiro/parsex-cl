(in-package :parsex-cl.test/tokenizer.test)

;; Test suite to contain all tokenizer tests
(def-suite :parsex-cl/tokenizer.test-suite
  :description "Tests both source-backed tokenizer and backtracking tokenizer."
  :in :parsex-cl.test-suite)

;;; All tests below to be part of tokenizer-tests
(in-suite :parsex-cl/tokenizer.test-suite)

(defun run-source-backed-tokenization-test-loop (tokenizer-core-dfa input-text expected-tokens)
  "Runs tokenization loop on `input-text` against tokenizer core `tokenizer-core-dfa`, expecting a series
of tokens described by `expected-tokens`, which is a list with each element in the form:
(expected-token-id expected-token-text)."
  (let* ((input (input:create-basic-regex-input input-text))
         (tokenizer-fn (tokenizer:create-source-backed-tokenizer tokenizer-core-dfa input)))
    (loop for tokenizer-result = (funcall tokenizer-fn)
          for (token-id . token-text) = (cons (and tokenizer-result (aref tokenizer-result 0))
                                              (input:retrieve-last-accumulated-value input))
          for (expected-token-id expected-token-text) in expected-tokens
          do (fiveam:is (equal token-id expected-token-id))
             (fiveam:is (equal token-text expected-token-text))
          until (null token-id))))

(defmacro deftest (name &key desc tokenizer-core-dfa input-text expected-tokens)
  "Define a test case for tokenization loop on `input-text` against tokenizer `tokenizer-core-dfa`,
expecting a series of tokens described by `expected-tokens`, which is a list with each element in the
form (expected-token-id expected-token-text).
Note that the `expected-tokens`argument is not evaluated (should not be quoted)."
  `(fiveam:test ,name ,@(when desc (list desc))
     (run-source-backed-tokenization-test-loop ,tokenizer-core-dfa ,input-text ',expected-tokens)))

(deftest source-backed-tokenizer-test-1
  :desc "Basic source-backed tokenizer test for the happy scenario (no invalid input characters), using
tokenizer core macro."
  :tokenizer-core-dfa (token-core:tokenizer
                        (token-core:token assign-op (regex:regex "="))
                        (token-core:token mul-op (regex:regex "*"))
                        (token-core:token add-op (regex:regex "+"))
                        (token-core:token left-paren (regex:regex #\())
                        (token-core:token right-paren (regex:regex #\)))
                        (token-core:token statement-term (regex:regex ";"))
                        (token-core:token id (regex:regex (seq #1=(char-range #\A #\z)
                                                               (* (or #1#
                                                                      (char-range #\0 #\9))))))
                        (token-core:token num (regex:regex (+ (char-range #\0 #\9)))))
  :input-text "id1=id2*(id3+11);"
  :expected-tokens ((id "id1")
                    (assign-op "=")
                    (id "id2")
                    (mul-op "*")
                    (left-paren "(")
                    (id "id3")
                    (add-op "+")
                    (num "11")
                    (right-paren ")")
                    (statement-term ";")
                    (nil nil)))

(deftest source-backed-tokenizer-test-2
  :desc "Basic source-backed tokenizer test for the happy scenario (no invalid input characters), using
tokenizer sexp macro instead."
  :tokenizer-core-dfa (token-core:tokenizer
                        (token:token assign-op "=")
                        (token:token mul-op "*")
                        (token:token add-op "+")
                        (token:token left-paren #\()
                        (token:token right-paren #\))
                        (token:token statement-term ";")
                        (token:token id (seq #1=(char-range #\A #\z)
                                             (* (or #1#
                                                    (char-range #\0 #\9)))))
                        (token:token num (+ (char-range #\0 #\9))))
  :input-text "id1=id2*(id3+11);"
  :expected-tokens ((id "id1")
                    (assign-op "=")
                    (id "id2")
                    (mul-op "*")
                    (left-paren "(")
                    (id "id3")
                    (add-op "+")
                    (num "11")
                    (right-paren ")")
                    (statement-term ";")
                    (nil nil)))
