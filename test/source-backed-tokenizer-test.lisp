(in-package :parsex-cl.test/source-backed-tokenizer.test)

;; Test suite to contain all tokenizer tests
(def-suite source-backed-tokenizer.test-suite
  :description "Tests source-backed tokenizer."
  :in :parsex-cl.test-suite)

;;; All tests below to be part of tokenizer-tests
(in-suite source-backed-tokenizer.test-suite)

(defun run-source-backed-tokenization-test-loop (tokenizer-core-dfa input-text expected-results)
  "Runs tokenization loop on `input-text` against tokenizer core `tokenizer-core-dfa`, expecting a series
of tokens described by `expected-results`, which is a list with each element in the form:
(expected-token-id expected-token-text expected-status expected-consumed-text), with the last two
parameters optional."
  (let* ((input (input:create-basic-regex-input input-text))
         (tokenizer-fn (tokenizer:create-source-backed-tokenizer tokenizer-core-dfa input)))
    (loop for tokenizer-result = (funcall tokenizer-fn)
          for token-id = (and tokenizer-result (aref tokenizer-result 0))
          for token-text = (input:retrieve-last-accumulated-value input)
          for status = :tbd
          for consumed-text = (input:retrieve-last-consumed-value input)
          for expected-result in expected-results
          for (exp-token-id exp-token-text exp-status exp-consumed-text) = expected-result
          do (fiveam:is (equal token-id exp-token-id))
             (fiveam:is (equal token-text exp-token-text))
             ;; check only if specified in expected
             (when (>= (length expected-result) 3)
               (fiveam:is (equal status exp-status)))
             (when (>= (length expected-result) 4)
               (fiveam:is (equal consumed-text exp-consumed-text))))))

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
  :desc "Basic source-backed tokenizer test for the happy scenario (no invalid input characters), this
time using tokenizer sexp macro (preferred, since it hides regex processing, hence shorter syntax)."
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

(deftest source-backed-tokenizer-test-3
  :desc "Source-backed tokenizer test, with error encountered at start of tokenization."
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
  :input-text "id1=id2*&#$(id3+11);"
  :expected-tokens ((id "id1")
                    (assign-op "=")
                    (id "id2")
                    (mul-op "*")
                    (nil nil :tbd "&")
                    (nil nil :tbd "#")
                    (nil nil :tbd "$")
                    (left-paren "(")
                    (id "id3")
                    (add-op "+")
                    (num "11")
                    (right-paren ")")
                    (statement-term ";")
                    (nil nil)))

(deftest source-backed-tokenizer-test-4
  :desc "Source-backed tokenizer test, with error encountered in the middle of tokenization."
  :tokenizer-core-dfa (token-core:tokenizer
                        (token:token equality-op "==")
                        (token:token statement-term ";")
                        (token:token id (seq #1=(char-range #\A #\z)
                                             (* (or #1#
                                                    (char-range #\0 #\9))))))
  :input-text "id1=#id2;id3==id4;"
  :expected-tokens ((id "id1")
                    (nil nil :tbd "=")
                    (nil nil :tbd "#")
                    (id "id2")
                    (statement-term ";")
                    (id "id3")
                    (equality-op "==" :tbd "==")
                    (id "id4")
                    (statement-term ";")
                    (nil nil)))

(deftest source-backed-tokenizer-test-5
  :desc "Source-backed tokenizer test, backtracking in the middle of tokenization."
  :tokenizer-core-dfa (token-core:tokenizer
                        (token:token assign-op "=")
                        (token:token equality-op "==")
                        (token:token statement-term ";")
                        (token:token id (seq #1=(char-range #\A #\z)
                                             (* (or #1#
                                                    (char-range #\0 #\9))))))
  :input-text "id1=id2;id3==id4;"
  :expected-tokens ((id "id1")
                    (assign-op "=" :tbd "=")
                    (id "id2")
                    (statement-term ";")
                    (id "id3")
                    (equality-op "==" :tbd "==")
                    (id "id4")
                    (statement-term ";")
                    (nil nil)))
