(in-package :parsex-cl.test/backtracking-tokenizer.test)

;; Test suite to contain all tokenizer tests
(def-suite backtracking-tokenizer.test-suite
  :description "Tests backtracking tokenizer."
  :in :parsex-cl.test-suite)

;;; All tests below to be part of tokenizer-tests
(in-suite backtracking-tokenizer.test-suite)

(defun prep-tokenizer (tokenizer-core input-text)
  (let* ((input (input:create-basic-regex-input input-text))
         (source-backed-tokenizer-fn (tokenizer:create-source-backed-tokenizer tokenizer-core input)))
    (values (bt-tokenizer:create-backtracking-tokenizer source-backed-tokenizer-fn input) input)))

(defmacro deftest (name desc ((tokenizer-core input-text) (tokenizer-var input-var)) &body body)
  "Define a test case for backtracking tokenization on `input-text` against tokenizer core (root DFA
state) `tokenizer-core`, and applying test assertions provided in the `body`."
  `(fiveam:test ,name ,@(when desc (list desc))
     (multiple-value-bind (,tokenizer-var ,input-var) (prep-tokenizer ,tokenizer-core ,input-text)
       ,@body)))

(defun match-and-check (tokenizer expected-token-id &optional (expected-status :ok))
  (multiple-value-bind (token-id status) (bt-tokenizer::match-token tokenizer expected-token-id)
    (is (equal token-id expected-token-id))
    (is (equal status expected-status))))

(defparameter *sample-tokenizer-core*
  (token-core:tokenizer
    (token:token assign-op "=")
    (token:token mul-op "*")
    (token:token add-op "+")
    (token:token left-paren #\()
    (token:token right-paren #\))
    (token:token statement-term ";")
    (token:token id (seq #1=(char-range #\A #\z)
                         (* (or #1#
                                (char-range #\0 #\9)))))
    (token:token num (+ (char-range #\0 #\9)))))

(defparameter *sample-input-text* "id1=id2*(id3+11);")

(deftest bt-tokenizer-test-1
  "Backtracking tokenizer test, covering the happy path (no backtracking, just acting as a wrapper around
a source-backed tokenizer). Also tests input exhaustion (no more input for tokenization to work)."
  ((*sample-tokenizer-core* *sample-input-text*)
   (tokenizer input))
  (match-and-check tokenizer 'id)
  (match-and-check tokenizer 'assign-op)
  (match-and-check tokenizer 'id)
  (match-and-check tokenizer 'mul-op)
  (match-and-check tokenizer 'left-paren)
  (match-and-check tokenizer 'id)
  (match-and-check tokenizer 'add-op)
  (match-and-check tokenizer 'num)
  (match-and-check tokenizer 'right-paren)
  (match-and-check tokenizer 'statement-term)
  (match-and-check tokenizer nil :invalid-token-or-empty-input))

(deftest bt-tokenizer-test-2
  "Backtracking tokenizer test, covering a reasonable execution, including marking, unmarking, and
rewinding to backtracking points. Note that I haven't yet settled on the return value of the mark /
unmark / rewind functions, and may change them later (so this TC should be updated accordingly then)."
  ((*sample-tokenizer-core* *sample-input-text*)
   (tokenizer input))
  (match-and-check tokenizer 'id)
  (match-and-check tokenizer 'assign-op)
  (match-and-check tokenizer 'id)
  (match-and-check tokenizer 'mul-op)
  ;; first backtracking pos
  (is (equal (bt-tokenizer:mark-backtracking-position tokenizer 'owner1) '((4 . owner1))))
  (match-and-check tokenizer 'left-paren)
  (match-and-check tokenizer 'id)
  (match-and-check tokenizer 'add-op)
  ;; second backtracking pos
  (is (equal (bt-tokenizer:mark-backtracking-position tokenizer 'owner2) '((7 . owner2) (4 . owner1))))
  (match-and-check tokenizer 'num)
  ;; unmark (cancel) second backtracking pos
  (is (equal (bt-tokenizer:unmark-backtracking-position tokenizer 'owner2) '(7 . owner2)))
  (match-and-check tokenizer 'right-paren)
  ;; rewind to BT position on top of stack
  (is (equal (bt-tokenizer:rewind-token-position tokenizer 'owner1) 4))
  (match-and-check tokenizer 'left-paren)
  (match-and-check tokenizer 'id)
  (match-and-check tokenizer 'add-op)
  (match-and-check tokenizer 'num)
  (match-and-check tokenizer 'right-paren)
  (match-and-check tokenizer 'statement-term))

(deftest bt-tokenizer-test-3
  "Backtracking tokenizer test, covering error handling during rewinding and unmarking."
  ((*sample-tokenizer-core* *sample-input-text*)
   (tokenizer input))
  (match-and-check tokenizer 'id)
  (match-and-check tokenizer 'assign-op)
  (match-and-check tokenizer 'id)
  (match-and-check tokenizer 'mul-op)
  ;; first backtracking pos
  (is (equal (bt-tokenizer:mark-backtracking-position tokenizer 'owner1) '((4 . owner1))))
  (match-and-check tokenizer 'left-paren)
  (match-and-check tokenizer 'id)
  (match-and-check tokenizer 'add-op)
  ;; second backtracking pos
  (is (equal (bt-tokenizer:mark-backtracking-position tokenizer 'owner2) '((7 . owner2) (4 . owner1))))
  (match-and-check tokenizer 'num)
  ;; attempt to rewind with incorrect owner (should be TOS' owner, which is owner2)
  (signals error (bt-tokenizer:rewind-token-position tokenizer 'owner1))
  ;; attempt to unmark with incorrect owner (should be TOS' owner, which is owner2)
  (signals error (bt-tokenizer:unmark-backtracking-position tokenizer 'owner1))
  (bt-tokenizer:unmark-backtracking-position tokenizer 'owner2)
  (bt-tokenizer:unmark-backtracking-position tokenizer 'owner1)
  ;; attempt to unmark with empty backtracking markers stack 
  (signals error (bt-tokenizer:unmark-backtracking-position tokenizer 'owner1))
  ;; note that previous failures did not affect the state
  (match-and-check tokenizer 'right-paren)
  (match-and-check tokenizer 'statement-term))
