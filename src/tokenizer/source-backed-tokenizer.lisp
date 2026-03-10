(in-package :parsex-cl/source-backed-tokenizer)

;;;;
;;;; source-backed-tokenizer factory
;;;;

;; default matching (token equality check)
(defun find-matching-token (expected-token actual-tokens)
  "Matches expected token against one of the actual tokens (those identified by the regex machine),
provided as a sequence, and returns truth or NIL. Note that it relies on default equality test
(using EQL). If more control is needed, the user could use an alternative (custom) implementation."
  (declare (optimize (debug 3) (speed 0)))
  (find expected-token actual-tokens))

(defun create-source-backed-tokenizer (tokenizer-core-dfa input-source)
  "Creates and returns a tokenizer powered by a tokenizer core `tokenizer-core-dfa`, and backed by an
input source `input-source`. The returned tokenizer is a closure that performs tokenization each time
it's called. Note that caller could use `input-source` to retrieve last accumulated text and whether
input is exhausted."
  #+nil(declare (optimize (debug 0) (speed 3)))
  (labels ((get-tokens ()
             "Retrieve next token(s) from source."
             (let ((result (match:match-regex input-source tokenizer-core-dfa)))
               ;; note that tokens alone has implicit indication of whether match is success or not
               ;; (NIL if no match), i.e. no need to inspect the status (:regex-not-matched /
               ;; :regex-matched)
               (match:regex-matching-result-tokens result))))
    #'get-tokens))
