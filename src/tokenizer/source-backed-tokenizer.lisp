(in-package :parsex-cl/source-backed-tokenizer)

;;;;
;;;; source-backed-tokenizer factory
;;;;

(defun create-source-backed-tokenizer (tokenizer-core-dfa input-source)
  "Creates and returns a tokenizer powered by a tokenizer core `tokenizer-core-dfa`, and backed by an
input source `input-source`. The returned tokenizer is a closure that performs tokenization each time
it's called. Note that client code could use `input-source` to retrieve last accumulated text and whether
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
