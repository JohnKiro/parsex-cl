(in-package :parsex-cl/source-backed-tokenizer)

;;;;
;;;; source-backed-tokenizer factory
;;;;

(defun create-source-backed-tokenizer (tokenizer-core-dfa input-source)
  "Creates and returns a tokenizer powered by a tokenizer core `tokenizer-core-dfa`, and backed by an
input source `input-source`. The returned tokenizer is a closure that performs tokenization each time
it's called, and returns the same values returned by the regex matcher: a regex matching result instance,
and a keyword indicating whether input is exhausted. Note that the two cases are mutually exclusive:
either a result, or input exhausted indication is returned, not both.
Note that client code could use `input-source` to retrieve last accumulated text."
  #+nil(declare (optimize (debug 0) (speed 3)))
  (labels ((get-tokens ()
             "Retrieve next token(s) from source, indicating if input is exhausted in secondary value."
             (match:match-regex input-source tokenizer-core-dfa)))
    #'get-tokens))
