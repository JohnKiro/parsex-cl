(in-package :parsex-cl/source-backed-tokenizer)

;;;;
;;;; source-backed-tokenizer factory
;;;;

(defun create-source-backed-tokenizer (tokenizer-core-dfa input-source)
  "Creates and returns a tokenizer powered by a tokenizer core `tokenizer-core-dfa`, and backed by an
input source `input-source`. The returned tokenizer is a closure that performs tokenization each time
it's called, and returns the same values returned by the regex matcher: a regex matching result instance.
Note that client code could use `input-source` to retrieve empty status, last accumulated text etc."
  (declare (type dfa:dfa-state tokenizer-core-dfa)
           (type input:input-source input-source))
  (labels ((get-tokens ()
             "Retrieve next token(s) from source."
             (match:match-regex input-source tokenizer-core-dfa)))
    #'get-tokens))
