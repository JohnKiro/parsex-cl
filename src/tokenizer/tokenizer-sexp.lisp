(in-package :parsex-cl/tokenizer/sexp)

(defmacro token (token-id regex-sexp)
  "Prepares a token definition, taking `token-id` and `regex-sexp` in sexp form, and producing a
corresponding `token-holder-element` object.
Note that the arguments are not evaluated, so should not be quoted.
Example usage:
    (token (+ (char-range #\0 #\9)) int)"
  `(elm:prepare-token-element (regex-sexp:regex ,regex-sexp) ',token-id))

