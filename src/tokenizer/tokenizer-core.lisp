(in-package :parsex-cl/tokenizer-core)

;;;;
;;;; tokenizer core is the regex-based tokenizer core (DFA), that is produced from regex element tree.
;;;;

(defun prepare-tokenizer-dfa (token-elements)
  "Creates a tokenizer DFA tree, given a vector of token definitions, and returns the DFA's start state."
  (regex-core:make-regex-machine-core (elm:prepare-tokenizer-root-element token-elements)))

(defmacro token (token-id regex-element)
  "Prepares a token definition, taking `token-id` and corresponding `regex-element`, and producing a
corresponding `token-holder-element` object.
Example usage (including using the `regex` macro to parse the regex sexp):
    (token (regex:regex int (+ (char-range #\0 #\9)))"
  `(elm:prepare-token-element ,regex-element ',token-id))

(defmacro tokens (&body token-definitions)
  "Prepares a regex root element for a list of tokens that can be used in a tokenization task.
Example usage (together with the `token` and `regex` macros, with the later assumed to be available):
    (tokens
      (token integer (regex:regex (+ (char-range #\0 #\9))))
      (token alpha (regex:regex (+ (char-range #\A #\z)))))"
  `(elm:prepare-tokenizer-root-element (vector ,@token-definitions)))

(defmacro tokenizer (&body token-definitions)
  "Builds a tokenizer, given token definitions. It is very similar to the `tokens` macro, except that it
goes a step further by preparing the regex core (effectively the DFA state tree).
Example usage (together with the `token` and `regex` macros, with the later assumed to be available):
    (tokenizer
      (token (regex:regex (+ (char-range #\0 #\9)) integer))
      (token (regex:regex (+ (char-range #\A #\z)) alpha))) "
  `(prepare-tokenizer-dfa (vector ,@token-definitions)))

(defun make-tokenizer-core-builder ()
  "Creates a builder, that builds a tokenizer core incrementally, given token by token. It is useful when
token details are collected iteratively (say, from a larger grammar DSL), not as a single regex tree for
the whole tokenizer.
It provides two functions, returned as two values (in order):
- token accumulator function, that receives two arguments: token ID and regex element, and updates
internal token elements' sequence.
- builder function, that finalizes the build and returns the tokenizer core."
  (let ((token-elements (make-array 100 :adjustable t :fill-pointer 0)))
    (labels ((add-token-element (token-id regex-element)
               "Creates a token holder element, given corresponding token id and regex element, and
appends it to internal token sequence."
               (let* ((tok-elem (elm:prepare-token-element regex-element token-id)))
                 (vector-push-extend tok-elem token-elements)))
             (build ()
               "Combines token sequence into a full tokenizer core element, via an OR wrapper, and then
generates and returns corresponding DFA machine."
               (prepare-tokenizer-dfa token-elements)))
      (values #'add-token-element #'build))))
