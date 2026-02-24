(in-package :parsex-cl/tokenizer-core)

;;;;
;;;; tokenizer core is the regex-based tokenizer core (DFA), that is produced from regex element tree.
;;;;

(defun make-tokenizer-core-builder (regex-tree-constructor-fn)
  "Creates a builder, that builds a tokenizer core incrementally, given token by token. It is useful when
token details are collected iteratively (say, from a larger grammar DSL), not as a single regex tree for
the whole tokenizer.
It receives as argument a regex tree constructor function `regex-tree-constructor-fn`, that receives a
regex in 'raw' format (e.g. sexp), and generates a corresponding regex tree.
It exposes two functions, returned as two values (in order):
- token accumulator function, that receives two arguments: token ID and raw token regex, and updates
internal token elements' sequence.
- builder function, that finalizes the build and returns the tokenizer core."
  (let ((token-elements (make-array 100 :adjustable t :fill-pointer 0)))
    (labels ((add-token-element (token-id regex)
               "Creates a token holder element, given corresponding token id and regex form, and appends
it to internal token sequence."
               (let* ((regex-tree (funcall regex-tree-constructor-fn regex))
                      (tok (make-instance 'elm:token-holder-element :token token-id
                                                                    :element regex-tree)))
                 (vector-push-extend tok token-elements)))
             (build ()
               "Combines token sequence into a full tokenizer core element, via an OR wrapper, and then
generates and returns corresponding DFA machine."
               (regex-core:make-regex-machine-core (make-instance 'elm:or-element
                                                                  :elements token-elements))))
      (values #'add-token-element #'build))))
