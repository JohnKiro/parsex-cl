(in-package :parsex-cl/regex)

;;;; API interface for regex system, for use by client code (e.g. tokenizer, grep-like tools, etc.).

(defgeneric parse-regex-expression (regex-expression)
  (:documentation "Parse regex expression and produce a tree of regex elements (objects). This method
allows generic interface that could be implemented for different regex expression types (sexp, classic
regex string, JSON etc. Returns the produced element tree root element."))

(defun prepare-regex-machine (regex-expression)
  "Takes a regex expression `regex-expression` (in whatever format), and processes it in collaboration
with NFA and DFA handlers, to produce a regex machine, which is represented by the corresponding root DFA
state, which it returns. It also returns the root NFA state and the root regex element as secondary
return values (e.g. for tracing, debugging)."
  (let* ((regex-root-elem (parse-regex-expression regex-expression))
         (root-nfa-state (nfa:produce-nfa regex-root-elem))
         (root-dfa-state (dfa:nfa-to-dfa root-nfa-state)))
    (values root-dfa-state root-nfa-state regex-root-elem)))
