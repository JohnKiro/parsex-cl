(in-package :parsex-cl/regex-core)

;;;;
;;;; API interface for regex system, for use by client code (e.g. tokenizer, grep-like tools, etc.).
;;;;

(defun make-regex-machine-core (regex-root-elem)
  "Takes a regex root element `regex-root-elem`, and processes it in collaboration with NFA and DFA
handlers, to produce a regex machine (core), which is represented by the corresponding root DFA state.
It returns root DFA state, and also returns the root NFA state as secondary return value (e.g. for
tracing, debugging)."
  (let* ((root-nfa-state (nfa:produce-nfa regex-root-elem))
         (root-dfa-state (dfa:nfa-to-dfa root-nfa-state)))
    (values root-dfa-state root-nfa-state)))
