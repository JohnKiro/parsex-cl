(defpackage :parsex-cl
  (:use #:cl
        #:iterate)
  (:export #:todo))

(defpackage :parsex-cl.tokenizer-states
  (:use #:cl)
  (:export #:normal-state
           #:transition-finder-func
           #:token-on-no-input
           #:terminal-state
           #:terminal-token
           #:get-next-state))

(defpackage :parsex-cl.tokenizer-transitions
  (:use #:cl
        #:parsex-cl.tokenizer-states)
  (:export #:transition-to-other
           #:transition-to-self
           #:atom-handling
           #:get-next-state))

(defpackage :parsex-cl.tokenizer
  (:use #:cl
        #:parsex-cl.tokenizer-states
        #:parsex-cl.tokenizer-transitions)
  (:export #:retrieve-atom
           #:input-empty-p
           #:update-input
           #:update-accumulator
           #:prepare-tokenization-result
           #:tokenize
           #:create-tokenizer))

(defpackage :parsex-cl.basic-string-tokenizer
  (:use #:cl
        #:parsex-cl.tokenizer
        #:parsex-cl.tokenizer-states
        #:parsex-cl.tokenizer-transitions
        #:alexandria)
  (:export #:string-input
           #:input-text
           #:reading-index
           #:init-accumulator
           #:input-empty-p
           #:retrieve-atom
           #:update-input
           #:update-accumulator
           #:accumulator
           #:token))

(defpackage :parsex-cl.common-transition-finders
  (:use #:cl)
  (:export #:find-transition-from-set
           #:create-set-based-transition-finder))

(defpackage :parsex-cl.common-atom-matchers
  (:use #:cl)
  (:export #:create-matcher-for-single-char
           #:create-matcher-for-char-range
           #:create-matcher-for-char-set))

(defpackage :parsex-cl.chars
  (:use #:cl #:iterate)
  (:export #:char-range
           #:char-start
           #:char-end
           #:make-char-range
           #:char-range-equal
           #:inc-char
           #:dec-char
           #:insert-chars-in-order
           #:split-char-range
           #:simple-element
           #:simple-element-equal
           ))

(defpackage :parsex-cl.regex.input
  (:use #:cl #:iterate)
  (:export #:basic-regex-input
           #:source-empty-p
           #:remaining-length
           #:read-next-item
           #:advance-reading-position
           #:notify-match-termination
           #:register-candidate-matching-point
           #:retrieve-last-accumulated-value
           #:retrieve-last-consumed-value
           ))

(defpackage :parsex-cl.regex-nfa
  (:use #:cl #:iterate)
  (:local-nicknames (:chars :parsex-cl.chars))
  (:export #:nfa-state
           #:normal-transitions
           #:auto-transitions
           #:element
           #:next-state
           #:terminal-nfa-closure-union-p
           #:prepare-nfa-state-closure-union
           #:terminus
           #:create-nfa-normalized-transition-table-iterator
           #:parse-and-produce-nfa
           #:simple-element-equal
           ))

(defpackage :parsex-cl.regex
  (:use #:cl #:iterate)
  (:local-nicknames (:nfa #:parsex-cl.regex-nfa)
                    (:chars #:parsex-cl.chars)
                    (:input #:parsex-cl.regex.input))
  (:export #:produce-dfa
           #:dfa-state
           #:parse-and-produce-dfa ;wrapper that simplifies the above
           #:dfa-state-definitely-terminal-p
           #:candidate-terminal
           #:match-regex
           #:regex-matching-result
           #:regex-matching-result-accumulator-interface-fn
           #:regex-matching-result-input-interface-fn
           #:regex-matching-result-p
           #:regex-matching-result-status
           #:regex-matching-result-token
           #:transitions
           #:transition-on-any-other
           ))

(defpackage :parsex-cl.fsm-traversal
  (:use #:cl
        #:parsex-cl.regex)
  (:export #:object-lookup-factory-fn
           #:traverse-fsm-transitions
           ))
