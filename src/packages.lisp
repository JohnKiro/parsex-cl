(defpackage :parsex-cl
  (:use :cl :iterate)
  (:export :todo))

(defpackage :parsex-cl.nfa
  (:use :cl :iterate)
  (:export :todo))

(defpackage :parsex-cl.tokenizer-states
  (:use :cl)
  (:export :normal-state
           :transition-finder-func
           :token-on-no-input
           :terminal-state
           :terminal-token
           :get-next-state))

(defpackage :parsex-cl.tokenizer-transitions
  (:use :cl :parsex-cl.tokenizer-states)
  (:export :transition-to-other
           :transition-to-self
           :atom-handling
           :get-next-state))

(defpackage :parsex-cl.tokenizer
  (:use :cl
        :parsex-cl.tokenizer-states
        :parsex-cl.tokenizer-transitions)
  (:export :retrieve-atom
           :input-empty-p
           :update-input
           :update-accumulator
           :prepare-tokenization-result
           :tokenize
           :create-tokenizer))

(defpackage :parsex-cl.basic-string-tokenizer
  (:use :cl
        :parsex-cl.tokenizer
        :parsex-cl.tokenizer-states
        :parsex-cl.tokenizer-transitions
        :alexandria)
  (:export :string-input
           :input-text
           :reading-index
           :init-accumulator
           :input-empty-p
           :retrieve-atom
           :update-input
           :update-accumulator
           :accumulator
           :token))

(defpackage :parsex-cl.common-transition-finders
  (:use :cl)
  (:export :find-transition-from-set
           :create-set-based-transition-finder))

(defpackage :parsex-cl.common-atom-matchers
  (:use :cl)
  (:export :create-matcher-for-single-char
            :create-matcher-for-char-range
            :create-matcher-for-char-set))
(defpackage :parsex-cl.regex
  (:use #:cl #:iterate)
  (:export #:parse-and-produce-dfa
           #:match-regex
           #:create-basic-accumulator
           #:create-basic-input
           #:regex-matching-result
           #:regex-matching-result-accumulator-interface-fn
           #:regex-matching-result-input-interface-fn
           #:regex-matching-result-p
           #:regex-matching-result-status
           #:regex-matching-result-token))
