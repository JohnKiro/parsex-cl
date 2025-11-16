(defpackage :parsex-cl/manual-tokenizer/states
  (:use #:cl)
  (:export #:normal-state
           #:transition-finder-func
           #:token-on-no-input
           #:terminal-state
           #:terminal-token
           #:get-next-state))

(defpackage :parsex-cl/manual-tokenizer/transitions
  (:use #:cl
        #:parsex-cl/manual-tokenizer/states)
  (:export #:transition-to-other
           #:transition-to-self
           #:atom-handling
           #:get-next-state))

(defpackage :parsex-cl/manual-tokenizer
  (:use #:cl
        #:parsex-cl/manual-tokenizer/states
        #:parsex-cl/manual-tokenizer/transitions)
  (:export #:retrieve-atom
           #:input-empty-p
           #:update-input
           #:update-accumulator
           #:prepare-tokenization-result
           #:tokenize
           #:create-tokenizer))

(defpackage :parsex-cl/manual-tokenizer/basic-string-tokenizer
  (:use #:cl
        #:parsex-cl/manual-tokenizer
        #:parsex-cl/manual-tokenizer/states
        #:parsex-cl/manual-tokenizer/transitions
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

(defpackage :parsex-cl/manual-tokenizer/commons/common-transition-finders
  (:use #:cl)
  (:export #:find-transition-from-set
           #:create-set-based-transition-finder))

(defpackage :parsex-cl/manual-tokenizer/commons/common-atom-matchers
  (:use #:cl)
  (:export #:create-matcher-for-single-char
           #:create-matcher-for-char-range
           #:create-matcher-for-char-set))
