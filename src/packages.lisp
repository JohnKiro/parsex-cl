(defpackage :parsex-cl
  (:use :cl :iterate)
  (:export :todo))

(defpackage :parsex-cl.nfa
  (:use :cl :iterate)
  (:export :todo))

(defpackage :parsex-cl.tokenizer
  (:use :cl)
  (:export :normal-state
           :terminal-state
           :transition-to-other
           :transition-to-self
           :retrieve-atom
           :input-empty-p
           :update-input
           :update-accumulator
           :token ;reader of token from output obj
           :accumulator ;reader of token accumulator from output obj
           :tokenize ;TODO: may remove, since create-tokenizer is simpler
           :create-tokenizer))

(defpackage :parsex-cl.basic-string-tokenizer
  (:use :cl :parsex-cl.tokenizer :alexandria))
