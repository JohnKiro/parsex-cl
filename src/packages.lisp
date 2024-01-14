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
           :tokenizer-fsm-config
           :tokenize))

(defpackage :parsex-cl.basic-string-tokenizer
  (:use :cl :parsex-cl.tokenizer :alexandria))
