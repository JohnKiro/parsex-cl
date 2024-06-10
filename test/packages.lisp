(defpackage :parsex-cl.test
  (:use :cl
        :fiveam)
  (:export :parsex-cl.test-suite
           :test-it))

(defpackage :parsex-cl.tokenizer.test
  (:use :cl
        :fiveam
        :parsex-cl.test ;package containing root test suite
        :parsex-cl.tokenizer
        :parsex-cl.tokenizer-states
        :parsex-cl.tokenizer-transitions
        :parsex-cl.common-transition-finders
        :parsex-cl.common-atom-matchers
        :parsex-cl.basic-string-tokenizer))

(defpackage :parsex-cl.regex.test
  (:use #:cl
        #:fiveam
        #:parsex-cl.test ;package containing root test suite
        #:parsex-cl.regex-nfa
        #:parsex-cl.regex
        #:parsex-cl.chars
        #:parsex-cl.regex.input
        #:parsex-cl.graphviz-util
        ))
