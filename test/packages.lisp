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
        #:parsex-cl.regex
        #:parsex-cl.graphviz-util
        )
  (:local-nicknames (:nfa #:parsex-cl.regex-nfa)
                    (:input #:parsex-cl.regex.input)))

(defpackage :parsex-cl.chars.test
  (:use #:cl
        #:fiveam
        #:parsex-cl.test)
  (:local-nicknames (:chars #:parsex-cl.chars)
                    (:nfa #:parsex-cl.regex-nfa)))
