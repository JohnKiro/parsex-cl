(defpackage :parsex-cl.test
  (:use :cl
        :fiveam)
  (:export :parsex-cl.test-suite
           :test-it))

(defpackage :parsex-cl.test/tokenizer.test
  (:use :cl
        :fiveam
        :parsex-cl.test ;package containing root test suite
        :parsex-cl/tokenizer
        :parsex-cl/tokenizer/states
        :parsex-cl/tokenizer/transitions
        :parsex-cl/commons/common-transition-finders
        :parsex-cl/commons/common-atom-matchers
        :parsex-cl/tokenizer/basic-string-tokenizer))

(defpackage :parsex-cl.test/nfa-element.test
  (:use #:cl
        #:fiveam
        #:parsex-cl.test ;package containing root test suite
        )
  (:local-nicknames (:elm #:parsex-cl/regex/element)
                    (:sexp #:parsex-cl/regex/sexp)
                    ;;TODO: should not have such dependency (relocate/remove dependetn test case)
                    (:nfa #:parsex-cl/regex/nfa)))

(defpackage :parsex-cl.test/regex-input.test
  (:use #:cl
        #:parsex-cl.test ;package containing root test suite
        )
  (:local-nicknames (:input #:parsex-cl/regex/input)))

(defpackage :parsex-cl.test/regex.test
  (:use #:cl
        #:fiveam
        #:parsex-cl.test ;package containing root test suite
        )
  (:local-nicknames (:regex #:parsex-cl/regex)
                    (:nfa #:parsex-cl/regex/nfa)
                    (:dfa #:parsex-cl/regex/dfa)
                    (:sexp #:parsex-cl/regex/sexp)
                    (:input #:parsex-cl/regex/input)
                    (:elm #:parsex-cl/regex/element)
                    (:graphviz #:parsex-cl/graphviz-util)))

(defpackage :parsex-cl.test/chars.test
  (:use #:cl
        #:fiveam
        #:parsex-cl.test)
  (:local-nicknames (:chars #:parsex-cl/char-util)
                    (:elm #:parsex-cl/regex/element)
                    (:nfa #:parsex-cl/regex/nfa)))
