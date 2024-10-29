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

(defpackage :parsex-cl.nfa-element.test
  (:use #:cl
        #:fiveam
        #:parsex-cl.test ;package containing root test suite
        )
  (:local-nicknames (:elm #:parsex-cl.regex-element)
                    (:sexp #:parsex-cl.regex-sexp)
                    ;;TODO: should not have such dependency (relocate/remove dependetn test case)
                    (:nfa #:parsex-cl.regex-nfa)))

(defpackage :parsex-cl.regex.test
  (:use #:cl
        #:fiveam
        #:parsex-cl.test ;package containing root test suite
        #:parsex-cl.regex
        #:parsex-cl.graphviz-util
        )
  (:local-nicknames (:nfa #:parsex-cl.regex-nfa)
                    (:sexp #:parsex-cl.regex-sexp)
                    (:input #:parsex-cl.regex.input)))

(defpackage :parsex-cl.chars.test
  (:use #:cl
        #:fiveam
        #:parsex-cl.test)
  (:local-nicknames (:chars #:parsex-cl.chars)
                    (:elm #:parsex-cl.regex-element)
                    (:nfa #:parsex-cl.regex-nfa)))
