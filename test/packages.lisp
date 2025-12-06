(defpackage :parsex-cl.test
  (:use :cl
        :fiveam)
  (:export :parsex-cl.test-suite
           :test-it))

(defpackage :parsex-cl.test/nfa-element.test
  (:use #:cl
        #:fiveam
        #:parsex-cl.test ;package containing root test suite
        )
  (:local-nicknames (:elm #:parsex-cl/regex/element)
                    (:sexp #:parsex-cl/regex/sexp)
                    ;;TODO: should not have such dependency (relocate/remove dependetn test case)
                    (:nfa #:parsex-cl/regex/nfa)
                    (:nfa-state #:parsex-cl/regex/nfa/state)))

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
  (:local-nicknames (:match #:parsex-cl/regex/match)
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
  (:local-nicknames (:chars #:parsex-cl/char-util)))
