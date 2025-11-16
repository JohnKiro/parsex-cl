(defpackage :parsex-cl/manual-tokenizer/test
  (:use :cl :fiveam))

(defpackage :parsex-cl/manual-tokenizer/tokenizer/test
  (:use :cl
        :fiveam
        :parsex-cl/manual-tokenizer/test
        :parsex-cl/manual-tokenizer
        :parsex-cl/manual-tokenizer/states
        :parsex-cl/manual-tokenizer/transitions
        :parsex-cl/manual-tokenizer/commons/common-transition-finders
        :parsex-cl/manual-tokenizer/commons/common-atom-matchers
        :parsex-cl/manual-tokenizer/basic-string-tokenizer))

