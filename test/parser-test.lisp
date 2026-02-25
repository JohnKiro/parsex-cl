(in-package :parsex-cl.test/parser.test)

(fiveam:def-suite :parsex-cl/parser.test-suite
  :description "Tests the backtracking recursive descent parser"
  :in :parsex-cl.test-suite)

(fiveam:in-suite :parsex-cl/parser.test-suite)

(setf fiveam:*on-failure* nil)
(setf fiveam:*on-error* :debug)

(fiveam:test parser-smoke-test
  "Basic test that demonstrates parser usage in client code, and provides quick verification."
  (let* ((sample-grammar
           '((token id (seq
                        #1=(or (char-range #\A #\Z) (char-range #\a #\z))
                        (+ (or #1# (char-range #\0 #\9)))))
             (token int (+ (or #1# (char-range #\0 #\9))))
             (token l-paren #\()
             (token r-paren #\))
             (token *-op #\*)
             (token +-op #\+)
             (token assign #\=)
             (token semicolon #\;)
             (token end-of-text "") ;;TODO: HANDLE!!!
             (rule factor (or id int (seq l-paren add-expr r-paren)))
             (rule mul-expr (seq factor (? (seq *-op factor))))
             (rule add-expr (seq mul-expr (? (seq +-op mul-expr))))
             (rule statement (seq id assign add-expr semicolon))
             (rule statement-block (seq statement (* statement)))
             (rule root (seq statement-block end-of-text))))
         (*print-case* :downcase)
         (parsex-cl/rdp/parser:*parse-execution-count* 0)
         (sample-text (concatenate 'string
                                   "id1=(id2*id3);"
                                   "id1=id2*(id+11);"
                                   "id1=(id2+id3);")))
    (multiple-value-bind (root-grammar-constr tokenizer-core-dfa _)
        (parsex-cl/rdp/grammar/sexp:parse-grammar sample-grammar 'root)
      (declare (ignorable _))
      (let* ((input (input:create-basic-regex-input sample-text))
             (underlying-tokenizer (tokenizer:create-source-backed-tokenizer tokenizer-core-dfa input))
             (bt-tokenizer (bt-tokenizer:create-backtracking-tokenizer underlying-tokenizer)))
        (fiveam:is (equal (parsex-cl/rdp/parser:parse-construct root-grammar-constr bt-tokenizer nil)
                          :ok))))))
