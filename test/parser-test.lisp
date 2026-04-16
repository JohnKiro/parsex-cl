(in-package :parsex-cl.test/parser.test)

(fiveam:def-suite :parsex-cl/parser.test-suite
  :description "Tests the backtracking recursive descent parser"
  :in :parsex-cl.test-suite)

(fiveam:in-suite :parsex-cl/parser.test-suite)

(setf fiveam:*on-failure* nil)
(setf fiveam:*on-error* :debug)

(defun sample-parser-notif-callback-factory (input-source)
  "Create a parser callback closure, that accumulates a log entry for each parsed construct (1st
arg), and the parsing status (2nd arg). Also for token constructs, the log entry includes the token's
text. The `input-source` argument is used to retrieve the token's accumulated value (text).
Note: when the closure is called with the first arg as NIL, it doesn't append any log entries, but
rather, returns the accumulated log entries. This is how the the log is retrieved for testing and
debugging."
  (let ((parsing-log))
    (lambda (construct-obj parsing-status)
      (if construct-obj ; append log entry or dump log?
          (let ((log `(,construct-obj
                       ,parsing-status
                       ,@(when (eq (class-name (class-of construct-obj)) 'constr::token-construct)
                           (input:retrieve-last-accumulated-value input-source)))))
            (and (push log parsing-log) nil))
          (nreverse parsing-log)))))

(fiveam:test parser-smoke-test
  "Basic test that demonstrates parser usage in client code, and provides quick verification for a simple
grammar."
  (declare (optimize (debug 3) (speed 0)))
  (let* ((sample-grammar
           '((token id (seq
                        #1=(or (char-range #\A #\Z) (char-range #\a #\z))
                        (+ (or #1# (char-range #\0 #\9)))))
             (token int (+ (or #1# (char-range #\0 #\9))))
             (token *-op #\*)
             (token assign #\=)
             (token semicolon #\;)
             (token end-of-text "") ;;TODO: HANDLE!!!
             (rule factor (or id int))
             (rule mul-expr (seq factor (? (seq *-op factor))))
             (rule statement (seq id assign mul-expr semicolon))
             (rule statement-block (seq statement (* statement)))
             (rule root (seq statement-block end-of-text))))
         (*print-case* :downcase)
         (parsex-cl/rdp/parser:*parse-execution-count* 0)
         (sample-text (concatenate 'string
                                   "id1=id2*3;"
                                   "id11=id22*33;")))
    (multiple-value-bind (root-grammar-constr tokenizer-core-dfa _)
        (parsex-cl/rdp/grammar/sexp:parse-grammar sample-grammar 'root)
      (declare (ignorable _))
      (let* ((input (input:create-basic-regex-input sample-text))
             (underlying-tokenizer (tokenizer:create-source-backed-tokenizer tokenizer-core-dfa input))
             (bt-tokenizer (bt-tokenizer:create-backtracking-tokenizer underlying-tokenizer input))
             (sample-parser-notif-callback (sample-parser-notif-callback-factory input)))
        (fiveam:is (equal (parsex-cl/rdp/parser:parse-construct root-grammar-constr bt-tokenizer
                                                                sample-parser-notif-callback)
                          :ok))
        ;; call with NIL arg, just to get final parsing log (for eye-inspection for now)
        (print (funcall sample-parser-notif-callback nil nil))))))

(fiveam:test parser-smoke-test-2
  "Basic test that demonstrates parser usage in client code, and provides quick verification for a simple
grammar. This one is identical to previous one, except that it uses one-or-more instead of seq +
zero-or-more (to test that construct as well)."
  (declare (optimize (debug 3) (speed 0)))
  (let* ((sample-grammar
           '((token id (seq
                        #1=(or (char-range #\A #\Z) (char-range #\a #\z))
                        (+ (or #1# (char-range #\0 #\9)))))
             (token int (+ (or #1# (char-range #\0 #\9))))
             (token *-op #\*)
             (token assign #\=)
             (token semicolon #\;)
             (token end-of-text "") ;;TODO: HANDLE!!!
             (rule factor (or id int))
             (rule mul-expr (seq factor (? (seq *-op factor))))
             (rule statement (seq id assign mul-expr semicolon))
             (rule statement-block (+ statement))
             (rule root (seq statement-block end-of-text))))
         (*print-case* :downcase)
         (parsex-cl/rdp/parser:*parse-execution-count* 0)
         (sample-text (concatenate 'string
                                   "id1=id2*3;"
                                   "id11=id22*33;")))
    (multiple-value-bind (root-grammar-constr tokenizer-core-dfa _)
        (parsex-cl/rdp/grammar/sexp:parse-grammar sample-grammar 'root)
      (declare (ignorable _))
      (let* ((input (input:create-basic-regex-input sample-text))
             (underlying-tokenizer (tokenizer:create-source-backed-tokenizer tokenizer-core-dfa input))
             (bt-tokenizer (bt-tokenizer:create-backtracking-tokenizer underlying-tokenizer input))
             (sample-parser-notif-callback (sample-parser-notif-callback-factory input)))
        (fiveam:is (equal (parsex-cl/rdp/parser:parse-construct root-grammar-constr bt-tokenizer
                                                                sample-parser-notif-callback)
                          :ok))
        ;; call with NIL arg, just to get final parsing log (for eye-inspection for now)
        (print (funcall sample-parser-notif-callback nil nil))))))

(fiveam:test parser-smoke-test-3
  "Basic test that demonstrates parser usage in client code, and provides quick verification."
  (declare (optimize (debug 3) (speed 0)))
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
             (bt-tokenizer (bt-tokenizer:create-backtracking-tokenizer underlying-tokenizer input))
             (sample-parser-notif-callback (sample-parser-notif-callback-factory input)))
        (fiveam:is (equal (parsex-cl/rdp/parser:parse-construct root-grammar-constr bt-tokenizer
                                                                sample-parser-notif-callback)
                          :ok))
        ;; call with NIL arg, just to get final parsing log (for eye-inspection for now)
        (print (funcall sample-parser-notif-callback nil nil))))))
