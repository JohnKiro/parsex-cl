(in-package :parsex-cl.test/parser.test)

(fiveam:def-suite :parsex-cl/parser.test-suite
  :description "Tests the backtracking recursive descent parser"
  :in :parsex-cl.test-suite)

(fiveam:in-suite :parsex-cl/parser.test-suite)

(setf fiveam:*on-failure* nil)
(setf fiveam:*on-error* :debug)

(defparameter *dump-parsing-log* nil "Flag to control dumping the generated parsing log during tests.")

(defun sample-parser-notif-callback-factory (input-source)
  "Create a parser callback closure, that accumulates a log entry for each parsed construct (1st
arg), and the parsing status (2nd arg). Also for token constructs, the log entry includes the token's
text. The `input-source` argument is used to retrieve the token's accumulated value (text).
Note: when the closure is called with the first arg as NIL, it doesn't append any log entries, but
rather, returns the accumulated log entries. This is how the the log is retrieved for testing and
debugging."
  (let ((parsing-log))
    (lambda (construct-obj parsing-status maybe-tokenization-result)
      (if construct-obj ; append log entry or dump log?
          (let ((log `(,construct-obj
                       ,parsing-status
                       ,@(when (eq (class-name (class-of construct-obj)) 'constr:token-construct)
                           (when maybe-tokenization-result
                             (input:retrieve-subrange input-source (cdr maybe-tokenization-result)))))))
            (push log parsing-log)
            nil)
          (nreverse parsing-log)))))

(defun %prepare-test-data-from-log-entry (parsing-log-entry)
  (destructuring-bind (construct-obj status . maybe-token-text) parsing-log-entry
    `(,(class-name (class-of construct-obj))
      ,(constr:construct-id construct-obj)
      ,status
      ,@(unless (null maybe-token-text)
          `(,maybe-token-text)))))

(defun %prepare-test-data-from-log (parsing-log)
  "Prepares testing result based on previous execution (to avoid manual preparation, as long as
a previous execution's result is validated by visual inspection. The `parsing-log` is a list of entries
in the form (construct-obj status token-text)."
  (mapcar #'%prepare-test-data-from-log-entry parsing-log))

(defun check-log-entry (log-entry expected-parsing-output-entry)
  (destructuring-bind (construct-obj status . maybe-token-text) log-entry
    (destructuring-bind (expected-construct-classname expected-constr-id expected-status
                         &optional expected-token-text)
        expected-parsing-output-entry
      (fiveam:is (eq (class-name (class-of construct-obj)) expected-construct-classname))
      (fiveam:is (eq (constr:construct-id construct-obj) expected-constr-id))
      (fiveam:is (eq status expected-status))
      (fiveam:is (equal maybe-token-text expected-token-text)))))

(defun check-log (parsing-log expected-parsing-log)
  "Checks parsing log against expected parsing log. For the expected format, see `check-log-entry`, and
also refer to the included test cases for examples."
  (mapcar #'check-log-entry parsing-log expected-parsing-log))

(defun parser-test (&key grammar text (expected-final-parsing-status :ok) expected-parsing-result)
  "Prepares and executes parsing test, for a specific grammar `grammar` (in sexp form, for now), input
text `text`, and given optional expected parsing result `expected-parsing-result`, which serves to test
not only the final parsing status, but the progress of parsing (sequence of constructs, expected status
for exch, and tokenized text for each token). When not provided (or provided as NIL), the only test done
is that the final parsing result is :ok."
  (let ((*print-case* :downcase)
        (parsex-cl/rdp/parser:*parse-execution-count* 0))
    (multiple-value-bind (root-grammar-constr tokenizer-core-dfa _)
        (parsex-cl/rdp/grammar/sexp:parse-grammar grammar 'root)
      (declare (ignorable _))
      (let* ((input (input:create-basic-regex-input text))
             (underlying-tokenizer (tokenizer:create-source-backed-tokenizer tokenizer-core-dfa input))
             (bt-tokenizer (bt-tokenizer:create-backtracking-tokenizer underlying-tokenizer input))
             (sample-parser-notif-callback (sample-parser-notif-callback-factory input)))
        (fiveam:is (equal (parsex-cl/rdp/parser:parse-construct root-grammar-constr bt-tokenizer
                                                                #'parsex-cl/rdp/parser::token-matches-p
                                                                sample-parser-notif-callback)
                          expected-final-parsing-status))
        ;; call with NIL arg, just to get final parsing log
        (let ((parsing-log (funcall sample-parser-notif-callback nil nil nil)))
          ;; dumps log, for visual inspection, and then could be fed back subsequently into the
          ;; expected-parsing-result parameter. The idea is that after first visual inspection, it serves
          ;; in subsequent (automated) regression tests.
          (when *dump-parsing-log*
            (print (%prepare-test-data-from-log parsing-log))
            (terpri))
          (when expected-parsing-result
            (check-log parsing-log expected-parsing-result)))))))

(fiveam:test parser-test
  "Basic test that demonstrates parser usage in client code, and provides quick verification for a simple
grammar."
  (declare (optimize (debug 3) (speed 0)))
  (parser-test :grammar '((token id (seq
                                     #1=(or (char-range #\A #\Z) (char-range #\a #\z))
                                     (+ (or #1# (char-range #\0 #\9)))))
                          (token int (+ (or #1# (char-range #\0 #\9))))
                          (token *-op #\*)
                          (token assign #\=)
                          (token semicolon #\;)
                          (token eot "") ;;TODO: HANDLE!!!
                          (rule factor (or id int))
                          (rule mul-expr (seq factor (? (seq *-op factor))))
                          (rule statement (seq id assign mul-expr semicolon))
                          (rule statement-block (seq statement (* statement)))
                          (rule root (seq statement-block eot)))
               :text (concatenate 'string
                                  "id1=id2*3;"
                                  "id11=id22*33;")
               :expected-parsing-result '((constr:token-construct id :ok "id1")
                                          (constr:token-construct assign :ok "=")
                                          (constr:token-construct id :ok "id2")
                                          (constr:or-construct factor :ok)
                                          (constr:token-construct *-op :ok "*")
                                          (constr:token-construct id :no-match "3")
                                          (constr:token-construct int :ok "3")
                                          (constr:or-construct factor :ok)
                                          (constr:sequence-construct nil :ok)
                                          (constr:zero-or-one-construct nil :ok)
                                          (constr:sequence-construct mul-expr :ok)
                                          (constr:token-construct semicolon :ok ";")
                                          (constr:sequence-construct statement :ok)
                                          (constr:token-construct id :ok "id11")
                                          (constr:token-construct assign :ok "=")
                                          (constr:token-construct id :ok "id22")
                                          (constr:or-construct factor :ok)
                                          (constr:token-construct *-op :ok "*")
                                          (constr:token-construct id :no-match "33")
                                          (constr:token-construct int :ok "33")
                                          (constr:or-construct factor :ok)
                                          (constr:sequence-construct nil :ok)
                                          (constr:zero-or-one-construct nil :ok)
                                          (constr:sequence-construct mul-expr :ok)
                                          (constr:token-construct semicolon :ok ";")
                                          (constr:sequence-construct statement :ok)
                                          (constr:token-construct id :no-match "")
                                          (constr:sequence-construct statement :no-match)
                                          (constr:zero-or-more-construct nil :ok)
                                          (constr:sequence-construct statement-block :ok)
                                          (constr:token-construct eot :ok "")
                                          (constr:sequence-construct root :ok))))

(fiveam:test parser-test-2
  "Basic test that demonstrates parser usage in client code, and provides quick verification for a simple
grammar. This one is identical to previous one, except that it uses one-or-more instead of seq +
zero-or-more (to test that construct as well)."
  (declare (optimize (debug 3) (speed 0)))
  (parser-test :grammar '((token id (seq
                                     #1=(or (char-range #\A #\Z) (char-range #\a #\z))
                                     (+ (or #1# (char-range #\0 #\9)))))
                          (token int (+ (or #1# (char-range #\0 #\9))))
                          (token *-op #\*)
                          (token assign #\=)
                          (token semicolon #\;)
                          (token eot "") ;;TODO: HANDLE!!!
                          (rule factor (or id int))
                          (rule mul-expr (seq factor (? (seq *-op factor))))
                          (rule statement (seq id assign mul-expr semicolon))
                          (rule statement-block (+ statement))
                          (rule root (seq statement-block eot)))
               :text (concatenate 'string
                                  "id1=id2*3;"
                                  "id11=id22*33;")
               :expected-parsing-result '((constr:token-construct id :ok "id1")
                                          (constr:token-construct assign :ok "=")
                                          (constr:token-construct id :ok "id2")
                                          (constr:or-construct factor :ok)
                                          (constr:token-construct *-op :ok "*")
                                          (constr:token-construct id :no-match "3")
                                          (constr:token-construct int :ok "3")
                                          (constr:or-construct factor :ok)
                                          (constr:sequence-construct nil :ok)
                                          (constr:zero-or-one-construct nil :ok)
                                          (constr:sequence-construct mul-expr :ok)
                                          (constr:token-construct semicolon :ok ";")
                                          (constr:sequence-construct statement :ok)
                                          (constr:token-construct id :ok "id11")
                                          (constr:token-construct assign :ok "=")
                                          (constr:token-construct id :ok "id22")
                                          (constr:or-construct factor :ok)
                                          (constr:token-construct *-op :ok "*")
                                          (constr:token-construct id :no-match "33")
                                          (constr:token-construct int :ok "33")
                                          (constr:or-construct factor :ok)
                                          (constr:sequence-construct nil :ok)
                                          (constr:zero-or-one-construct nil :ok)
                                          (constr:sequence-construct mul-expr :ok)
                                          (constr:token-construct semicolon :ok ";")
                                          (constr:sequence-construct statement :ok)
                                          (constr:token-construct id :no-match "")
                                          (constr:sequence-construct statement :no-match)
                                          (constr:one-or-more-construct statement-block :ok)
                                          (constr:token-construct eot :ok "")
                                          (constr:sequence-construct root :ok))))

(fiveam:test parser-test-3
  "Basic test that demonstrates parser usage in client code, this time with more complicated grammar and
input text."
  (declare (optimize (debug 3) (speed 0)))
  (parser-test :grammar '((token id (seq
                                     #1=(or (char-range #\A #\Z) (char-range #\a #\z))
                                     (+ (or #1# (char-range #\0 #\9)))))
                          (token int (+ (or #1# (char-range #\0 #\9))))
                          (token l-paren #\()
                          (token r-paren #\))
                          (token *-op #\*)
                          (token +-op #\+)
                          (token assign #\=)
                          (token semicolon #\;)
                          (token eot "") ;;TODO: HANDLE!!!
                          (rule factor (or id int (seq l-paren add-expr r-paren)))
                          (rule mul-expr (seq factor (? (seq *-op factor))))
                          (rule add-expr (seq mul-expr (? (seq +-op mul-expr))))
                          (rule statement (seq id assign add-expr semicolon))
                          (rule statement-block (seq statement (* statement)))
                          (rule root (seq statement-block eot)))
               :text (concatenate 'string
                                  "id1=(id2*id3);"
                                  "id1=id2*(id+11);"
                                  "id1=(id2+id3);")
               :expected-parsing-result '((constr:token-construct id :ok "id1")
                                          (constr:token-construct assign :ok "=")
                                          (constr:token-construct id :no-match "(")
                                          (constr:token-construct int :no-match "(")
                                          (constr:token-construct l-paren :ok "(")
                                          (constr:token-construct id :ok "id2")
                                          (constr:or-construct factor :ok)
                                          (constr:token-construct *-op :ok "*")
                                          (constr:token-construct id :ok "id3")
                                          (constr:or-construct factor :ok)
                                          (constr:sequence-construct nil :ok)
                                          (constr:zero-or-one-construct nil :ok)
                                          (constr:sequence-construct mul-expr :ok)
                                          (constr:token-construct +-op :no-match ")")
                                          (constr:sequence-construct nil :no-match)
                                          (constr:zero-or-one-construct nil :ok)
                                          (constr:sequence-construct add-expr :ok)
                                          (constr:token-construct r-paren :ok ")")
                                          (constr:sequence-construct nil :ok)
                                          (constr:or-construct factor :ok)
                                          (constr:token-construct *-op :no-match ";")
                                          (constr:sequence-construct nil :no-match)
                                          (constr:zero-or-one-construct nil :ok)
                                          (constr:sequence-construct mul-expr :ok)
                                          (constr:token-construct +-op :no-match ";")
                                          (constr:sequence-construct nil :no-match)
                                          (constr:zero-or-one-construct nil :ok)
                                          (constr:sequence-construct add-expr :ok)
                                          (constr:token-construct semicolon :ok ";")
                                          (constr:sequence-construct statement :ok)
                                          (constr:token-construct id :ok "id1")
                                          (constr:token-construct assign :ok "=")
                                          (constr:token-construct id :ok "id2")
                                          (constr:or-construct factor :ok)
                                          (constr:token-construct *-op :ok "*")
                                          (constr:token-construct id :no-match "(")
                                          (constr:token-construct int :no-match "(")
                                          (constr:token-construct l-paren :ok "(")
                                          (constr:token-construct id :ok "id")
                                          (constr:or-construct factor :ok)
                                          (constr:token-construct *-op :no-match "+")
                                          (constr:sequence-construct nil :no-match)
                                          (constr:zero-or-one-construct nil :ok)
                                          (constr:sequence-construct mul-expr :ok)
                                          (constr:token-construct +-op :ok "+")
                                          (constr:token-construct id :no-match "11")
                                          (constr:token-construct int :ok "11")
                                          (constr:or-construct factor :ok)
                                          (constr:token-construct *-op :no-match ")")
                                          (constr:sequence-construct nil :no-match)
                                          (constr:zero-or-one-construct nil :ok)
                                          (constr:sequence-construct mul-expr :ok)
                                          (constr:sequence-construct nil :ok)
                                          (constr:zero-or-one-construct nil :ok)
                                          (constr:sequence-construct add-expr :ok)
                                          (constr:token-construct r-paren :ok ")")
                                          (constr:sequence-construct nil :ok)
                                          (constr:or-construct factor :ok)
                                          (constr:sequence-construct nil :ok)
                                          (constr:zero-or-one-construct nil :ok)
                                          (constr:sequence-construct mul-expr :ok)
                                          (constr:token-construct +-op :no-match ";")
                                          (constr:sequence-construct nil :no-match)
                                          (constr:zero-or-one-construct nil :ok)
                                          (constr:sequence-construct add-expr :ok)
                                          (constr:token-construct semicolon :ok ";")
                                          (constr:sequence-construct statement :ok)
                                          (constr:token-construct id :ok "id1")
                                          (constr:token-construct assign :ok "=")
                                          (constr:token-construct id :no-match "(")
                                          (constr:token-construct int :no-match "(")
                                          (constr:token-construct l-paren :ok "(")
                                          (constr:token-construct id :ok "id2")
                                          (constr:or-construct factor :ok)
                                          (constr:token-construct *-op :no-match "+")
                                          (constr:sequence-construct nil :no-match)
                                          (constr:zero-or-one-construct nil :ok)
                                          (constr:sequence-construct mul-expr :ok)
                                          (constr:token-construct +-op :ok "+")
                                          (constr:token-construct id :ok "id3")
                                          (constr:or-construct factor :ok)
                                          (constr:token-construct *-op :no-match ")")
                                          (constr:sequence-construct nil :no-match)
                                          (constr:zero-or-one-construct nil :ok)
                                          (constr:sequence-construct mul-expr :ok)
                                          (constr:sequence-construct nil :ok)
                                          (constr:zero-or-one-construct nil :ok)
                                          (constr:sequence-construct add-expr :ok)
                                          (constr:token-construct r-paren :ok ")")
                                          (constr:sequence-construct nil :ok)
                                          (constr:or-construct factor :ok)
                                          (constr:token-construct *-op :no-match ";")
                                          (constr:sequence-construct nil :no-match)
                                          (constr:zero-or-one-construct nil :ok)
                                          (constr:sequence-construct mul-expr :ok)
                                          (constr:token-construct +-op :no-match ";")
                                          (constr:sequence-construct nil :no-match)
                                          (constr:zero-or-one-construct nil :ok)
                                          (constr:sequence-construct add-expr :ok)
                                          (constr:token-construct semicolon :ok ";")
                                          (constr:sequence-construct statement :ok)
                                          (constr:token-construct id :no-match "")
                                          (constr:sequence-construct statement :no-match)
                                          (constr:zero-or-more-construct nil :ok)
                                          (constr:sequence-construct statement-block :ok)
                                          (constr:token-construct eot :ok "")
                                          (constr:sequence-construct root :ok))))

(fiveam:test parser-test-4
  "Test parsing error (unexpected token in factor, assign found instead of factor)."
  (declare (optimize (debug 3) (speed 0)))
  (parser-test :grammar '((token id (seq
                                     #1=(or (char-range #\A #\Z) (char-range #\a #\z))
                                     (+ (or #1# (char-range #\0 #\9)))))
                          (token int (+ (or #1# (char-range #\0 #\9))))
                          (token *-op #\*)
                          (token assign #\=)
                          (token semicolon #\;)
                          (token eot "") ;;TODO: HANDLE!!!
                          (rule factor (or id int))
                          (rule mul-expr (seq factor (? (seq *-op factor))))
                          (rule statement (seq id assign mul-expr semicolon))
                          (rule statement-block (+ statement))
                          (rule root (seq statement-block eot)))
               :text (concatenate 'string
                                  "id1=id2*3;"
                                  "id11=id22*=;")
               :expected-final-parsing-status :no-match
               :expected-parsing-result '((constr:token-construct id :ok "id1")
                                          (constr:token-construct assign :ok "=")
                                          (constr:token-construct id :ok "id2")
                                          (constr:or-construct factor :ok)
                                          (constr:token-construct *-op :ok "*")
                                          (constr:token-construct id :no-match "3")
                                          (constr:token-construct int :ok "3")
                                          (constr:or-construct factor :ok)
                                          (constr:sequence-construct nil :ok)
                                          (constr:zero-or-one-construct nil :ok)
                                          (constr:sequence-construct mul-expr :ok)
                                          (constr:token-construct semicolon :ok ";")
                                          (constr:sequence-construct statement :ok)
                                          (constr:token-construct id :ok "id11")
                                          (constr:token-construct assign :ok "=")
                                          (constr:token-construct id :ok "id22")
                                          (constr:or-construct factor :ok)
                                          (constr:token-construct *-op :ok "*")
                                          (constr:token-construct id :no-match "=")
                                          (constr:token-construct int :no-match "=")
                                          (constr:or-construct factor :no-match)
                                          (constr:sequence-construct nil :no-match)
                                          (constr:zero-or-one-construct nil :ok)
                                          (constr:sequence-construct mul-expr :ok)
                                          (constr:token-construct semicolon :no-match "*")
                                          (constr:sequence-construct statement :no-match)
                                          (constr:one-or-more-construct statement-block :ok)
                                          (constr:token-construct eot :no-match "id11")
                                          (constr:sequence-construct root :no-match))))

(fiveam:test parser-test-5
  "Test parsing error (unexpected token in factor, as if factor is missing)."
  (declare (optimize (debug 3) (speed 0)))
  (parser-test :grammar '((token id (seq
                                     #1=(or (char-range #\A #\Z) (char-range #\a #\z))
                                     (+ (or #1# (char-range #\0 #\9)))))
                          (token int (+ (or #1# (char-range #\0 #\9))))
                          (token *-op #\*)
                          (token assign #\=)
                          (token semicolon #\;)
                          (token eot "") ;;TODO: HANDLE!!!
                          (rule factor (or id int))
                          (rule mul-expr (seq factor (? (seq *-op factor))))
                          (rule statement (seq id assign mul-expr semicolon))
                          (rule statement-block (+ statement))
                          (rule root (seq statement-block eot)))
               :text (concatenate 'string
                                  "id1=id2*3;"
                                  "id11=id22*;")
               :expected-final-parsing-status :no-match
               :expected-parsing-result '((constr:token-construct id :ok "id1")
                                          (constr:token-construct assign :ok "=")
                                          (constr:token-construct id :ok "id2")
                                          (constr:or-construct factor :ok)
                                          (constr:token-construct *-op :ok "*")
                                          (constr:token-construct id :no-match "3")
                                          (constr:token-construct int :ok "3")
                                          (constr:or-construct factor :ok)
                                          (constr:sequence-construct nil :ok)
                                          (constr:zero-or-one-construct nil :ok)
                                          (constr:sequence-construct mul-expr :ok)
                                          (constr:token-construct semicolon :ok ";")
                                          (constr:sequence-construct statement :ok)
                                          (constr:token-construct id :ok "id11")
                                          (constr:token-construct assign :ok "=")
                                          (constr:token-construct id :ok "id22")
                                          (constr:or-construct factor :ok)
                                          (constr:token-construct *-op :ok "*")
                                          ;; expecting factor (id/int), got semicolon
                                          (constr:token-construct id :no-match ";")
                                          (constr:token-construct int :no-match ";")
                                          (constr:or-construct factor :no-match)
                                          ;; *-op factor fails
                                          (constr:sequence-construct nil :no-match)
                                          ;; (? (seq *-op factor)) succeeds (since optional)
                                          ;; rewinding ("*" not consumed)
                                          (constr:zero-or-one-construct nil :ok)
                                          (constr:sequence-construct mul-expr :ok)
                                          ;; expecting statement termination
                                          (constr:token-construct semicolon :no-match "*")
                                          (constr:sequence-construct statement :no-match)
                                          ;; at least one statement succeeded, we're now back to point
                                          ;; just after that statement ("id11")
                                          (constr:one-or-more-construct statement-block :ok)
                                          (constr:token-construct eot :no-match "id11")
                                          (constr:sequence-construct root :no-match))))

(fiveam:test parser-test-6
  "Test parsing error: investigating skipping error and finding a continuation point: currently, parsing
stops exactly at same point as previous TC (#5), so we get exactly same expected result. This is because
the error propagates upwards till root, and gets reported. There is no current mechanism for
continuation."
  (declare (optimize (debug 3) (speed 0)))
  (parser-test :grammar '((token id (seq
                                     #1=(or (char-range #\A #\Z) (char-range #\a #\z))
                                     (+ (or #1# (char-range #\0 #\9)))))
                          (token int (+ (or #1# (char-range #\0 #\9))))
                          (token *-op #\*)
                          (token assign #\=)
                          (token semicolon #\;)
                          (token eot "") ;;TODO: HANDLE!!!
                          (rule factor (or id int))
                          (rule mul-expr (seq factor (? (seq *-op factor))))
                          (rule statement (seq id assign mul-expr semicolon))
                          (rule statement-block (+ statement))
                          (rule root (seq statement-block eot)))
               :text (concatenate 'string
                                  "id1=id2*3;"
                                  "id11=id22*;" ; erroneous line, causes parser to abort
                                  "id111=id222*333;")
               :expected-final-parsing-status :no-match
               :expected-parsing-result '((constr:token-construct id :ok "id1")
                                          (constr:token-construct assign :ok "=")
                                          (constr:token-construct id :ok "id2")
                                          (constr:or-construct factor :ok)
                                          (constr:token-construct *-op :ok "*")
                                          (constr:token-construct id :no-match "3")
                                          (constr:token-construct int :ok "3")
                                          (constr:or-construct factor :ok)
                                          (constr:sequence-construct nil :ok)
                                          (constr:zero-or-one-construct nil :ok)
                                          (constr:sequence-construct mul-expr :ok)
                                          (constr:token-construct semicolon :ok ";")
                                          (constr:sequence-construct statement :ok)
                                          (constr:token-construct id :ok "id11")
                                          (constr:token-construct assign :ok "=")
                                          (constr:token-construct id :ok "id22")
                                          (constr:or-construct factor :ok)
                                          (constr:token-construct *-op :ok "*")
                                          ;; expecting factor (id/int), got semicolon
                                          (constr:token-construct id :no-match ";")
                                          (constr:token-construct int :no-match ";")
                                          (constr:or-construct factor :no-match)
                                          ;; *-op factor fails
                                          (constr:sequence-construct nil :no-match)
                                          ;; (? (seq *-op factor)) succeeds (since optional)
                                          ;; rewinding ("*" not consumed)
                                          (constr:zero-or-one-construct nil :ok)
                                          (constr:sequence-construct mul-expr :ok)
                                          ;; expecting statement termination
                                          (constr:token-construct semicolon :no-match "*")
                                          (constr:sequence-construct statement :no-match)
                                          ;; at least one statement succeeded, we're now back to point
                                          ;; just after that statement ("id11")
                                          (constr:one-or-more-construct statement-block :ok)
                                          (constr:token-construct eot :no-match "id11")
                                          (constr:sequence-construct root :no-match))))
