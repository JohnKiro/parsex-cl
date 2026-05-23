(in-package :parsex-cl.test/parser.test)

(fiveam:def-suite :parsex-cl/parser.test-suite
  :description "Tests the backtracking recursive descent parser"
  :in :parsex-cl.test-suite)

(fiveam:in-suite :parsex-cl/parser.test-suite)

(setf fiveam:*on-failure* nil)
(setf fiveam:*on-error* :debug)

(defparameter *dump-parsing-log* nil "Flag to control dumping the generated parsing log during tests.")
(defparameter *parsing-error-registry* nil
  "This is where I'll record parsing errors, for visual inspection while analyzing and debugging.")

(defmacro add-parsing-error-entry (error-log construct-obj status tokenization-result
                                   skipped-tokenizations-log)
  `(push `(:construct ,,construct-obj :status ,,status :tokenization-result ,,tokenization-result
           :skipped-tokenizations-log ,,skipped-tokenizations-log)
         ,error-log))

(defun sample-parser-notif-callback-factory (input-source)
  "Create a parser callback closure, that accumulates a log entry for each parsed construct (1st
arg), the parsing status (2nd arg), the token's text (for token constructs). In case of error, it also
accumulates an error log entry that includes the construct, status, last tokenization result, skipped
tokenizations (if any).
The `input-source` argument is used to retrieve the token's accumulated value (text).
Note: when the closure is called with the first arg as NIL, it doesn't append any log entries, but
rather, returns two values, including the accumulated log entries. This is how the logs are retrieved
for testing and debugging."
  (let (parsing-log error-log)
    (lambda (construct-obj parsing-status maybe-tokenization-result
             &optional maybe-skipped-tokenizations-results)
      (if construct-obj ; append log entry or dump log?
          (progn
            (let ((log `(,construct-obj
                         ,parsing-status
                         ,@(when maybe-tokenization-result
                             (input:retrieve-subrange input-source (cdr maybe-tokenization-result))))))
              (push log parsing-log)
              nil)
            (when (or (eq parsing-status :partial-failure)
                      maybe-skipped-tokenizations-results)
              (add-parsing-error-entry error-log
                                       construct-obj
                                       parsing-status
                                       maybe-tokenization-result
                                       maybe-skipped-tokenizations-results)))
          (values (nreverse parsing-log) (nreverse error-log))))))

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
                         &optional (expected-token-text nil expected-token-text-supplied))
        expected-parsing-output-entry
      (fiveam:is (eq (class-name (class-of construct-obj)) expected-construct-classname))
      (fiveam:is (eq (constr:construct-id construct-obj) expected-constr-id))
      (fiveam:is (eq status expected-status))
      (when expected-token-text-supplied
        (fiveam:is (equal maybe-token-text expected-token-text))))))

(defun check-log (parsing-log expected-parsing-log)
  "Checks parsing log against expected parsing log. For the expected format, see `check-log-entry`, and
also refer to the included test cases for examples."
  (mapcar #'check-log-entry parsing-log expected-parsing-log))

(defun parser-test (&key grammar text (expected-final-parsing-status :ok) expected-parsing-result
                      (check-sync-tokens nil) (grammar-start-rule 'root) (resilience nil)
                      (seq-abort-on-first-failure t))
  "Prepares and executes parsing test, for a specific grammar `grammar` (in sexp form, for now), input
text `text`, and given optional expected parsing result `expected-parsing-result`, which serves to test
not only the final parsing status, but the progress of parsing (sequence of constructs, expected status
for exch, and tokenized text for each token). When not provided (or provided as NIL), the only test done
is that the final parsing result is :ok."
  (let ((*print-case* :downcase))
    (multiple-value-bind (root-grammar-constr tokenizer-core-dfa _)
        (parsex-cl/rdp/grammar/sexp:parse-grammar grammar grammar-start-rule)
      (declare (ignorable _))
      (let* ((input (input:create-basic-regex-input text))
             (underlying-tokenizer (tokenizer:create-source-backed-tokenizer tokenizer-core-dfa input))
             (bt-tokenizer (bt-tokenizer:create-backtracking-tokenizer underlying-tokenizer input))
             (sample-parser-notif-callback (sample-parser-notif-callback-factory input)))
        (fiveam:is (equal (parsex-cl/rdp/parser::parse-root root-grammar-constr bt-tokenizer
                                                            #'parsex-cl/rdp/parser::token-matches-p
                                                            sample-parser-notif-callback
                                                            :resilience resilience
                                                            :seq-abort-on-first-failure seq-abort-on-first-failure
                                                            :check-sync-tokens check-sync-tokens)
                          expected-final-parsing-status))
        ;; call with NIL arg, just to get final parsing log
        (multiple-value-bind (parsing-log error-log) (funcall sample-parser-notif-callback nil nil nil)
          ;; dumps log, for visual inspection, and then could be fed back subsequently into the
          ;; expected-parsing-result parameter. The idea is that after first visual inspection, it serves
          ;; in subsequent (automated) regression tests. The error log is also dumped for inspection.
          (when *dump-parsing-log*
            (terpri)
            (princ "Parsing log:")
            (print (%prepare-test-data-from-log parsing-log))
            (terpri)
            (princ "Error log:")
            (print error-log)
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
                          (token int (+ (char-range #\0 #\9)))
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
                                          ;; since statement seq had zero progress, => complete failure
                                          (constr:sequence-construct statement :complete-failure)
                                          (constr:zero-or-more-construct nil :ok)
                                          (constr:sequence-construct statement-block :ok)
                                          (constr:token-construct eot :ok "")
                                          (constr:sequence-construct root :ok))))

(fiveam:test parser-test_1
  "Basic test, similar to previous, but with 'sequence abortion on failure' disabled."
  (declare (optimize (debug 3) (speed 0)))
  (let ()
    (parser-test :grammar '((token id (seq
                                       #1=(or (char-range #\A #\Z) (char-range #\a #\z))
                                       (+ (or #1# (char-range #\0 #\9)))))
                            (token int (+ (char-range #\0 #\9)))
                            (token *-op #\*)
                            (token assign #\=)
                            (token semicolon #\;)
                            (token eot "") ;;TODO: HANDLE!!!
                            (rule factor (or id int))
                            (rule mul-expr (seq factor (? (seq *-op factor))))
                            (rule statement (seq id assign mul-expr semicolon))
                            (rule statement-block (seq statement (* statement)))
                            (rule root (seq statement-block eot)))
                 :seq-abort-on-first-failure nil
                 :text (concatenate 'string
                                    "id1=id2*3;"
                                    "id11=id22*33;")
                 :expected-parsing-result
                 '((constr:token-construct id :ok "id1")
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
                   ;; rather than aborting here, statment seq proceeds with next child
                   ;; alas! next child (assign) gets empty input, due to "advance on no match" input flag
                   (constr:token-construct assign :input-exhausted)
                   (constr:token-construct id :input-exhausted)
                   (constr:token-construct int :input-exhausted)
                   (constr:or-construct factor :input-exhausted)
                   (constr:token-construct *-op :input-exhausted)
                   (constr:token-construct id :input-exhausted)
                   (constr:token-construct int :input-exhausted)
                   (constr:or-construct factor :input-exhausted)
                   (constr:sequence-construct nil :complete-failure)
                   (constr:zero-or-one-construct nil :ok)
                   (constr:sequence-construct mul-expr :partial-failure)
                   (constr:token-construct semicolon :input-exhausted)
                   ;; the zero-or-one above caused the report to be partial failure, not complete failure
                   (constr:sequence-construct statement :partial-failure)
                   ;; z-o-m handles the seq failure gracefully even though it's partial failure.
                   ;; normally should behave like this only on complete failure. I think I need two
                   ;; thinkgs: treat :input-exhausted specially (abort regardless of flag),
                   ;; and also may need to detect case of partial failure when the succeeded children
                   ;; did not consume any thing (such as the zero-or-one in this grammar)
                   (constr:zero-or-more-construct nil :ok)
                   (constr:sequence-construct statement-block :ok)
                   (constr:token-construct eot :ok "")
                   (constr:sequence-construct root :ok)))))

(fiveam:test parser-test-2
  "Basic test that demonstrates parser usage in client code, and provides quick verification for a simple
grammar. This one is identical to previous one, except that it uses one-or-more instead of seq +
zero-or-more (to test that construct as well)."
  (declare (optimize (debug 3) (speed 0)))
  (parser-test :grammar '((token id (seq
                                     #1=(or (char-range #\A #\Z) (char-range #\a #\z))
                                     (+ (or #1# (char-range #\0 #\9)))))
                          (token int (+ (char-range #\0 #\9)))
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
                                          (constr:sequence-construct statement :complete-failure)
                                          (constr:one-or-more-construct statement-block :ok)
                                          (constr:token-construct eot :ok "")
                                          (constr:sequence-construct root :ok))))

(fiveam:test parser-test-3
  "Basic test that demonstrates parser usage in client code, this time with more complicated grammar and
input text. Note how complete-failure of sequence should actually be treated gracefully, by most parent
types (e.g. zero-or-more), besides the OR parent of course, which only cares to find one successful
branch. Anyway, I'm still working on the 'harmony' between the different types of parents and childen."
  (declare (optimize (debug 3) (speed 0)))
  (parser-test :grammar '((token id (seq
                                     #1=(or (char-range #\A #\Z) (char-range #\a #\z))
                                     (+ (or #1# (char-range #\0 #\9)))))
                          (token int (+ (char-range #\0 #\9)))
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
                                          (constr:sequence-construct nil :complete-failure)
                                          (constr:zero-or-one-construct nil :ok)
                                          (constr:sequence-construct add-expr :ok)
                                          (constr:token-construct r-paren :ok ")")
                                          (constr:sequence-construct nil :ok)
                                          (constr:or-construct factor :ok)
                                          (constr:token-construct *-op :no-match ";")
                                          (constr:sequence-construct nil :complete-failure)
                                          (constr:zero-or-one-construct nil :ok)
                                          (constr:sequence-construct mul-expr :ok)
                                          (constr:token-construct +-op :no-match ";")
                                          (constr:sequence-construct nil :complete-failure)
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
                                          (constr:sequence-construct nil :complete-failure)
                                          (constr:zero-or-one-construct nil :ok)
                                          (constr:sequence-construct mul-expr :ok)
                                          (constr:token-construct +-op :ok "+")
                                          (constr:token-construct id :no-match "11")
                                          (constr:token-construct int :ok "11")
                                          (constr:or-construct factor :ok)
                                          (constr:token-construct *-op :no-match ")")
                                          (constr:sequence-construct nil :complete-failure)
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
                                          (constr:sequence-construct nil :complete-failure)
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
                                          (constr:sequence-construct nil :complete-failure)
                                          (constr:zero-or-one-construct nil :ok)
                                          (constr:sequence-construct mul-expr :ok)
                                          (constr:token-construct +-op :ok "+")
                                          (constr:token-construct id :ok "id3")
                                          (constr:or-construct factor :ok)
                                          (constr:token-construct *-op :no-match ")")
                                          (constr:sequence-construct nil :complete-failure)
                                          (constr:zero-or-one-construct nil :ok)
                                          (constr:sequence-construct mul-expr :ok)
                                          (constr:sequence-construct nil :ok)
                                          (constr:zero-or-one-construct nil :ok)
                                          (constr:sequence-construct add-expr :ok)
                                          (constr:token-construct r-paren :ok ")")
                                          (constr:sequence-construct nil :ok)
                                          (constr:or-construct factor :ok)
                                          (constr:token-construct *-op :no-match ";")
                                          (constr:sequence-construct nil :complete-failure)
                                          (constr:zero-or-one-construct nil :ok)
                                          (constr:sequence-construct mul-expr :ok)
                                          (constr:token-construct +-op :no-match ";")
                                          (constr:sequence-construct nil :complete-failure)
                                          (constr:zero-or-one-construct nil :ok)
                                          (constr:sequence-construct add-expr :ok)
                                          (constr:token-construct semicolon :ok ";")
                                          (constr:sequence-construct statement :ok)
                                          (constr:token-construct id :no-match "")
                                          (constr:sequence-construct statement :complete-failure)
                                          (constr:zero-or-more-construct nil :ok)
                                          (constr:sequence-construct statement-block :ok)
                                          (constr:token-construct eot :ok "")
                                          (constr:sequence-construct root :ok))))

(fiveam:test parser-test-4
  "Test parsing error (unexpected token in factor, assign found instead of factor): no token skipping,
and error reported upwards, where constructs such as zero-or-one and one-or-more will rewind and hence
overlook the failure, which would eventually resurge."
  (declare (optimize (debug 3) (speed 0)))
  (parser-test :grammar '((token id (seq
                                     #1=(or (char-range #\A #\Z) (char-range #\a #\z))
                                     (+ (or #1# (char-range #\0 #\9)))))
                          (token int (+ (char-range #\0 #\9)))
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
               :expected-final-parsing-status :partial-failure
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
                                          (constr:sequence-construct nil :partial-failure)
                                          (constr:zero-or-one-construct nil :ok)
                                          (constr:sequence-construct mul-expr :ok)
                                          (constr:token-construct semicolon :no-match "*")
                                          (constr:sequence-construct statement :partial-failure)
                                          (constr:one-or-more-construct statement-block :ok)
                                          (constr:token-construct eot :no-match "id11")
                                          (constr:sequence-construct root :partial-failure))))

(fiveam:test parser-test-4_2
  "Test parsing error (unexpected token in factor, assign found instead of factor): parser managed to
detect and record the error (in a separate log so far) and recover, by skipping tokens that are not found
in sync list."
  (declare (optimize (debug 3) (speed 0)))
  (parser-test :grammar '((token id (seq
                                     #1=(or (char-range #\A #\Z) (char-range #\a #\z))
                                     (+ (or #1# (char-range #\0 #\9)))))
                          (token int (+ (char-range #\0 #\9)))
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
               :expected-final-parsing-status :ok
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
                                          ;; '=' skipped
                                          (constr:token-construct id :no-match ";")
                                          (constr:token-construct int :no-match ";")
                                          (constr:or-construct factor :no-match)
                                          (constr:sequence-construct nil :partial-failure)
                                          (constr:zero-or-one-construct nil :ok)
                                          (constr:sequence-construct mul-expr :ok)
                                          ;; '*' and '=' skipped
                                          (constr:token-construct semicolon :ok ";")
                                          (constr:sequence-construct statement :ok ";")
                                          (constr:token-construct id :no-match "")
                                          (constr:sequence-construct statement :complete-failure "")
                                          (constr:one-or-more-construct statement-block :ok)
                                          (constr:token-construct eot :ok "")
                                          (constr:sequence-construct root :ok ""))
               :check-sync-tokens t))

(fiveam:test parser-test-5
  "Test parsing error (unexpected token in factor, as if factor is missing): no token skipping, and error
reported upwards, where constructs such as zero-or-one and one-or-more will rewind and hence overlook the
failure, which would eventually resurge."
  (declare (optimize (debug 3) (speed 0)))
  (parser-test :grammar '((token id (seq
                                     #1=(or (char-range #\A #\Z) (char-range #\a #\z))
                                     (+ (or #1# (char-range #\0 #\9)))))
                          (token int (+ (char-range #\0 #\9)))
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
               :expected-final-parsing-status :partial-failure
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
                                          (constr:sequence-construct nil :partial-failure)
                                          ;; (? (seq *-op factor)) succeeds (since optional)
                                          ;; rewinding ("*" not consumed)
                                          (constr:zero-or-one-construct nil :ok)
                                          (constr:sequence-construct mul-expr :ok)
                                          ;; expecting statement termination
                                          (constr:token-construct semicolon :no-match "*")
                                          (constr:sequence-construct statement :partial-failure)
                                          ;; at least one statement succeeded, we're now back to point
                                          ;; just after that statement ("id11")
                                          (constr:one-or-more-construct statement-block :ok)
                                          (constr:token-construct eot :no-match "id11")
                                          (constr:sequence-construct root :partial-failure))))

(fiveam:test parser-test-5_2
  "Test parsing error (unexpected token in factor, as if factor is missing): parser managed to detect and
record the error (in a separate log so far) and recover, by skipping tokens that are not found in sync
list."
  (declare (optimize (debug 3) (speed 0)))
  (parser-test :grammar '((token id (seq
                                     #1=(or (char-range #\A #\Z) (char-range #\a #\z))
                                     (+ (or #1# (char-range #\0 #\9)))))
                          (token int (+ (char-range #\0 #\9)))
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
               :expected-final-parsing-status :ok
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
                                          ;; expecting factor (id/int), got semicolon (which is in sync)
                                          (constr:token-construct id :no-match ";")
                                          (constr:token-construct int :no-match ";")
                                          (constr:or-construct factor :no-match)
                                          ;; *-op factor partially fails
                                          (constr:sequence-construct nil :partial-failure)
                                          ;; (? (seq *-op factor)) succeeds (since optional)
                                          ;; rewinding ("*" not consumed)
                                          (constr:zero-or-one-construct nil :ok)
                                          (constr:sequence-construct mul-expr :ok)
                                          ;; expecting statement termination, finding "*" (will skip
                                          ;; since not in sync list at this point)
                                          ;; TODO: may introduce :ok-but-had-to-skip
                                          (constr:token-construct semicolon :ok ";")
                                          (constr:sequence-construct statement :ok ";")
                                          ;; we recovered from the partial failure and moved forward,
                                          ;; next: trying to parse a new statement
                                          (constr:token-construct id :no-match "")
                                          (constr:sequence-construct statement :complete-failure "")
                                          (constr:one-or-more-construct statement-block :ok)
                                          (constr:token-construct eot :ok "")
                                          (constr:sequence-construct root :ok ""))
               :check-sync-tokens t))

(fiveam:test parser-test-6
  "Test parsing error: investigating skipping error and finding a continuation point: error recovery
takes place within the second statement, where the ';' construct skips '*' (not in sync list), and
successfully matches, then parsing proceeds successfully till end."
  (declare (optimize (debug 3) (speed 0)))
  (parser-test :grammar '((token id (seq
                                     #1=(or (char-range #\A #\Z) (char-range #\a #\z))
                                     (+ (or #1# (char-range #\0 #\9)))))
                          (token int (+ (char-range #\0 #\9)))
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
                                  "id11=id22*;" ; erroneous line, causes parser to skip statement
                                  "id111=id222*333;")
               :expected-final-parsing-status :ok
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
                                          (constr:sequence-construct nil :partial-failure)
                                          ;; (? (seq *-op factor)) succeeds (since optional)
                                          ;; rewinding ("*" not consumed)
                                          (constr:zero-or-one-construct nil :ok)
                                          (constr:sequence-construct mul-expr :ok)
                                          ;; expecting statement termination, skipping '*' and success
                                          (constr:token-construct semicolon :ok ";")
                                          (constr:sequence-construct statement :ok)
                                          ;; starting last statement (proceeding successfully till end)
                                          (constr:token-construct id :ok "id111")
                                          (constr:token-construct assign :ok "=")
                                          (constr:token-construct id :ok "id222")
                                          (constr:or-construct factor :ok "id222")
                                          (constr:token-construct *-op :ok "*")
                                          (constr:token-construct id :no-match "333")
                                          (constr:token-construct int :ok "333")
                                          (constr:or-construct factor :ok "333")
                                          (constr:sequence-construct nil :ok "333")
                                          (constr:zero-or-one-construct nil :ok "333")
                                          (constr:sequence-construct mul-expr :ok "333")
                                          (constr:token-construct semicolon :ok ";")
                                          (constr:sequence-construct statement :ok ";")
                                          (constr:token-construct id :no-match "")
                                          (constr:sequence-construct statement :complete-failure "")
                                          (constr:one-or-more-construct statement-block :ok "")
                                          (constr:token-construct eot :ok "")
                                          (constr:sequence-construct root :ok ""))
               :check-sync-tokens t))

;; Note that we don't need the resilience for sequence in this test (*seq-abort-on-first-failure*),
;; because the erroneous tokens are not found in the sync list, and hence skipped by the token construct
;; itself, so the upper SEQ is not made aware of them.
;; TODO: so far, the errors are 'hidden' within the separate error log (not checked in the test yet)
(fiveam:test parser-test-7
  "Simple test focusing on skipping tokens not in sync list (second '+' and '=' are skipped)."
  (parser-test :grammar '((token int (+ (char-range #\0 #\9)))
                          (token add-op #\+)
                          (token assign #\=)
                          (token semicolon #\;)
                          (rule equality (seq int add-op int assign int semicolon)))
               :grammar-start-rule 'equality
               :text "11+10=+=21;"
               :check-sync-tokens t
               :expected-parsing-result '((constr:token-construct int :ok "11")
                                          (constr:token-construct add-op :ok "+")
                                          (constr:token-construct int :ok "10")
                                          (constr:token-construct assign :ok "=")
                                          (constr:token-construct int :ok "21")
                                          (constr:token-construct semicolon :ok ";")
                                          (constr:sequence-construct equality :ok ";"))))

(fiveam:test parser-test-8
  "Testing erroneous tokens that are found in the sync list (second '='). Unlike the previous, in this,
we need resilience for the sequence construct (since erroneous tokens are found in the sync list). We
also test one-or-more with resilience flag activated, in order to test finding a continuation point."
  (let ()
    (parser-test :grammar '((token int (+ (char-range #\0 #\9)))
                            (token add-op #\+)
                            (token assign #\=)
                            (token semicolon #\;)
                            (rule equality (seq int add-op int assign int semicolon))
                            (rule root (+ equality)))
                 :seq-abort-on-first-failure nil
                 :resilience t
                 :grammar-start-rule 'root
                 :text (concatenate 'string
                                    "100+=20=120;"
                                    "22+33=55;#$%^")
                 :check-sync-tokens '((constr:token-construct int :ok "100")
                                      (constr:token-construct add-op :ok "+")
                                      (constr:token-construct int :no-match "=")
                                      (constr:token-construct assign :ok "=")
                                      (constr:token-construct int :ok "20")
                                      (constr:token-construct semicolon :no-match "120")
                                      (constr:sequence-construct equality :partial-failure "120")
                                      (constr:token-construct int :ok "120")
                                      (constr:token-construct add-op :no-match ";")
                                      (constr:token-construct int :no-match ";")
                                      (constr:token-construct assign :no-match ";")
                                      (constr:token-construct int :no-match ";")
                                      (constr:token-construct semicolon :ok ";")
                                      (constr:sequence-construct equality :partial-failure ";")
                                      (constr:token-construct int :ok "22")
                                      (constr:token-construct add-op :ok "+")
                                      (constr:token-construct int :ok "33")
                                      (constr:token-construct assign :ok "=")
                                      (constr:token-construct int :ok "55")
                                      (constr:token-construct semicolon :ok ";")
                                      (constr:sequence-construct equality :ok ";")
                                      ;; TODO: better to skip these ones in the token construct itself
                                      (constr:token-construct int :regex-not-matched)
                                      (constr:token-construct add-op :regex-not-matched)
                                      (constr:token-construct int :regex-not-matched)
                                      (constr:token-construct assign :regex-not-matched)
                                      (constr:token-construct int :regex-not-matched)
                                      (constr:token-construct semicolon :input-exhausted)
                                      (constr:sequence-construct equality :complete-failure)
                                      (constr:one-or-more-construct root :ok)))))

(fiveam:test parser-test-9
  "Simple test focusing on checking progress during a repeating construct (avoiding infinite loop)."
  (let ()
    (parser-test :grammar '((token int (+ (char-range #\0 #\9)))
                            (token end "$")
                            (rule silly-nums (+ (seq (* int) (? (+ int)) (or (* int) (+ int)))))
                            (rule root (seq silly-nums end)))
                 :seq-abort-on-first-failure nil
                 :resilience t
                 :text "$"
                 :check-sync-tokens t
                 :expected-parsing-result '((constr:token-construct int :no-match "$")
                                            (constr:zero-or-more-construct nil :ok "$")
                                            (constr:token-construct int :no-match "$")
                                            (constr:token-construct int :no-match "$")
                                            (constr:one-or-more-construct nil :ok "$")
                                            (constr:zero-or-one-construct nil :ok "$")
                                            (constr:token-construct int :no-match "$")
                                            (constr:zero-or-more-construct nil :ok "$")
                                            (constr:or-construct nil :ok "$")
                                            (constr:sequence-construct nil :ok "$")
                                            (constr:token-construct int :no-match "$")
                                            (constr:zero-or-more-construct nil :ok "$")
                                            (constr:token-construct int :no-match "$")
                                            (constr:token-construct int :no-match "$")
                                            (constr:one-or-more-construct nil :ok "$")
                                            (constr:zero-or-one-construct nil :ok "$")
                                            (constr:token-construct int :no-match "$")
                                            (constr:zero-or-more-construct nil :ok "$")
                                            (constr:or-construct nil :ok "$")
                                            (constr:sequence-construct nil :ok "$")
                                            (constr:one-or-more-construct silly-nums :ok "$")
                                            (constr:token-construct end :ok "$")
                                            (constr:sequence-construct root :ok "$")))))


