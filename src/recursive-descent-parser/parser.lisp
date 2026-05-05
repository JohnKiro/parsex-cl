(in-package :parsex-cl/rdp/parser)

(defgeneric parse-construct (construct-obj tokenizer token-matching-fn notification-fn)
  (:documentation "Recursively parse the `construct-obj`, with the `tokenizer` passed down, to be used
in tokenization when handling token-construct objects (see corresponding `parse-construct` method).
The `token-matching-fn` is a predicate that matches expected token (1st arg) against actual tokens
received from the regex machine (2nd arg). Implementations should return a truth value or NIL (in case no
match).
The `notification-fn` is funcalled to do any required processing (e.g. constructing parse tree), and
it takes two arguments: the `construct-obj` object, and the parsing status."))

(defparameter +max-parse-execution-count+ 1000 "Temporary protection against infinite recursion")
(defparameter *parse-execution-count* 0 "Temporary protection against infinite recursion")

(defparameter *sync-tokens* (make-hash-table)
  "List of sync tokens for recovery. Created globally for now, just for experimentation, to be moved
locally later.")

(defun add-sync-tokens (list-of-tokens)
  (format t "Adding tokens ~a to sync list..~%" list-of-tokens)
  (dolist (tok list-of-tokens)
    (if #1=(gethash tok *sync-tokens*)
        (incf #1#)
        (setf #1# 1))))

(defun rem-sync-tokens (list-of-tokens)
  (format t "Removing tokens ~a from sync list..~%" list-of-tokens)
  (dolist (tok list-of-tokens)
    (if #1=(gethash tok *sync-tokens*)
        (decf #1#))))

(defun find-in-sync-tokens (actual-tokens)
  (loop for actual-token across actual-tokens
        when (let ((val (gethash actual-token *sync-tokens*)))
               (> val 0))
          return t))

(defparameter *parsing-error-registry* nil
  "This is where I'll record parsing errors, for visual inspection while analyzing and debugging.")

(defun add-parsing-error-entry (expected-token actual-tokens indices)
  (push `(:expected-token ,expected-token :actual-tokens ,actual-tokens :indices ,indices)
        *parsing-error-registry*))

(defun reset-parsing-error-registry ()
  (setf *parsing-error-registry* nil))

(defun dump-parsing-error-registry ()
  (print (reverse *parsing-error-registry*)))
(defmethod parse-construct :around (construct-obj tokenizer token-matching-fn notification-fn)
  #+debug(format t "~&Start parsing construct ~a......~%" construct-obj)
  #+debug(format t "Tokenizer state before: ~a~%" (bt-tokenizer:dump-internal-state tokenizer))
  (when (> *parse-execution-count* +max-parse-execution-count+)
    (error "Recursion protection activated: execution count reached ~a!" *parse-execution-count*))
  (incf *parse-execution-count*)
  (multiple-value-prog1
      (call-next-method)
    #+debug(format t "Tokenizer state after: ~a~%" (bt-tokenizer:dump-internal-state tokenizer))
    #+debug(format t "~&End parsing construct ~a.~%" construct-obj)))

(defmethod parse-construct :around ((construct-obj constr:grammar-construct) tokenizer token-matching-fn
                                    notification-fn)
  "Auxiliary method to call the `notification-fn` after parsing each construct. It's separated to avoid
the redundancy of calling it in each construct method."
  #+debug(format t "Starting :around for construct ~a..~%" construct-obj)
  (multiple-value-bind (result last-tokenization-result) (call-next-method)
    #+debug(format t "Construct: ~a, last tokenization result: ~a.~%" construct-obj
                   last-tokenization-result)
    (funcall notification-fn construct-obj result last-tokenization-result)
    (values result last-tokenization-result)))

(defmethod parse-construct ((construct-obj constr:sequence-construct) tokenizer token-matching-fn
                            notification-fn)
  (loop for child across (constr:child-constructs construct-obj)
        ;; TODO: REFACTOR (SHOULDN'T ACCESS SLOT!!)
        do (add-sync-tokens (slot-value child 'constr::%first-set)))
  (let ((status :ok)
        last-tokenization-result)
    (loop for child across (constr:child-constructs construct-obj)
          do (when (eq status :ok)
               (multiple-value-setq (status last-tokenization-result)
                 (parse-construct child tokenizer token-matching-fn notification-fn)))
             (rem-sync-tokens (slot-value child 'constr::%first-set)))
    (values status last-tokenization-result)))

(defmethod parse-construct ((construct-obj constr:or-construct) tokenizer token-matching-fn
                            notification-fn)
  (loop for child across (constr:child-constructs construct-obj)
        ;; TODO: REFACTOR (SHOULDN'T ACCESS SLOT!!)
        do (add-sync-tokens (slot-value child 'constr::%first-set)))
  (bt-tokenizer:mark-backtracking-position tokenizer construct-obj)
  (let (status last-tokenization-result)
    (loop for child across (constr:child-constructs construct-obj)
          do (rem-sync-tokens (slot-value child 'constr::%first-set))
          ;; if OK (one OR branch succeeded), then just remove it from sync tokens, else rewind and check
          ;; other branches
          unless (eq status :ok) do
            (progn
              (multiple-value-setq (status last-tokenization-result)
                (parse-construct child tokenizer token-matching-fn notification-fn))
              (unless (eq status :ok)
                (bt-tokenizer:rewind-token-position tokenizer construct-obj))))
    ;; TODO: check if need to rewind in the FINALLY clause (meaning no match found, stopping at
    ;; start position, or should we keep at current position? Should be clear when I implement
    ;; actual parsing.
    ;; TODO: check if we need to report a "stronger" error, in case no OR branch succeeded, since OR does
    ;; not tolerate failure, unlike zero-or-one, for example! (see tc #4)
    (bt-tokenizer:unmark-backtracking-position tokenizer construct-obj)
    (values status last-tokenization-result)))

;; default matching (token equality check)
(defun token-matches-p (expected-token actual-tokens)
  "Matches `expected-token` against one of the `actual-tokens` sequence. returns T or NIL. Default
equality test is used (EQL). A custom implementation could be provided instead, for more elaborate needs.
For example, in case of need to inspect the actual token text (say, to map token via
lookup table), a custom implementation could have access to the tokenizer (e.g. within a closure env)."
  (numberp (position expected-token actual-tokens)))


(defmethod parse-construct ((construct-obj constr:token-construct) tokenizer token-matching-fn
                            notification-fn)
  "Matches expected token against next token(s), which it retrieves by calling `get-tokens` on the
tokenizer (`tokenizer`). Returns tokenization status (:ok / :no-match / :invalid-token-or-empty-input),
and a secondary value may also be returned containing the tokenization result (if available), which is a
pair: (actual-tokens . acc-indices).
EXPERIMENTALLY: I'm also checking in case of no match for sync tokens, and keep skipping tokens till
finding a match or sync (later, I think I'll just report status to caller, and it's up to it to
decide how to handle)."
  (let ((expected-token (constr:token construct-obj)))
    (loop
      (alexandria:if-let ((tokenizer-result (bt-tokenizer:get-tokens tokenizer)))
        (destructuring-bind (actual-tokens . acc-indices) tokenizer-result
          (if (funcall token-matching-fn expected-token actual-tokens)
              (progn
                (format t "Success match: expected token ~a, actual token(s) ~a.~%" expected-token
                        actual-tokens)
                (return (values :ok tokenizer-result)))
              (progn
                (add-parsing-error-entry expected-token actual-tokens acc-indices)
                (if (or t (find-in-sync-tokens actual-tokens))
                    ;; WORST THING HERE: the OR branches are treated in the same way (e.g.` (or id int)`)
                    ;; need to avoid this, in order not to have interference between handling of the OR
                    ;; construct, and the error reporting/recovery. I think this is by just reporting
                    ;; the status, without skipping here or recording any error.
                    (progn (format t "No match, token(s) ~a found in sync list.~%" actual-tokens)
                           (return (values (or :no-match :nok-but-found-sync) tokenizer-result)))
                    (format t "No match, and token(s) ~a NOT in sync list.. skipping input token(s)!~%"
                            actual-tokens))))) ; TODO: SKIP, BUT ALSO REPORT ERROR
        (return :invalid-token-or-empty-input)))))

(defun parse-zero-or-more-child (child tokenizer token-matching-fn notification-fn)
  "Reusable parser for zero-or-more, that will be used in both zero-or-more-construct and
one-or-more-construct."
  (loop
    (bt-tokenizer:mark-backtracking-position tokenizer child)
    (multiple-value-bind (status last-tokenization-result)
        (parse-construct child tokenizer token-matching-fn notification-fn)
      (if (eq status :ok)
          (bt-tokenizer:unmark-backtracking-position tokenizer child)
          (progn
            (bt-tokenizer:rewind-token-position tokenizer child)
            (bt-tokenizer:unmark-backtracking-position tokenizer child)
            (return (values :ok last-tokenization-result)))))))

(defmethod parse-construct ((construct-obj constr:one-or-more-construct) tokenizer token-matching-fn
                            notification-fn)
  (let* ((child (constr:child-construct construct-obj)))
    (add-sync-tokens (slot-value child 'constr::%first-set))
    (multiple-value-bind (status1 last-tokenization-result)
        (parse-construct child tokenizer token-matching-fn notification-fn)
      (multiple-value-prog1
          (if (eq status1 :ok)
              (parse-zero-or-more-child child tokenizer token-matching-fn notification-fn)
              (values status1 last-tokenization-result))
        (rem-sync-tokens (slot-value child 'constr::%first-set))))))


(defmethod parse-construct ((construct-obj constr:zero-or-more-construct) tokenizer token-matching-fn
                            notification-fn)
  (let ((child (constr:child-construct construct-obj)))
    (add-sync-tokens (slot-value child 'constr::%first-set))
    (multiple-value-prog1
        (parse-zero-or-more-child child tokenizer token-matching-fn notification-fn)
      (rem-sync-tokens (slot-value child 'constr::%first-set)))))

(defmethod parse-construct ((construct-obj constr:zero-or-one-construct) tokenizer token-matching-fn
                            notification-fn)
  ;; what about putting this in :before? (TODO: CHECK!)
  (bt-tokenizer:mark-backtracking-position tokenizer construct-obj)
  (multiple-value-bind (status last-tokenization-result) (parse-construct
                                                          (constr:child-construct construct-obj)
                                                          tokenizer token-matching-fn notification-fn)
    (unless (eq status :ok)
      (bt-tokenizer:rewind-token-position tokenizer construct-obj))
    ;; we unmark backtracking position, and return success even if parsing failed (since construct is
    ;; optional)
    (bt-tokenizer:unmark-backtracking-position tokenizer construct-obj)
    (values :ok last-tokenization-result)))
