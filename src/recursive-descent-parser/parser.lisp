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

(defparameter *seq-abort-on-first-failure* t "Flag indicating whether the seq construct parser should
abort on first failure, or proceed with attempt to parse all remaining children. To be moved to local
config.")

(defparameter *sync-tokens* (make-hash-table)
  "List of sync tokens for recovery. Created globally for now, just for experimentation, to be moved
locally later.")

(defparameter *check-sync-tokens* t "Flag used by parser for token constructs, to check in sync list for
tokens that are not matched. True means check and skip if not found, false (NIL) means return whatever
status, without skipping. TODO: To be moved locally later.")

(defun add-sync-tokens (list-of-tokens)
  (format t "~%Adding tokens ~a to sync list..~%" list-of-tokens)
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
  (multiple-value-bind (status last-tokenization-result skipped-tokenizations-log) (call-next-method)
    #+debug(format t "Construct: ~a, last tokenization result: ~a.~%" construct-obj
                   last-tokenization-result)
    (funcall notification-fn construct-obj status last-tokenization-result skipped-tokenizations-log)
    ;; TODO: it might be useful to return whatever the notif function returns (which could be multiple
    ;; values. This gives control to the client code, but one condition on the notif function, in order
    ;; to preserve the parsing flow, is to include the `status` as the primary value (any other params
    ;; could be added as secondary values)
    (values status last-tokenization-result)))

(defmethod parse-construct ((construct-obj constr:sequence-construct) tokenizer token-matching-fn
                            notification-fn)
  (loop for child across (constr:child-constructs construct-obj)
        ;; TODO: REFACTOR (SHOULDN'T ACCESS SLOT!!)
        do (add-sync-tokens (slot-value child 'constr::%first-set)))
  (let ((curr-child-status nil)
        (a-child-failed nil)
        (progress nil)
        (last-tokenization-result nil))
    (loop for child across (constr:child-constructs construct-obj)
          ;; yes, 1st child added needlessly, but this way the above loop is simple
          ;; note that we still need to remove sync tokens, even if we abort from the sequence
          do (rem-sync-tokens (slot-value child 'constr::%first-set))
             (unless (and a-child-failed *seq-abort-on-first-failure*)
               (multiple-value-setq (curr-child-status last-tokenization-result)
                 (parse-construct child tokenizer token-matching-fn notification-fn))
               (if (eq curr-child-status :ok)
                   (setf progress t) ;at least part of the sequence has succeeded
                   (progn
                     (setf a-child-failed t)
                     ;; partial failure implies there is also some progress
                     (when (eq curr-child-status :partial-failure)
                       (setf progress t))))))
    (values (cond
              ;; strong indication of a "real" parsing error
              ((and progress a-child-failed) :partial-failure)
              ;; all children succeeded
              (progress :ok)
              ;; complete failure
              (t :complete-failure))
            last-tokenization-result)))

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
tokenizer (`tokenizer`). Returns status (:ok / :no-match / status returned by tokenizer),
and a secondary value may also be returned containing the tokens and input indices (if available), as a
pair: (actual-tokens . acc-indices), and finally, a list of skipped tokenization details is returned as
a third value.
NOTE: checking the sync tokens is controlled by a global flag `*check-sync-tokens*`, for now.
TODO: consider just reporting the status to caller, and leaving it up to it to decide how to handle."
  (let ((expected-token (constr:token construct-obj))
        (skipped-tokenization-result-log nil))
    (loop
      (multiple-value-bind (tok-and-indices tokenizer-status) (bt-tokenizer:get-tokens tokenizer)
        (if tok-and-indices
            (destructuring-bind (actual-tokens . acc-indices) tok-and-indices
              (if (funcall token-matching-fn expected-token actual-tokens)
                  (progn
                    #+debug
                    (format t "~%Expected token ~a matched with ~a~%" expected-token actual-tokens)
                    (return (values :ok tok-and-indices (nreverse skipped-tokenization-result-log))))
                  (if (or (not *check-sync-tokens*) (find-in-sync-tokens actual-tokens))
                      ;; FIXME: the OR branches are treated in the same way (e.g.` (or id int)`)
                      ;; need to avoid this, in order not to have interference between handling of the OR
                      ;; construct, and the error reporting/recovery.
                      ;; In other words, we depend on the fact that the OR element included its 1st set
                      ;; to the sync list already, that's why by skipping here, we don't risk to miss
                      ;; relevant OR branches, but I don't like this, since we depend on the OR behavior
                      ;; here!
                      ;; I think alternatively, I'll just report the status, without skipping here, and
                      ;; leave it to the upper construct to handle the error.
                      (progn
                        #+debug
                        (format t (if *check-sync-tokens*
                                      "No match, token(s) ~a found in sync list.~%"
                                      "No match, token(s) ~a.~%")
                                actual-tokens)
                        (return (values :no-match ;TODO: consider something such as :invalid-token
                                        tok-and-indices
                                        (nreverse skipped-tokenization-result-log))))
                      (progn
                        ;; TODO: Skip token? Report error in log? Report error to upper?
                        #+debug
                        (format t "No match, token(s) ~a NOT in sync list.. skipped.~%"
                                actual-tokens)
                        (push tok-and-indices skipped-tokenization-result-log)
                        #+nil(return (values :no-match-and-no-sync-ahead tok-and-indices))))))
            (return (values tokenizer-status
                            nil
                            (nreverse skipped-tokenization-result-log))))))))

(defun parse-zero-or-more-child (child tokenizer token-matching-fn notification-fn)
  "Reusable parser for zero-or-more, that will be used in both zero-or-more-construct and
one-or-more-construct. It parses the child as long as it get success status, and it reports success in
all cases (zero occurrence is accepted)."
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
              (values :complete-failure last-tokenization-result))
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
  (multiple-value-bind (status last-tokenization-result)
      (parse-construct (constr:child-construct construct-obj)
                       tokenizer token-matching-fn notification-fn)
    (unless (eq status :ok)
      (bt-tokenizer:rewind-token-position tokenizer construct-obj))
    ;; we unmark backtracking position, and return success even if parsing failed (since construct is
    ;; optional)
    (bt-tokenizer:unmark-backtracking-position tokenizer construct-obj)
    (values :ok last-tokenization-result)))
