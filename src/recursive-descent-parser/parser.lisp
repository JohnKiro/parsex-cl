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

(defparameter *resilience* nil "Flag indicating whether the one-or-more/zero-or-more construct parsers
should abort loop on first child's failure, or proceed with attempt to parse child over again. Note that
a child's failure wih zero consumption would abort anyway, regardless of the flag.")

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

;; FIXME: we depend on the fact that the OR element included its 1st set. Example: in (or int id), when
;; matching the INT token fails, and assuming the `*check-sync-tokens*` flag is active, then we'll find
;; ID in the sync list, and report :no-match, which will be understood by the OR construct as failure,
;; and will rewind and try the 'id' branch. If the sync list does not contain the 'id' token (for some
;; reason, then it will skip it, and get next token (after the ID), and hence interferes with the upper
;; OR, which is bad! Why bad? Because the purpose of sync list is to find a continuation point in case
;; of failure, which is not the case here, since failure in one branch of the OR construct is normal.
;; So currently for the OR to operate properly, it must add 1st set of all branches to the sync list!
;; So I think alternatively, I may just report the status (e.g. :no-match-but-token-found-in-sync-list),
;; without skipping here, and leave it to the upper construct to handle the error (OR, for example, would
;; rewind and try next branch).
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
                  (if *check-sync-tokens*
                      (if (find-in-sync-tokens actual-tokens)
                          (progn
                            ;; alternatively, need to move this logic to tok
                            #+debug
                            (format t "No match, token(s) ~a found in sync list, returning..~%"
                                    actual-tokens)
                            (bt-tokenizer::put-back-tokens tokenizer)
                            (return
                              (values :no-match ;TODO: consider something such as :token-not-consumed
                                      tok-and-indices
                                      (nreverse skipped-tokenization-result-log))))
                          (progn
                            #+debug
                            (format t (concatenate 'string
                                                   "No match, token(s) ~a NOT in sync list, "
                                                   "skipped and checking next token.~%")
                                    actual-tokens)
                            (push tok-and-indices skipped-tokenization-result-log)))
                      (progn
                        ;; not checking sync list, rather, returning token error to parent.
                        #+debug
                        (format t "No match, token(s) ~a.~%" actual-tokens)
                        (return (values :no-match ;TODO: consider something such as :invalid-token
                                        tok-and-indices
                                        (nreverse skipped-tokenization-result-log)))))))
            (return (progn
                      (format t "~%No token returned, tokenizer status: ~a.~%" tokenizer-status)
                      ;; TOOD: consider adding a flag to loop till get a token, in case the
                      ;; status is regex not matched (note that here we catch also input exhausted case)
                      (values tokenizer-status
                              nil
                              (nreverse skipped-tokenization-result-log)))))))))

(defun parse-zero-or-more-child (child tokenizer token-matching-fn notification-fn)
  "Reusable parser for zero-or-more, that will be used in both zero-or-more-construct and
one-or-more-construct. It parses the child as long as it gets success status or if the resilience flag is
set, until it gets 'zero consumption', and it reports success in all cases (zero occurrence is accepted).
Note that any inner errors will be reported by the inner constructs themselves."
  (loop with status = nil
        with last-tokenization-result = nil
        for prev-position = nil then curr-position
        for curr-position = (bt-tokenizer::get-current-backtracking-position tokenizer)
        do  ;; alternatively, mark-backtracking-position itself returns current position, and we check
            ;; progress: if no progress, then unmark and return. This saves the need for the
            ;; get-current-backtracking-position operation, but it could be useful op anyway, if we need
            ;; to get progress without marking.
            (when (and prev-position
                       (eq (bt-tokenizer::compare-positions tokenizer prev-position curr-position)
                           :no-progress))
              (format t "~%No progress in parsing loop (prev: ~a, curr: ~a, last tok: ~a), aborting..~%"
                      prev-position curr-position last-tokenization-result)
              (return (values :ok last-tokenization-result)))
            (bt-tokenizer:mark-backtracking-position tokenizer child)
            (multiple-value-setq (status last-tokenization-result)
              (parse-construct child tokenizer token-matching-fn notification-fn))
            (if (or (eq status :ok) *resilience*)
                (bt-tokenizer:unmark-backtracking-position tokenizer child)
                (progn
                  (bt-tokenizer:rewind-token-position tokenizer child)
                  (bt-tokenizer:unmark-backtracking-position tokenizer child)
                  (return (values :ok last-tokenization-result))))))

(defmethod parse-construct ((construct-obj constr:one-or-more-construct) tokenizer token-matching-fn
                            notification-fn)
  (let* ((child (constr:child-construct construct-obj)))
    (add-sync-tokens (slot-value child 'constr::%first-set))
    (multiple-value-bind (status1 last-tokenization-result)
        (parse-construct child tokenizer token-matching-fn notification-fn)
      (multiple-value-prog1
          (if (or (eq status1 :ok) *resilience*) ;TODO: INCLUDE CHECK FOR 'ZERO CONSUMPTION'!!
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
