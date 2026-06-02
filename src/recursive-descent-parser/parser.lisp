(in-package :parsex-cl/rdp/parser)

(defparameter +max-parse-recursion-depth+ 40 "Temporary protection against infinite recursion")

(func-v1:define-functional-interface sync-tokens-manager ()
  "Interface of token sync list manager, supporting the operations to add, remove, and find tokens."
  (add-sync-tokens (tokens) :doc "Add list of tokens (`tokens`) to the sync list.")
  (rem-sync-tokens (tokens) :doc "Remove list of tokens (`tokens`) from the sync list.")
  (find-in-sync-tokens (tokens) :doc "Search for any token specified in the array `tokens` in the sync
list."))

(defun sync-tokens-manager-factory ()
  "Create a sync token manager instance, for use during a parsing job. It initializes the sync list as
an empty hash table, and returns a struct holding pointers to the operations (add/remove/find)."
  (let ((sync-tokens (make-hash-table)))
    (labels ((add-sync-tokens (tokens)
               (dolist (tok tokens)
                 (if #1=(gethash tok sync-tokens)
                     (incf #1#)
                     (setf #1# 1))))
             (rem-sync-tokens (tokens)
               (dolist (tok tokens)
                 (if (gethash tok sync-tokens)
                     (decf #1#))))
             (find-in-sync-tokens (tokens)
               "Notice that unlike the above functions, the tokens to search for are typically retrieved
from tokenizer, in the form of array rather than a list. I'm planning to convert the above into array
anyway (typically coming from construct's first set)."
               (loop for tok across tokens
                     when (> (gethash tok sync-tokens) 0)
                       return t)))
      (make-sync-tokens-manager :add-sync-tokens-fn #'add-sync-tokens
                                :rem-sync-tokens-fn #'rem-sync-tokens
                                :find-in-sync-tokens-fn #'find-in-sync-tokens))))

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

(defun parse-root (root-construct-obj tokenizer token-matching-fn notification-fn
                   &key resilience (seq-abort-on-first-failure t) (check-sync-tokens t)
                   &aux
                     (recursion-depth 0)
                     (sync-token-mgr (sync-tokens-manager-factory))
                     (input-exhausted nil))
  "Entry point for the parser, starting with the root construct `root-construct-obj`, recursively parsing
it, and using a backtracking tokenizer implementation `tokenizer` to retrieve tokens from input source.
The `token-matching-fn` is a predicate that matches expected token (1st arg) against actual tokens
received from the regex machine (2nd arg). Implementations should return a truth value or NIL (in case no
match).
The `notification-fn` is funcalled to do any required processing (e.g. constructing parse tree), and
it takes three arguments: the `construct-obj` object, the parsing status, and parsing result details,
which is currently only used for token constructs (all other constructs need just a status, at least for
now). TODO: actually need two callback functions: pre and post.
`resilience` is a flag indicating whether the one-or-more/zero-or-more construct parsers should abort
loop on first child's failure, or proceed with attempt to parse child over again. For these constructs
also, tokenizer progress is checked, to avoid infinite loop in case of zero consumption (e.g.
child failure, or even successfully matching a zero-length string. In such cases, the construct parsing
would abort anyway, regardless of the resilience flag.
`seq-abort-on-first-failure` is a flag indicating whether the seq construct parser should abort on first
failure (same purpose as resilience, but with inverted meaning), or proceed with attempt to parse all
remaining children. TODO: TO BE MERGED INTO RESILIENCE, AND ALLOWING OVERRIDING PER CONSTRUCT IN GRAMMAR.
`check-sync-tokens` flag is used by parser for token constructs, to check in sync list for tokens that
are not matched. If set and token is found in sync list, then the token will be put back into the
tokenizer for the upper construct that is interested in it, else (if not found), then it will be
skipped (since won't be interesting to any upper construct), else (if flag is not set), then error will
returned without skipping."
  (labels ((parse-construct (construct-obj)
             #+debug(format t "~&Start parsing construct ~a......~%" construct-obj)
             #+debug(format t "Tokenizer state before: ~a~%" (bt-tokenizer:dump-internal-state tokenizer))
             (when (> recursion-depth +max-parse-recursion-depth+)
               (error "Recursion protection activated: execution count reached ~a!" recursion-depth))
             (incf recursion-depth)
             (multiple-value-bind (status result-details)
                 (etypecase construct-obj
                   (constr:token-construct (parse-token-construct construct-obj))
                   (constr:sequence-construct (parse-sequence-construct construct-obj))
                   (constr:or-construct (parse-or-construct construct-obj))
                   (constr:one-or-more-construct (parse-one-or-more-construct construct-obj))
                   (constr:zero-or-more-construct (parse-zero-or-more-construct construct-obj))
                   (constr:zero-or-one-construct (parse-zero-or-one-construct construct-obj)))
               (funcall notification-fn construct-obj status result-details)
               ;; TODO: it might be useful to return whatever the notif function returns (which could be multiple
               ;; values. This gives control to the client code, but one condition on the notif function, in order
               ;; to preserve the parsing flow, is to include the `status` as the primary value (any other params
               ;; could be added as secondary values)
               (decf recursion-depth)
               #+debug(format t "Tokenizer state after: ~a~%" (bt-tokenizer:dump-internal-state tokenizer))
               #+debug(format t "~&End parsing construct ~a.~%" construct-obj)
               (values status result-details)))
           (parse-token-construct (construct-obj)
             "Matches expected token against next token(s), which it retrieves by calling `get-tokens` on
the tokenizer (`tokenizer`). Returns status (:ok / :no-match / status returned by tokenizer),
and a secondary value contains tokenization details as an `token-construct-parsing-result` object.
TODO: consider just reporting the status to caller, and leaving it up to it to decide how to handle."
             (let ((expected-token (constr:token construct-obj))
                   (skipped-tokens nil))
               (loop
                 (multiple-value-bind (tok-and-indices tokenization-status)
                     (bt-tokenizer:get-tokens tokenizer)
                   (if tok-and-indices
                       (destructuring-bind (actual-tokens . acc-indices) tok-and-indices
                         (if (funcall token-matching-fn expected-token actual-tokens)
                             (return (values :ok (make-instance
                                                  'token-construct-parsing-result
                                                  :tokenization-status tokenization-status
                                                  :tokenizer-matched-token actual-tokens
                                                  :tokenizer-matched-tokens-indices acc-indices
                                                  :skipped-tokens (nreverse skipped-tokens))))
                             (if check-sync-tokens
                                 (if (find-in-sync-tokens sync-token-mgr actual-tokens)
                                     (progn
                                       ;; alternatively, need to move this logic to tok
                                       #+debug
                                       (format t "No match, token(s) ~a found in sync list, returning..~%"
                                               actual-tokens)
                                       (bt-tokenizer:put-back-tokens tokenizer)
                                       (return
                                         (values :no-match ;TODO: consider something such as :token-not-consumed
                                                 (make-instance
                                                  'token-construct-parsing-result
                                                  :tokenization-status tokenization-status
                                                  :tokenizer-matched-token actual-tokens
                                                  :tokenizer-matched-tokens-indices acc-indices
                                                  :skipped-tokens (nreverse skipped-tokens)))))
                                     (progn
                                       #+debug
                                       (format t (concatenate 'string
                                                              "No match, token(s) ~a NOT in sync list, "
                                                              "skipped and checking next token.~%")
                                               actual-tokens)
                                       (push tok-and-indices skipped-tokens)))
                                 (progn
                                   ;; not checking sync list, rather, returning token error to parent.
                                   #+debug
                                   (format t "No match, token(s) ~a.~%" actual-tokens)
                                   (return
                                     (values :no-match ;TODO: consider something such as :invalid-token
                                             (make-instance
                                              'token-construct-parsing-result
                                              :tokenization-status tokenization-status
                                              :tokenizer-matched-token actual-tokens
                                              :tokenizer-matched-tokens-indices acc-indices
                                              :skipped-tokens (nreverse skipped-tokens))))))))
                       (progn
                         (when (and (not input-exhausted) (eq tokenization-status :input-exhausted))
                           (setf input-exhausted t))
                         (format t "~%No token returned, tokenizer status: ~a.~%"
                                 tokenization-status)
                         ;; TOOD: consider adding a flag to loop till get a token, in case the
                         ;; status is regex not matched (note that here we catch also input
                         ;; exhausted case)
                         (return
                           (values tokenization-status
                                   (make-instance 'token-construct-parsing-result
                                                  :tokenization-status tokenization-status
                                                  :skipped-tokens (nreverse skipped-tokens))))))))))
           (parse-sequence-construct (construct-obj)
             (loop for child across (constr:child-constructs construct-obj)
                   do (add-sync-tokens sync-token-mgr (constr:first-set child)))
             (let ((curr-child-status nil)
                   (a-child-failed nil)
                   (progress nil))
               (loop for child across (constr:child-constructs construct-obj)
                     ;; yes, 1st child added needlessly, but this way the above loop is simple
                     ;; note that we still need to remove sync tokens, even if we abort from the sequence
                     do (rem-sync-tokens sync-token-mgr (constr:first-set child))
                        ;; TODO: may have a check here for :input-exhausted condition, to break the loop
                        ;; if so, but not sure, because this would be done unnecessarily many times,
                        ;; until we reach end of input
                        (unless (and a-child-failed seq-abort-on-first-failure)
                          (setf curr-child-status (parse-construct child))
                          (if (eq curr-child-status :ok)
                              (setf progress t) ;at least part of the sequence has succeeded
                              (progn
                                (setf a-child-failed t)
                                ;; partial failure implies there is also some progress
                                (when (eq curr-child-status :partial-failure)
                                  (setf progress t))))))
               (if progress
                   (if a-child-failed
                       :partial-failure
                       :ok)
                   :complete-failure)))
           (parse-or-construct (construct-obj)
             (loop for child across (constr:child-constructs construct-obj)
                   do (add-sync-tokens sync-token-mgr (constr:first-set child)))
             (bt-tokenizer:mark-backtracking-position tokenizer construct-obj)
             (let (status)
               (loop for child across (constr:child-constructs construct-obj)
                     do (rem-sync-tokens sync-token-mgr (constr:first-set child))
                     unless (eq status :ok) do
                       (progn
                         (setf status (parse-construct child))
                         (unless (eq status :ok)
                           (bt-tokenizer:rewind-token-position tokenizer construct-obj))))
               ;; TODO: check if need to rewind in the FINALLY clause (meaning no match found, stopping
               ;; at start position, or should we keep at current position? Should be clear when I
               ;; implement actual parsing.
               ;; TODO: check if we need to report a "stronger" error, in case no OR branch succeeded,
               ;; since OR does not tolerate failure, unlike zero-or-one, for example! (see tc #4)
               (bt-tokenizer:unmark-backtracking-position tokenizer construct-obj)
               ;; TODO (19 May): I think status should either be OK or NOK, so if last branch's status is
               ;; :partial-failure, for example, should change it, because it's not meaningful as a
               ;; parsing states of the OR
               status))
           (parse-zero-or-more-child (child)
             "Parser for zero-or-more, that will be used in both zero-or-more-construct and
one-or-more-construct. It parses the child as long as it gets success status or if the resilience flag is
set, until it gets 'zero consumption', and it reports success in all cases (zero occurrence is accepted).
Note that any inner errors will be reported by the inner constructs themselves."
             (loop with status = nil
                   for prev-position = nil then curr-position
                   for curr-position = (bt-tokenizer:get-current-backtracking-position tokenizer)
                   do  ;; alternatively, mark-backtracking-position itself returns current position, and
                       ;; we check progress: if no progress, then unmark and return. This saves the need
                       ;; for the get-current-backtracking-position operation, but it could be useful op
                       ;; anyway, if we need to get progress without marking.
                       (when (and prev-position
                                  (eq (bt-tokenizer:compare-positions tokenizer prev-position
                                                                      curr-position)
                                      :no-progress))
                         (return :ok))
                       (bt-tokenizer:mark-backtracking-position tokenizer child)
                       (setf status (parse-construct child))
                       (if (or (eq status :ok) resilience)
                           (bt-tokenizer:unmark-backtracking-position tokenizer child)
                           (progn
                             (bt-tokenizer:rewind-token-position tokenizer child)
                             (bt-tokenizer:unmark-backtracking-position tokenizer child)
                             (return :ok)))))
           (parse-one-or-more-construct (construct-obj)
             (let* ((child (constr:child-construct construct-obj)))
               (add-sync-tokens sync-token-mgr (constr:first-set child))
               (let ((status1 (parse-construct child)))
                 (multiple-value-prog1
                     (if (or (eq status1 :ok) resilience)
                         (parse-zero-or-more-child child)
                         :complete-failure)
                   (rem-sync-tokens sync-token-mgr (constr:first-set child))))))
           (parse-zero-or-more-construct (construct-obj)
             (let ((child (constr:child-construct construct-obj)))
               (add-sync-tokens sync-token-mgr (constr:first-set child))
               (multiple-value-prog1
                   (parse-zero-or-more-child child)
                 (rem-sync-tokens sync-token-mgr (constr:first-set child)))))
           (parse-zero-or-one-construct (construct-obj)
             ;; what about putting this in :before? (TODO: CHECK!)
             (bt-tokenizer:mark-backtracking-position tokenizer construct-obj)
             (let ((status (parse-construct (constr:child-construct construct-obj))))
               (unless (eq status :ok)
                 (bt-tokenizer:rewind-token-position tokenizer construct-obj))
               ;; we unmark backtracking position, and return success even if parsing failed (since
               ;; construct is optional)
               (bt-tokenizer:unmark-backtracking-position tokenizer construct-obj)
               :ok)))
    (parse-construct root-construct-obj)))

;; default matching (token equality check)
(defun token-matches-p (expected-token actual-tokens)
  "Matches `expected-token` against one of the `actual-tokens` sequence. returns T or NIL. Default
equality test is used (EQL). A custom implementation could be provided instead, for more elaborate needs.
For example, in case of need to inspect the actual token text (say, to map token via
lookup table), a custom implementation could have access to the tokenizer (e.g. within a closure env)."
  (numberp (position expected-token actual-tokens)))

(defclass token-construct-parsing-result ()
  ((%tokenization-status
    :initarg :tokenization-status
    :reader tokenization-status
    :documentation "Status returned by tokenizer. Nil (no status) means ok.")
   (%tokenizer-matched-tokens
    :initarg :tokenizer-matched-token
    :initform nil
    :reader tokenizer-matched-token
    :documentation "Actual token(s) that the tokenizer succeeded to match.")
   (%tokenizer-matched-tokens-indices
    :initarg :tokenizer-matched-tokens-indices
    :initform nil
    :reader tokenizer-matched-tokens-indices
    :documentation "Indice of the matched token(s) as a dotted pair: (start . end).")
   (%skipped-tokens
    :initarg :skipped-tokens
    :initform nil
    :reader skipped-tokens
    :documentation "List of invalid tokens that were neither matched nor found in the sync list." )))
