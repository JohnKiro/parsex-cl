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

(defmethod parse-construct :around (construct-obj tokenizer token-matching-fn notification-fn)
  #+debug(format t "~&Start parsing construct ~a......~%" construct-obj)
  #+debug(format t "Tokenizer state before: ~a~%" (bt-tokenizer:dump-internal-state tokenizer))
  (when (> *parse-execution-count* +max-parse-execution-count+)
    (error "Recursion protection activated: execution count reached ~a!" *parse-execution-count*))
  (incf *parse-execution-count*)
  (prog1
      (call-next-method)
    #+debug(format t "Tokenizer state after: ~a~%" (bt-tokenizer:dump-internal-state tokenizer))
    #+debug(format t "~&End parsing construct ~a.~%" construct-obj)))

(defmethod parse-construct :around ((construct-obj constr:grammar-construct) tokenizer token-matching-fn
                                    notification-fn)
  "Auxiliary method to call the `notification-fn` after parsing each construct. It's separated to avoid
the redundancy of calling it in each construct method."
  (let ((result (call-next-method)))
    (funcall notification-fn construct-obj result)
    result))

(defmethod parse-construct ((construct-obj constr:sequence-construct) tokenizer token-matching-fn
                            notification-fn)
  (loop for child across (constr:child-constructs construct-obj)
        for result = (parse-construct child tokenizer token-matching-fn notification-fn)
        while (eq result :ok)
        finally (return result)))

(defmethod parse-construct ((construct-obj constr:or-construct) tokenizer token-matching-fn
                            notification-fn)
  (bt-tokenizer:mark-backtracking-position tokenizer construct-obj)
  (loop for child across (constr:child-constructs construct-obj)
        for result = (parse-construct child tokenizer token-matching-fn notification-fn)
        if (eq result :ok)
          do (bt-tokenizer:unmark-backtracking-position tokenizer construct-obj)
             (return result)
        else
          do (bt-tokenizer:rewind-token-position tokenizer construct-obj)
             ;; TODO: check if need to rewind in the FINALLY clause (meaning no match found, stopping at
             ;; start position, or should we keep at current position? Should be clear when I implement
             ;; actual parsing.
        finally (progn
                  (bt-tokenizer:unmark-backtracking-position tokenizer construct-obj)
                  (return result))))

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
tokenizer (`tokenizer`). In case of success, it advances the backtracking index.
Returns tokenization status (:ok / :no-match / :invalid-token-or-empty-input)."
  (let ((expected-token (constr:token construct-obj)))
    (alexandria:if-let ((tokenizer-result (bt-tokenizer:get-tokens tokenizer)))
      (destructuring-bind (actual-tokens . acc-indices) tokenizer-result
        (if (funcall token-matching-fn expected-token actual-tokens)
            (progn
              (bt-tokenizer:advance tokenizer)
              :ok)
            :no-match))
      :invalid-token-or-empty-input)))

(defmethod parse-construct ((construct-obj constr:one-or-more-construct) tokenizer token-matching-fn
                            notification-fn)
  (let* ((child (constr:child-construct construct-obj))
         (result1 (parse-construct child tokenizer token-matching-fn notification-fn)))
    (if (eq result1 :ok)
        (loop for result = (progn
                             (bt-tokenizer:mark-backtracking-position tokenizer child)
                             (parse-construct child tokenizer token-matching-fn notification-fn))
              if (eq result :ok)
                do (bt-tokenizer:unmark-backtracking-position tokenizer child)
              else
                do (progn
                     (bt-tokenizer:rewind-token-position tokenizer child)
                     (bt-tokenizer:unmark-backtracking-position tokenizer child)
                     (return result1))
              finally (progn
                        (error "DEBUG: IS THIS REALLY A DEAD CODE??")
                        (return result)))
        result1)))

(defmethod parse-construct ((construct-obj constr:zero-or-more-construct) tokenizer token-matching-fn
                            notification-fn)
  (bt-tokenizer:mark-backtracking-position tokenizer construct-obj)
  (loop with child = (constr:child-construct construct-obj)
        for result = (progn
                       (bt-tokenizer:mark-backtracking-position tokenizer child)
                       (parse-construct child tokenizer token-matching-fn notification-fn))
        if (eq result :ok)
          do (bt-tokenizer:unmark-backtracking-position tokenizer child)
        else
          do (progn
               (bt-tokenizer:rewind-token-position tokenizer child)
               (bt-tokenizer:unmark-backtracking-position tokenizer child)
               (return :ok))))

(defmethod parse-construct ((construct-obj constr:zero-or-one-construct) tokenizer token-matching-fn
                            notification-fn)
  ;; what about putting this in :before? (TODO: CHECK!)
  (bt-tokenizer:mark-backtracking-position tokenizer construct-obj)
  (let ((result (parse-construct (constr:child-construct construct-obj) tokenizer token-matching-fn
                                 notification-fn)))
    (unless (eq result :ok)
      (bt-tokenizer:rewind-token-position tokenizer construct-obj))
    ;; we unmark backtracking position, and return success even if parsing failed (since construct is
    ;; optional)
    (bt-tokenizer:unmark-backtracking-position tokenizer construct-obj)
    :ok))
