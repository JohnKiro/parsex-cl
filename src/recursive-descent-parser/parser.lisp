(in-package :parsex-cl/rdp/parser)

(defgeneric parse-construct (construct-obj tokenizer notification-fn)
  (:documentation "Recursively parse the `construct-obj`, with the `tokenizer` passed down, to be used
in tokenization when handling token-construct objects (see corresponding `parse-construct` method).
The `notification-fn` is funcalled to do any required processing (e.g. constructing parse tree), and
it takes two arguments: the `construct-obj` object, and the parsing status."))

(defparameter +max-parse-execution-count+ 1000 "Temporary protection against infinite recursion")
(defparameter *parse-execution-count* 0 "Temporary protection against infinite recursion")

(defmethod parse-construct :around (construct-obj tokenizer notification-fn)
  #+debug(format t "~&Start parsing construct ~a......~%" construct-obj)
  #+debug(format t "Tokenizer state before: ~a~%" (bt-tokenizer:dump-internal-state tokenizer))
  (when (> *parse-execution-count* +max-parse-execution-count+)
    (error "Recursion protection activated: execution count reached ~a!" *parse-execution-count*))
  (incf *parse-execution-count*)
  (prog1
      (call-next-method)
    #+debug(format t "Tokenizer state after: ~a~%" (bt-tokenizer:dump-internal-state tokenizer))
    #+debug(format t "~&End parsing construct ~a.~%" construct-obj)))

(defmethod parse-construct ((construct-obj constr::sequence-construct) tokenizer notification-fn)
  (loop for child across (constr::child-constructs construct-obj)
        for result = (parse-construct child tokenizer notification-fn)
        while (eq result :ok)
        finally (progn (funcall notification-fn construct-obj result)
                       (return result))))

(defmethod parse-construct ((construct-obj constr::or-construct) tokenizer notification-fn)
  (bt-tokenizer:mark-backtracking-position tokenizer construct-obj)
  (loop for child across (constr::child-constructs construct-obj)
        for result = (parse-construct child tokenizer notification-fn)
        if (eq result :ok)
          do (bt-tokenizer:unmark-backtracking-position tokenizer construct-obj)
             (funcall notification-fn construct-obj result)
             (return result)
        else
          do (bt-tokenizer:rewind-token-position tokenizer construct-obj)
             ;; TODO: check if need to rewind in the FINALLY clause (meaning no match found, stopping at
             ;; start position, or should we keep at current position? Should be clear when I implement
             ;; actual parsing.
        finally (progn
                  (funcall notification-fn construct-obj result)
                  (return result))))

(defmethod parse-construct ((construct-obj constr::token-construct) tokenizer notification-fn)
  (let ((expected-token (constr::token construct-obj)))
    (multiple-value-bind (matched-token status slice-indices)
        (bt-tokenizer::match-token tokenizer expected-token)
      (funcall notification-fn construct-obj status)
      status)))

(defmethod parse-construct ((construct-obj constr::one-or-more-construct) tokenizer notification-fn)
  (let* ((child (constr::child-construct construct-obj))
         (result1 (parse-construct child tokenizer notification-fn)))
    (if (eq result1 :ok)
        (loop for result = (progn
                             (bt-tokenizer:mark-backtracking-position tokenizer child)
                             (parse-construct child tokenizer notification-fn))
              if (eq result :ok)
                do (bt-tokenizer:unmark-backtracking-position tokenizer child)
              else
                do (progn
                     (bt-tokenizer:rewind-token-position tokenizer child)
                     (bt-tokenizer:unmark-backtracking-position tokenizer child)
                     (funcall notification-fn construct-obj result1)
                     (return result1))
              finally (progn (funcall notification-fn construct-obj result)
                             (error "DEBUG: IS THIS REALLY A DEAD CODE??")
                             (return result)))
        (progn
          (funcall notification-fn construct-obj result1)
          result1))))

(defmethod parse-construct ((construct-obj constr::zero-or-more-construct) tokenizer notification-fn)
  (bt-tokenizer:mark-backtracking-position tokenizer construct-obj)
  (loop with child = (constr::child-construct construct-obj)
        for result = (progn
                       (bt-tokenizer:mark-backtracking-position tokenizer child)
                       (parse-construct child tokenizer notification-fn))
        if (eq result :ok)
          do (bt-tokenizer:unmark-backtracking-position tokenizer child)
        else
          do (progn
               (bt-tokenizer:rewind-token-position tokenizer child)
               (bt-tokenizer:unmark-backtracking-position tokenizer child)
               (funcall notification-fn construct-obj result)
               (return :ok))))

(defmethod parse-construct ((construct-obj constr::zero-or-one-construct) tokenizer notification-fn)
  ;; what about putting this in :before? (TODO: CHECK!)
  (bt-tokenizer:mark-backtracking-position tokenizer construct-obj)
  (let ((result (parse-construct (constr::child-construct construct-obj) tokenizer notification-fn)))
    (unless (eq result :ok)
      (bt-tokenizer:rewind-token-position tokenizer construct-obj))
    ;; I think  we unmark backtracking position, and return success even if parsing failed
    ;; (since construct is optional)
    (bt-tokenizer:unmark-backtracking-position tokenizer construct-obj)
    (funcall notification-fn construct-obj result)
    :ok))
