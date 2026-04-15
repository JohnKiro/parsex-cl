(in-package :parsex-cl/rdp/parser)

(defgeneric parse-construct (construct-obj tokenizer parse-tree))

(defparameter +max-parse-execution-count+ 1000 "Temporary protection against infinite recursion")
(defparameter *parse-execution-count* 0 "Temporary protection against infinite recursion")

(defmethod parse-construct :around (construct-obj tokenizer parse-tree)
  #+debug(format t "~&Start parsing construct ~a......~%" construct-obj)
  #+debug(format t "Tokenizer state before: ~a~%" (bt-tokenizer:dump-internal-state tokenizer))
  (when (> *parse-execution-count* +max-parse-execution-count+)
    (error "Recursion protection activated: execution count reached ~a!" *parse-execution-count*))
  (incf *parse-execution-count*)
  (prog1
      (call-next-method)
    #+debug(format t "Tokenizer state after: ~a~%" (bt-tokenizer:dump-internal-state tokenizer))
    #+debug(format t "~&End parsing construct ~a.~%" construct-obj)))

(defmethod parse-construct ((construct-obj constr::sequence-construct) tokenizer parse-tree)
  (loop for child across (constr::child-constructs construct-obj)
        for result = (parse-construct child tokenizer parse-tree)
        while (eq result :ok)
        finally (return result)))

(defmethod parse-construct ((construct-obj constr::or-construct) tokenizer parse-tree)
  (bt-tokenizer:mark-backtracking-position tokenizer construct-obj)
  (loop for child across (constr::child-constructs construct-obj)
        for result = (parse-construct child tokenizer parse-tree)
        if (eq result :ok)
          do (bt-tokenizer:unmark-backtracking-position tokenizer construct-obj)
             (return result)
        else
          do (bt-tokenizer:rewind-token-position tokenizer construct-obj)
             ;; TODO: check if need to rewind in the FINALLY clause (meaning no match found, stopping at
             ;; start position, or should we keep at current position? Should be clear when I implement
             ;; actual parsing.
        finally (return result)))

(defmethod parse-construct ((construct-obj constr::token-construct) tokenizer parse-tree)
  (let ((expected-token (constr::token construct-obj))
        #+nil(actual-token (bt-tokenizer:get-token tokenizer)))
    (multiple-value-bind (matched-token status slice-indices)
        (bt-tokenizer::match-token tokenizer expected-token)
      status)))

(defmethod parse-construct ((construct-obj constr::one-or-more-construct) tokenizer parse-tree)
  (let* ((child (constr::child-construct construct-obj))
         (result1 (parse-construct child tokenizer parse-tree)))
    (if (eq result1 :ok)
        (loop for result = (progn
                             (bt-tokenizer:mark-backtracking-position tokenizer child)
                             (parse-construct child tokenizer parse-tree))
              if (eq result :ok)
                do (bt-tokenizer:unmark-backtracking-position tokenizer child)
              else
                do (progn
                     (bt-tokenizer:rewind-token-position tokenizer child)
                     (bt-tokenizer:unmark-backtracking-position tokenizer child)
                     (return result))
              finally (return :ok))
        result1)))

(defmethod parse-construct ((construct-obj constr::zero-or-more-construct) tokenizer parse-tree)
  (bt-tokenizer:mark-backtracking-position tokenizer construct-obj)
  (loop with child = (constr::child-construct construct-obj)
        for result = (progn
                       (bt-tokenizer:mark-backtracking-position tokenizer child)
                       (parse-construct child tokenizer parse-tree))
        if (eq result :ok)
          do (bt-tokenizer:unmark-backtracking-position tokenizer child)
        else
          do (progn
               (bt-tokenizer:rewind-token-position tokenizer child)
               (bt-tokenizer:unmark-backtracking-position tokenizer child)
               (return :ok))
        finally (return :ok)))

(defmethod parse-construct ((construct-obj constr::zero-or-one-construct) tokenizer parse-tree)
  ;; what about putting this in :before? (TODO: CHECK!)
  (bt-tokenizer:mark-backtracking-position tokenizer construct-obj)
  (unless (eq (parse-construct (constr::child-construct construct-obj) tokenizer parse-tree) :ok)
    (bt-tokenizer:rewind-token-position tokenizer construct-obj))
  ;; I think  we unmark backtracking position, and return success even if parsing failed
  ;; (since construct is optional)
  (bt-tokenizer:unmark-backtracking-position tokenizer construct-obj)
  :ok)
