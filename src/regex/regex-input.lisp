(in-package :parsex-cl/regex/input)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *regex-input-handler-functions*
    '((source-empty-p-fn
       :doc "Predicate that returns t in case no more items could be read from the source.")
      (remaining-length-fn
       :doc "Returns count of remaining items in source. To have a valid contract, it should return
0 (zero) in case `source-empty-p` returns t.")
      (read-next-item-fn
       :doc "Read next item (e.g. char) from source (e.g. string), without advancing the reading
position. In case source is exhausted, throw an error.")
      (advance-reading-position-fn
       :doc "Advance reading position in source.")
      (notify-match-termination-fn
       :doc "Notify source that current matching operation is terminating. Source should prepare
itself for upcoming matching operation.")
      (register-candidate-matching-point-fn
       :doc "Notify source that there is a candidate match at current reading position. Source
should record this for posible backtracking.")
      (retrieve-last-accumulated-value-fn
       :doc "Retrieve last accumulated value, which is the portion of input that was last
matched. When nothing is accumulated, this method should return NIL, to indicate to the caller that
the REGEX did not consume any characters, and hence, it might be a good indication to stop matching
loops, to avoid going into infinite loops. This of course does not necessarily mean that the input
is empty.  In case the accumulated value is not interesting, then the implementation of this method
could return something like SOMETHING or NIL (to distinguish between the case of accumulation/ no
accumulation).")
      (retrieve-last-consumed-value-fn
       :doc "Retrieve the last consumed portion of the input. Unlike
`retrieve-last-accumulated-value`, this one returns the consumed characters, whether or not they
correspond to a successful match.  Similarly to the `retrieve-last-accumulated-value`, it should
return NIL in case no characters were consumed."))
    "List of slots that will be used in creation of the regex input handler class. It respects the
  format expected by the `define-class-of-functions` macro."))

(class-util:define-class-of-functions-with-constructor regex-input-handler ()
  :doc "Class that represents the functions that handle the internal state of a regex input source.
None of the functions take any arguments, rather, the internal input source state is supposed to be
encapsulated and shared by all functions. Also refer to the documentation string of each function."
  :slots #.*regex-input-handler-functions*
  :constructor-name make-regex-input-handler)

(defun create-basic-regex-input (initial-input-text &key
                                                      (reading-position 0)
                                                      (make-copy nil)
                                                      (advance-on-no-consumption-on-match t)
                                                      (advance-on-no-consumption-on-no-match t))
  "Basic implementation for regex input, based on a string + reading position (index). It also
includes an accumulator for the current/last matching operation, and allows customizing consumption
(whether to consume on match, whether to consume /on no match)."
  (declare (type string initial-input-text)
           (type fixnum reading-position))
  (when (minusp reading-position)
    (error "Reading position cannot be negative!"))
  (let ((source (if make-copy
                    (copy-seq initial-input-text)
                    initial-input-text))
        (starting-reference-position reading-position)
        ;; TODO: shouldn't we exclude (subtract) initial reading position?
        (total-length (length initial-input-text))
        (accumulator-start -1)
        (accumulator-end -1)
        ;; I depend on the fact that definite termination is also candidate termination, so we
        ;; can assume that this will hold value of last matching position (whether last candidate
        ;; or current position). TODO: may rethink about this later.
        (candidate-matching-point -1))
    (make-regex-input-handler
     :source-empty-p-fn (>= reading-position total-length)
     :remaining-length-fn (let ((remaining (- total-length reading-position)))
                            (max remaining 0))
     :read-next-item-fn (char source reading-position)
     :advance-reading-position-fn (incf reading-position)
     :notify-match-termination-fn (progn
                                    (setf accumulator-start starting-reference-position)
                                    (setf accumulator-end candidate-matching-point)
                                    ;; non-negative candidate-matching-point implies regex match
                                    (if (>= candidate-matching-point 0)
                                        (progn
                                          ;;if no chars consumed
                                          (if (= candidate-matching-point
                                                 starting-reference-position)
                                              ;; no char consumed, => inc position conditionally
                                              ;; (based on flag)
                                              (when advance-on-no-consumption-on-match
                                                (incf starting-reference-position))
                                              (setf starting-reference-position
                                                    candidate-matching-point))
                                          (setf candidate-matching-point -1))
                                        ;;negative candidate-matching-point implies regex no-match
                                        (when advance-on-no-consumption-on-no-match
                                          (incf starting-reference-position)))
                                    (setf reading-position starting-reference-position))
     :register-candidate-matching-point-fn (setf candidate-matching-point reading-position)
     :retrieve-last-accumulated-value-fn (when (< accumulator-start accumulator-end)
                                           (subseq source accumulator-start accumulator-end))
     :retrieve-last-consumed-value-fn (when (< accumulator-start starting-reference-position)
                                        (subseq source accumulator-start
                                                starting-reference-position)))))
