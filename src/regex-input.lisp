(in-package :parsex-cl.regex.input)

;;;; Generic interface for regex source (input), and basic implementation for it

(defgeneric source-empty-p (source)
  (:documentation "Predicate that returns t in case no more items could be read from SOURCE."))

(defgeneric remaining-length (source)
  (:documentation "Returns count of remaining items in source. To have a valid contract, it should
return 0 (zero) in case SOURCE-EMPTY-P returns t."))

(defgeneric read-next-item (source)
  (:documentation "Read next item (e.g. cha) from SOURCE (e.g. string), without advancing reading
 position. In case source is exhausted, throw an error."))

(defgeneric advance-reading-position (source)
  (:documentation "Advance reading position in SOURCE."))

(defgeneric notify-match-termination (source)
  (:documentation "Notify source that current matching operation is terminating. Source should
prepare itself for upcoming matching operation."))

(defgeneric register-candidate-matching-point (source)
  (:documentation "Notify source that there is a candidate match at current reading position.
Source should record this for posible backtracking."))

(defgeneric retrieve-last-accumulated-value (source)
  (:documentation "Retrieve last accumulated value, which is the last consumed portion of the
input. When nothing is accumulated, this method should return NIL, to indicate to the caller that
the REGEX did not consume any characters, and hence, it's a good indication to stop matching loops,
to avoid going into infinite loops. This of course does not necessarily mean that the input is
empty. In case the accumulated value is not interesting, then the implementation of this method
could return something like SOMETHING or NIL (to distinguish between the case of accumulation/
no accumulation)."))

(defclass basic-regex-input ()
  ((input-text :reader input-text
               :type string)
   (reading-position :reader reading-position
                     :type fixnum)
   (starting-reference-position :reader starting-reference-position
                                :type fixnum)
   (total-length :reader total-length
                 :type fixnum)
   (accumulator-start :reader accumulator-start
                      :type fixnum)
   ;; I depend on the fact that definite termination is also candidate termination, so we
   ;; can assume that this will hold value of last matching position (whether last candidate
   ;; or current position). TODO: may rethink about this later.
   (candidate-matching-point :reader candidate-matching-point
                             :type fixnum
                             :initform -1))
  (:documentation "Basic implementation for regex input, based on a string + reading position
(index). It also includes an accumulator for the current/last matching operation."))

(defmethod initialize-instance :after ((source basic-regex-input) &key
                                                                    initial-input-text
                                                                    (initial-reading-position 0)
                                                                    (make-copy nil))
  (when (minusp initial-reading-position)
    (error "Initial reading position cannot be negative!"))
  (with-slots (input-text
               reading-position
               total-length
               accumulator-start
               starting-reference-position) source
    (if initial-input-text
        (setf input-text (if make-copy
                             (copy-seq initial-input-text)
                             initial-input-text))
        (error "Must provide valid string in INITIAL-INPUT-TEXT"))
    (setf reading-position initial-reading-position)
    (setf starting-reference-position initial-reading-position)
    (setf total-length (length initial-input-text))
    (setf accumulator-start initial-reading-position)))

(defmethod source-empty-p ((source basic-regex-input))
  (>= (reading-position source) (length (input-text source))))

(defmethod remaining-length ((source basic-regex-input))
  (let ((remaining (- (length (input-text source)) (reading-position source))))
    (max remaining 0)))

(defmethod read-next-item ((source basic-regex-input))
    (char (input-text source) (reading-position source)))

(defmethod advance-reading-position ((source basic-regex-input))
  (with-slots (reading-position total-length) source
    ;;TODO: remove this check (it's ok to advance beyond end, as we'll check elsewhere)
    (when (< reading-position total-length)
      (incf reading-position))))

(defmethod notify-match-termination ((source basic-regex-input))
  (with-slots (reading-position candidate-matching-point starting-reference-position) source
    ;; TODO: I'm now keeping track of starting position of each matching operation, so I set
    ;; this reference position here: in case of match success, then next operation starts at
    ;; reading pos, otherwise, we start at the current match start + 1 char
    ;; (rethink it)!!!
    (if (>= candidate-matching-point 0)
        (progn
          (setf reading-position candidate-matching-point)
          (setf starting-reference-position candidate-matching-point)
          (setf candidate-matching-point -1))
        (setf reading-position (incf starting-reference-position)))))

(defmethod register-candidate-matching-point ((source basic-regex-input))
  (setf (slot-value source 'candidate-matching-point)
        (slot-value source 'reading-position)))

(defmethod retrieve-last-accumulated-value ((source basic-regex-input))
  (with-slots (input-text accumulator-start reading-position) source
    (if (< accumulator-start reading-position)
        (prog1
            (subseq input-text accumulator-start reading-position)
          (setf accumulator-start reading-position))
        nil)))
