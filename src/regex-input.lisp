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
  (:documentation "Retrieve last accumulated value, which is the portion of input that was last
matched. When nothing is accumulated, this method should return NIL, to indicate to the caller that
the REGEX did not consume any characters, and hence, it might be a good indication to stop matching
loops, to avoid going into infinite loops. This of course does not necessarily mean that the input
is empty. In case the accumulated value is not interesting, then the implementation of this method
could return something like SOMETHING or NIL (to distinguish between the case of accumulation/
no accumulation)."))

(defgeneric retrieve-last-consumed-value (source)
  (:documentation "Retrieve the last consumed portion of the input. Unlike
RETRIEVE-LAST-ACCUMULATED-VALUE, this one returns the consumed characters, whether or not they
correspond to a successful match. Similarly to the RETRIEVE-LAST-ACCUMULATED-VALUE, it should
return NIL in case no characters were consumed."))

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
                      :type fixnum
                      :initform -1)
   (accumulator-end :reader accumulator-end
                    :type fixnum
                    :initform -1)
   ;; I depend on the fact that definite termination is also candidate termination, so we
   ;; can assume that this will hold value of last matching position (whether last candidate
   ;; or current position). TODO: may rethink about this later.
   (candidate-matching-point :reader candidate-matching-point
                             :type fixnum
                             :initform -1)
   (advance-on-no-consumption-on-match :type boolean
                                       :initarg :advance-on-no-consumption-on-match
                                       :initform t)
   (advance-on-no-consumption-on-no-match :type boolean
                                          :initarg :advance-on-no-consumption-on-no-match
                                          :initform t))
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
               starting-reference-position) source
    (if initial-input-text
        (setf input-text (if make-copy
                             (copy-seq initial-input-text)
                             initial-input-text))
        (error "Must provide valid string in INITIAL-INPUT-TEXT"))
    (setf reading-position initial-reading-position)
    (setf starting-reference-position initial-reading-position)
    (setf total-length (length initial-input-text))))

(defmethod source-empty-p ((source basic-regex-input))
  (>= (reading-position source) (total-length source)))

(defmethod remaining-length ((source basic-regex-input))
  (let ((remaining (- (total-length source) (reading-position source))))
    (max remaining 0)))

(defmethod read-next-item ((source basic-regex-input))
    (char (input-text source) (reading-position source)))

(defmethod advance-reading-position ((source basic-regex-input))
  (with-slots (reading-position total-length) source
    (incf reading-position)))

(defmethod notify-match-termination ((source basic-regex-input))
  (with-slots (reading-position
               candidate-matching-point
               accumulator-start
               accumulator-end
               starting-reference-position
               advance-on-no-consumption-on-match
               advance-on-no-consumption-on-no-match) source
    (setf accumulator-start starting-reference-position)
    (setf accumulator-end candidate-matching-point)
    ;; non-negative candidate-matching-point implies regex match
    (if (>= candidate-matching-point 0)
        (progn
          ;;if no chars consumed
          (if (= candidate-matching-point starting-reference-position)
              ;; no char consumed, => inc position conditionally (based on flag)
              (when advance-on-no-consumption-on-match
                (incf starting-reference-position))
              (setf starting-reference-position candidate-matching-point))
          (setf candidate-matching-point -1))
        ;;negative candidate-matching-point implies regex no-match
        (when advance-on-no-consumption-on-no-match
          (incf starting-reference-position)))
    (setf reading-position starting-reference-position)))

(defmethod register-candidate-matching-point ((source basic-regex-input))
  (setf (slot-value source 'candidate-matching-point)
        (slot-value source 'reading-position)))

(defmethod retrieve-last-accumulated-value ((source basic-regex-input))
  (with-slots (input-text accumulator-start accumulator-end) source
    (if (< accumulator-start accumulator-end)
        (subseq input-text accumulator-start accumulator-end)
        nil)))

(defmethod retrieve-last-consumed-value ((source basic-regex-input))
  (with-slots (input-text accumulator-start starting-reference-position) source
    (if (< accumulator-start starting-reference-position)
        (subseq input-text accumulator-start starting-reference-position)
        nil)))
