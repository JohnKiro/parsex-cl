(in-package :parsex-cl.regex.input)

;;;; Generic interface for regex source (input), and basic implementation for it

(defgeneric source-empty-p (source)
  (:documentation "Predicate that returns t in case no more items could be read from SOURCE."))

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
input. In case the accumulated value is not interesting, then the implementation of this method
could be left out."))

(defclass basic-regex-input ()
  ((input-text :reader input-text
               :type string)
   (reading-position :reader reading-position
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
  (with-slots (input-text reading-position total-length accumulator-start) source
    (if initial-input-text
        (setf input-text (if make-copy
                             (copy-seq initial-input-text)
                             initial-input-text))
        (error "Must provide valid string in INITIAL-INPUT-TEXT"))
    (setf reading-position initial-reading-position)
    (setf total-length (length initial-input-text))
    (setf accumulator-start initial-reading-position)))

(defmethod source-empty-p ((source basic-regex-input))
  (>= (reading-position source) (length (input-text source))))

(defmethod read-next-item ((source basic-regex-input))
    (char (input-text source) (reading-position source)))

(defmethod advance-reading-position ((source basic-regex-input))
  (with-slots (reading-position total-length) source
    (when (< reading-position total-length)
      (incf reading-position))))

(defmethod notify-match-termination ((source basic-regex-input))
  (with-slots (reading-position candidate-matching-point) source
    (when (>= candidate-matching-point 0)
      (setf reading-position candidate-matching-point)
      (setf candidate-matching-point -1))))

(defmethod register-candidate-matching-point ((source basic-regex-input))
  (setf (slot-value source 'candidate-matching-point)
        (slot-value source 'reading-position)))

(defmethod retrieve-last-accumulated-value ((source basic-regex-input))
  (with-slots (input-text accumulator-start reading-position) source
    (prog1
        ;; we know that index won't exceed total-length, so no need to check
        (subseq input-text accumulator-start reading-position)
      (setf accumulator-start reading-position))))
