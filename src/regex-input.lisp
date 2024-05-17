(in-package :parsex-cl.regex.input)

;;;; Generic interface for regex source (input), and basic implementation for it

(defun create-basic-input (input-text &key (initial-reading-index 0) (make-copy nil))
  (declare (string input-text)
           (fixnum initial-reading-index))
  (let* ((index initial-reading-index)
         (accumulator-start initial-reading-index)
         (total-length (length input-text))
         (text (if make-copy
                   (copy-seq input-text)
                   input-text))
         ;; I depend on the fact that definite termination is also candidate termination, so we
         ;; can assume that this will hold value of last matching position (whether last candidate
         ;; or current position). TODO: may rethink about this later.
         (candidate-matching-point -1)
         (input-empty-predicate (lambda ()
                                  (>= index total-length)))
         (read-input-fn (lambda ()
                          (char text index)))
         (advance-input-fn (lambda ()
                             (when (< index total-length)
                               (incf index))))
         (register-candidate-matching-point-fn (lambda ()
                                                 (setf candidate-matching-point index)))
         (notify-matching-termination-fn (lambda ()
                                           (when (>= candidate-matching-point 0)
                                             (setf index candidate-matching-point)
                                             (setf candidate-matching-point -1))))
         (retrieve-last-accumulated-value-fn (lambda ()
                                               ;; we know that index won't exceed total-length,
                                               ;; so no need to check
                                               (prog1
                                                   (subseq text accumulator-start index)
                                                 (setf accumulator-start index)))))

    (lambda ()
      (values input-empty-predicate
              read-input-fn
              advance-input-fn
              register-candidate-matching-point-fn
              notify-matching-termination-fn
              retrieve-last-accumulated-value-fn))))



