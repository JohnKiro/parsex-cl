(in-package :parsex-cl.regex.input)

;;;; Generic interface for regex source (input), and basic implementation for it

(defun create-basic-input (input-text &key (initial-reading-index 0) (make-copy nil))
  (declare (string input-text)
           (fixnum initial-reading-index))
  (let* ((index initial-reading-index)
         (text (if make-copy
                   (copy-seq input-text)
                   input-text))
         (input-empty-predicate (lambda ()
                                  (>= index (length text))))
         (read-input-fn (lambda ()
                          (char text index)))
         (advance-input-fn (lambda ()
                             (incf index)))
         (register-candidate-matching-point-fn (lambda ()
                                                 'not-yet-implemented))
         (notify-matching-termination-fn (lambda ()
                                           'not-yet-implemented)))
    (lambda ()
      (values input-empty-predicate
              read-input-fn
              advance-input-fn
              register-candidate-matching-point-fn
              notify-matching-termination-fn))))


;;; TODO: may make a reusable OOP framework like in PAIP ch. 13
(defun create-basic-accumulator (&key (initial-size 20))
  (let* ((accumul (make-array initial-size :element-type 'character
                                           :adjustable t :fill-pointer 0))
         (retrieve-accumulator-value-fn (lambda () accumul))
         (append-to-accumulator-fn (lambda (char)
                                     (vector-push-extend char accumul))))
    (lambda ()
      (values retrieve-accumulator-value-fn append-to-accumulator-fn))))




