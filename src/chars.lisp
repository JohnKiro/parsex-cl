(in-package :parsex-cl.chars)

;;;; Utility functions and classes for character handling

(deftype simple-element () '(or character char-range (eql :any-char)))

(defun inc-char (ch)
  "Increment character CH, i.e. find the following one."
  (code-char (1+ (char-code ch))))

(defun dec-char (ch)
  "Decrement character CH, i.e. find the preceding one."
  (code-char (1- (char-code ch))))


(defclass char-range ()
  ((char-start :initarg :char-start :initform (error "Mandatory")
               :reader char-start :type character)
   (char-end :initarg :char-end :initform (error "Mandatory")
             :reader char-end :type character))
  (:documentation "Char range regex element."))

(defmethod print-object ((object char-range) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (with-slots ((s char-start) (e char-end)) object
      (format stream "(~a - ~a)" s e ))))

(defmethod initialize-instance :after ((char-range char-range) &key)
  (with-slots (char-start char-end) char-range
    (if (char> char-start char-end)
        (let ((tmp char-start))
          (setf char-start char-end)
          (setf char-end tmp)))))

(defun char-range-equal (chr1 chr2)
  (and (char= (char-start chr1) (char-start chr2))
       (char= (char-end chr1) (char-end chr2))))

(defun simple-element-equal (element other-obj)
  "Equality test for all three types of simple elements (character, char-range, symbol)."
  (or (eq element other-obj)
      (typecase element
        (character (and (typep other-obj 'character)
                        (char= element other-obj)))
        (char-range (and (typep other-obj 'char-range)
                               (char-range-equal element other-obj))))))

(defun make-char-range (start end)
  "Utility function to simplify char-range construction."
  (make-instance 'char-range :char-start start :char-end end))


;;; TODO: may use other data structures later.
(defun split-char-range (char-range splitting-points)
  "Split a char range CHAR-RANGE into a number of ranges based on SPLITTING-POINTS (vector of
characters).
Preconditions: end >= start, splitting points must be a sorted vector."
  (let ((slider (char-start char-range))
        (end (char-end char-range))
        (acc nil))
    (loop for p across splitting-points
      ;; skip splitting points < slider (effective at start, till splitting points start to
      ;; overlap with the range at hand, noting that splitting points are assumed sorted).
      ;; However, since we're doing this check for all splitting points, those that are not in
      ;; order will be skipped. Alternatively, I may have separate loop to skip points < start.
          do (when (char>= p slider)
               (if (char< p end)
                   (progn
                     (push (make-instance 'char-range :char-start slider :char-end p) acc)
                     (setf slider (inc-char p)))
                   (progn
                     ;; add last range
                     (push (make-instance 'char-range :char-start slider :char-end end) acc)
                     ;; break loop (since any remaining splitting points will be beyond range
                     (return)))))
    ;; return accumulator, containing all sub-ranges
    acc))
