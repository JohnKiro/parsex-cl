(in-package :parsex-cl.chars)

;;; Utility function to increment a character
(defun inc-char (ch)
  "Increment character CH, i.e. find the next one."
  (code-char (1+ (char-code ch))))

;;; Utility function to decrement a character
;;; TODO: utils package.
(defun dec-char (ch)
  "Decrement character CH, i.e. find the preceding one."
  (code-char (1- (char-code ch))))


;; TODO: add constructor that ensures char-end >= char-start
(defclass char-range ()
  ((char-start :initarg :char-start :initform (error "Mandatory")
               :reader char-start :type character)
   (char-end :initarg :char-end :initform (error "Mandatory")
             :reader char-end :type character))
  (:documentation "Char range regex element."))

(defmethod print-object ((object char-range) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (with-slots ((s char-start) (e char-end)) object
      (format stream "(~a ---- ~a)" s e ))))

;;; TODO: simplify?
(defun insert-char-in-order (char chars)
  "Destructively insert character CHAR in a sorted list CHARS, maintaining ascending order. Since
CHAR might be inserted ahead of CHARS, the returned pointer must be saved (as is usually the case
for list-destructive operations)."
  (labels ((insert-it-recurse (previous-pointer)
             ;;NOTE: returned value actually not used
             (let ((current-pointer (cdr previous-pointer)))
               (if (null current-pointer)
                   (setf (cdr previous-pointer) (cons char nil))
                   (let ((char-iter (car current-pointer)))
                     (cond
                       ((char> char char-iter) (insert-it-recurse current-pointer))
                       ((char= char char-iter) current-pointer)
                       (t (let ((new-cell (cons char current-pointer)))
                            (setf (cdr previous-pointer) new-cell)))))))))
    (cond
      ((null chars) (cons char nil))
      ((char< char (car chars)) (cons char chars))
      ((char= char (car chars)) chars)
      (t (insert-it-recurse chars) chars))))

;;; TODO: may use other data structures later.
(defun split-char-range (char-range splitting-points)
  "Split a char range CHAR-RANGE into a number of ranges based on SPLITTING-POINTS (list of
characters).
Preconditions: end >= start, splitting points must be a sorted list."
  (with-slots ((start char-start) (end char-end)) char-range
    (labels
        ((split-range-recurse (next-start splitting-points acc)
           (cond
             ((char> next-start end) acc) ;happens only if start and end are mixed up
             ((null splitting-points) (cons (make-instance 'char-range
                                                           :char-start next-start
                                                           :char-end end) acc))
             (t (let ((next-splitting-pt (car splitting-points))
                      (subsequent-splitting-pts (cdr splitting-points)))
                  (cond
                    ((char>= next-splitting-pt end) (cons (make-instance 'char-range
                                                                         :char-start next-start
                                                                         :char-end end) acc))
                    ((char>= next-splitting-pt next-start) (split-range-recurse
                                                            (inc-char next-splitting-pt)
                                                            subsequent-splitting-pts
                                                            (cons
                                                             (make-instance
                                                              'char-range
                                                              :char-start next-start
                                                              :char-end next-splitting-pt)
                                                             acc)))
                    (t (split-range-recurse next-start subsequent-splitting-pts acc))))))))
      (split-range-recurse start splitting-points nil))))
