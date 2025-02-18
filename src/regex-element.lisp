(in-package :parsex-cl.regex-element)

;;; Definition of regex elements

#||
Other element types not needing special class:
- symbols (currently only :any-char (corresponds to . in regex).
- single character (for now, I'm encapsulating it in single-char-element, mainly to group
  it together with char-range-element as simple-element).
- string (string of character corresponds to a sequence-element where all elements are characters).
||#

(defconstant +any-char-element+ :any-char
  "Centralized reference to any ANY-CHAR element. This corresponds to '.' (dot) in conventional
regular expressions. For example, for the regex fragment (:or (:seq #\a #\b) (seq :any-char #\x)),
the :any-char would match any char, including #\a.")

(defconstant +any-other-char-element+ :any-other-char
  "Centralized reference to any ANY-OTHER-CHAR element. This element is usually generated during
negation, and for now, I'm not allowing at for the user. Later, I may allow it, so that user could
define a regex like (or #\a (seq any-other-char #\x)). In such case, 'bx' would match, but not
'ax'.")

;; TODO: remove (no need for base abstract)
(defclass element () ()
  (:documentation "Base class for all regex element types."))

;;; Single character elements (range, any-char, specific char)
(defclass simple-element (element) ()
  (:documentation "Base class for single char regex elements (single char and char range)."))

;;; TODO: probably remove (use character directly). The problem is that I would have two
;;; identical produce-nfa methods :(
;;; Also may need an object, to include some properties, such as "accepted token" (not sure).
(defclass single-char-element (simple-element)
  ((single-char :initarg :single-char :initform (error "Mandatory")
                :reader single-char :type character))
  (:documentation "Wrapper object for single-char elements."))

;; TODO: add constructor that ensures char-end >= char-start
(defclass char-range-element (simple-element)
  ((char-start :initarg :char-start :initform (error "Mandatory")
               :reader char-start :type character)
   (char-end :initarg :char-end :initform (error "Mandatory")
             :reader char-end :type character))
  (:documentation "Char range regex element."))

(defmethod print-object ((object single-char-element) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "(~a)" (single-char object))))

(defmethod print-object ((object char-range-element) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (with-slots ((s char-start) (e char-end)) object
      (format stream "(~a - ~a)" s e ))))

(defmethod initialize-instance :after ((element char-range-element) &key)
  (with-slots (char-start char-end) element
    (when (char> char-start char-end)
      (let ((tmp char-start))
        (setf char-start char-end)
        (setf char-end tmp)))))

(defun char-range-equal (chr1 chr2)
  (and (char= (char-start chr1) (char-start chr2))
       (char= (char-end chr1) (char-end chr2))))

(defun match-char-against-simple-element (ch elem)
  "Matches a character CH against a simple element (single char/char range)."
  (etypecase elem
    (single-char-element (char= ch (single-char elem)))
    (char-range-element (and (char>= ch (char-start elem))
                             (char<= ch (char-end elem))))))

;; TODO: cleanup/simplify
(defun simple-element-equal (element other-obj)
  "Equality test for all types of simple elements (single char, char-range, symbol)."
  (or (eq element other-obj)
      (etypecase element
        (single-char-element (and (typep other-obj 'single-char-element)
                                  (char= (single-char element) (single-char other-obj))))
        (char-range-element (and (typep other-obj 'char-range-element)
                                 (char-range-equal element other-obj)))
        ;; since the above EQ evaluated to nil, we're sure no equality
        (symbol nil))))

(defun make-char-range (start end)
  "Utility function to simplify char-range-element construction."
  (make-instance 'char-range-element :char-start start :char-end end))


;;; TODO: may use other data structures later.
(defun split-char-range (char-range-element splitting-points)
  "Split a char range CHAR-RANGE-ELEMENT into a number of ranges based on SPLITTING-POINTS (vector
of characters).
Preconditions: end >= start, splitting points must be a sorted vector."
  (let ((slider (char-start char-range-element))
        (end (char-end char-range-element))
        (acc nil))
    (loop for p across splitting-points
      ;; skip splitting points < slider (effective at start, till splitting points start to
      ;; overlap with the range at hand, noting that splitting points are assumed sorted).
      ;; However, since we're doing this check for all splitting points, those that are not in
      ;; order will be skipped. Alternatively, I may have separate loop to skip points < start.
          do (when (char>= p slider)
               (if (char< p end)
                   (progn
                     (push (make-instance 'char-range-element :char-start slider :char-end p) acc)
                     (setf slider (chars:inc-char p)))
                   (progn
                     ;; add last range
                     (push (make-instance 'char-range-element :char-start slider :char-end end) acc)
                     ;; break loop (since any remaining splitting points will be beyond range
                     (return)))))
    ;; return accumulator, containing all sub-ranges
    acc))

(defclass single-element-wrapper-element ()
  ((element :initarg :element :initform (error "Mandatory") :reader inner-element)))

(defclass negated-element (single-element-wrapper-element) ())

(defclass zero-or-more-element (single-element-wrapper-element) ())

(defclass zero-or-one-element (single-element-wrapper-element) ())

;;; TODO: for now, not used (decide if needed)
(defclass one-or-more-element (single-element-wrapper-element) ())

;;; any-char, corresponds to "." in regular expressions.
;;; not sure whether I pass it as symbol (.) or map to this class (see note below)
;;; TODO: for now, not used (decide if needed)
(defclass any-char-element (simple-element) ())

;;; TODO: not currently used (actually instead, it's a parameter in DFA state)
(defclass any-other-char-element (simple-element) ())

(defclass multiple-elements-wrapper-element (element)
  ((elements :initarg :elements :initform nil :reader inner-elements :type sequence)))

(defclass sequence-element (multiple-elements-wrapper-element) ())

(defclass or-element (multiple-elements-wrapper-element) ())

(defclass inv-element (multiple-elements-wrapper-element) ()
  (:documentation "Equivalent to the caret (^) inside a square bracket pair in conventional regular
expressions. It matches any character other than the ones given in the inner elements.
Note: the inner elements have to be of type char/char-range."))
