(in-package :parsex-cl/regex/element)

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
  "Centralized reference to any ANY-OTHER-CHAR element. This element is currently generated during
negation and inversion, and for now, I'm not allowing it for the user. Later, I may allow it, so
that user could define a regex like (or #\a (seq any-other-char #\x)). In such case, 'bx' would
match, but not 'ax'.")

(defclass simple-element () ()
  (:documentation "Base class for single char regex elements (single char and char range)."))

;;; TODO: probably remove (use character directly). The problem is that I would have two
;;; identical produce-nfa methods :(
;;; Also may need an object, to include some properties, such as "accepted token" (not sure).
(defclass single-char-element (simple-element)
  ((%single-char :initarg :single-char :initform (error "Mandatory")
                 :reader single-char :type character))
  (:documentation "Wrapper object for single-char elements."))

(deftype char-range-left-type ()
  "Type for left-side of a char range (either a character, or keyword :min)."
  '(or character (eql :min)))

(deftype char-range-right-type ()
  "Type for right-side of a char range (either a character, or keyword :max)."
  '(or character (eql :max)))

(defclass char-range-element (simple-element)
  ((%char-start :initarg :char-start :initform (error "Mandatory")
                :reader char-start :type char-range-left-type)
   (%char-end :initarg :char-end :initform (error "Mandatory")
              :reader char-end :type char-range-right-type))
  (:documentation "Char range regex element. Either or both ends may be open.
For example, if char-start is :min, then then range covers all characters less than or equal to
 end-char."))

(defmethod print-object ((object single-char-element) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "(~a)" (single-char object))))

(defmethod print-object ((object char-range-element) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (with-slots ((s %char-start) (e %char-end)) object
      (format stream "(~a - ~a)" s e))))

(defmethod initialize-instance :after ((element char-range-element) &key
                                                                      (char-start :min)
                                                                      (char-end :max))
  "Validate range (start must less than or equal to end)."
  (declare (type char-range-left-type char-start)
           (type char-range-right-type char-end))
  (unless (or (keywordp char-start) (keywordp char-end))
    (when (char> char-start char-end)
      (error "Invalid character range initialization (start must be less than or equal to end)!")))
  (with-slots ((s %char-start) (e %char-end)) element
    (setf s char-start
          e char-end)))

(defun char-range-equal (char-range-1 char-range-2)
  "Equality test for two char range elements."
  (or (and char-range-1 (eql char-range-1 char-range-2))
      (with-slots ((s1 %char-start) (e1 %char-end)) char-range-1
        (with-slots ((s2 %char-start) (e2 %char-end)) char-range-2
               (and (eql s1 s2) (eql e1 e2))))))

(defun match-char-against-simple-element (ch elem)
  "Matches a character CH against a simple element (single char/char range)."
  (etypecase elem
    (single-char-element (char= ch (single-char elem)))
    (char-range-element (with-slots ((s %char-start) (e %char-end)) elem
                          (and (or (eql s :min) (char>= ch s))
                               (or (eql e :max) (char<= ch e)))))))

;; TODO: cleanup/simplify
(defun simple-element-equal (element other-obj)
  "Equality test for all types of simple elements (single char, char-range, symbol)."
  (or (eq element other-obj)
      (etypecase element
        (#1=single-char-element (and (typep other-obj '#1#)
                                     (char= (single-char element) (single-char other-obj))))
        (#2=char-range-element (and (typep other-obj '#2#)
                                    (char-range-equal element other-obj)))
        ;; since the above EQ evaluated to nil, we're sure no equality
        (symbol nil))))

(defun simple-element-before (element1 element2)
  "Compare two simple elements according to character order (character/start character). Note that
we do not care about element range end."
  (let ((c1 (etypecase element1
              (single-char-element (single-char element1))
              (char-range-element (char-start element1))))
        (c2 (etypecase element2
              (single-char-element (single-char element2))
              (char-range-element (char-start element2)))))
    (or (eq c1 :min)
        (and (not (eq c2 :min))
             (char<= c1 c2)))))

(defmacro make-char-range (&key (start :min) (end :max))
  "Utility macro to simplify char-range-element construction."
  `(make-instance 'char-range-element :char-start ,start :char-end ,end))

;;; TODO: may use other data structures later.
(defun split-char-range (char-range-element splitting-points)
  "Split a char range CHAR-RANGE-ELEMENT into a number of ranges based on SPLITTING-POINTS (vector
of characters).
Preconditions: splitting points must be a sorted vector.
Note: it takes into consideration half-open/full-open ranges."
  (format t "DEBUG: splitting range ~a on characters ~s..~&" char-range-element splitting-points)
  (let ((slider (char-start char-range-element))
        (end (char-end char-range-element))
        (acc nil))
    (loop for p across splitting-points
          ;; skip splitting points < slider (effective at start, till splitting points start to
          ;; overlap with the range at hand, noting that splitting points are assumed sorted).
          ;; TODO: some optimization is possible, since p will remain >= slider (once overlap
          ;; is detected), so we need to do this check only once (in a separate loop to find
          ;; first overlapping splitting point)
          do (when (or (eql slider :min) (char>= p slider))
               (if (or (eql end :max) (char< p end))
                   (progn
                     (push (make-instance 'char-range-element :char-start slider :char-end p) acc)
                     (setf slider (chars:inc-char p)))
                   ;; break loop (since any remaining splitting points will be beyond range
                   (return))))
      ;; add last range
      (push (make-instance 'char-range-element :char-start slider :char-end end) acc)
    ;; return accumulator, containing all sub-ranges
    (format t "DEBUG: Range split into: ~a~&" acc)
    acc))

(defun sort-simple-elements (elements)
  "Sort a sequence of simple elements `elements` according to character (if element is single char)
or start character (if element is char range).
Note: this function is destructive. I.e. it modifies the passed sequence."
  (sort elements #'simple-element-before))

(defun invert-elements%% (elements)
  "Invert (complement) elements, by finding elements that correspond to characters that do not
belong to any of the elements provided. This operation is used in regex negation.
Note: it assumes elements are sorted, with no overlaps between them. To guarantee this, user could
use the range splitting function to remove overlaps, then use `sort-simple-elements`, before calling
this function.
TODO: vector instead of list.
TODO: for ranges containing a single char, no need to return a range object (single char instead).
NOTE: THIS IS THE PREVIOUS VERSION (NOW OBSOLETE, KEPT FOR NOW FOR REFERENCE)."
  (labels ((find-surrounding-chars (elem)
             (etypecase elem
               (single-char-element (let ((ch (single-char elem)))
                                      (values (chars:dec-char ch) (chars:inc-char ch))))
               (char-range-element (let ((s (char-start elem))
                                         (e (char-end elem)))
                                     (values (if (eq s :min)
                                                 nil ;nothing is left to MIN
                                                 (chars:dec-char s))
                                             (if (eq e :max)
                                                 nil ;nothing is right to MAX
                                                 (chars:inc-char e))))))))
    (loop with output = nil ;TODO: may use vector instead of list
          with slider = :min
          for e in elements
          do (multiple-value-bind (left-char right-char) (find-surrounding-chars e)
               (when (and left-char slider (char> left-char slider))
                 (push (make-char-range :start slider :end left-char) output))
               (setf slider right-char))
          unless slider do (return output) ; exit early when finding an open-ended range (--> MAX)
          finally (progn
                    (when slider ;actually no need to recheck, since we have "unless" clause in LOOP
                      (push (make-char-range :start slider :end :max) output))
                    (return output)))))

(defun invert-elements (elements)
  "Invert (complement) elements, by finding elements that correspond to characters that do not
belong to any of the elements provided. This operation is used in regex negation.
Note: it assumes elements are sorted, with no overlaps between them. To guarantee this, user could
use the range splitting function to remove overlaps, then use `sort-simple-elements`, before calling
this function.
TODO: vector instead of list.
TODO: for ranges containing a single char, no need to return a range object (single char instead)."
  (loop with output = nil ;TODO: may use vector instead of list
        with slider = :min
        for elem in elements
        do (etypecase elem
             (single-char-element (let* ((ch (single-char elem))
                                         (left-char (chars:dec-char ch))
                                         (right-char (chars:inc-char ch)))
                                    (when (or (and (characterp slider) (char>= left-char slider))
                                              (eq slider :min))
                                      (push (make-char-range :start slider :end left-char) output))
                                    (setf slider right-char)))
             (char-range-element (let* ((s (char-start elem))
                                        (e (char-end elem))
                                        (left-char (if (eq s :min) :min (chars:dec-char s)))
                                        (right-char (if (eq e :max) :max (chars:inc-char e))))
                                   (when (and (characterp left-char) (or (eq slider :min)
                                                                         (char>= left-char slider)))
                                     (push (make-char-range :start slider :end left-char) output))
                                   (setf slider right-char))))
        when (eq slider :max)
          do (return output) ;exit early when finding an open-ended range
        finally (progn
                  ;; handle remaining segment (we're sure there is one, since no early exit done
                  ;; above)
                  (push (make-char-range :start slider :end :max) output)
                  (return output))))
  
(defun invert-simple-element (elem)
  "Computes the inversion of the simple element `elem`. The inversion will result in up to two
ranges: one range before the element (if applicable), and one range after it (if applicable).
The computed ranges are returned as two values, where one or both of them may be NIL.
TODO: probably won't use it (`invert-elements` instead). But may keep it as a utility function."
  (let (s e)
    (etypecase elem
      (single-char-element (setf s (single-char elem)
                                 e s))
      (char-range-element (setf s (char-start elem)
                                e (char-end elem))))
    (values (and (not (eq s :min)) (make-char-range :start :min :end (chars:dec-char s)))
            (And (not (eq e :max)) (make-char-range :start (chars:inc-char e) :end :max )))))

(defclass single-element-wrapper-element ()
  ((%element :initarg :element :initform (error "Mandatory") :reader inner-element)))

(defclass negated-element (single-element-wrapper-element)
  ((%greedy :initarg :greedy
            :initform nil
            :reader greedy-p
            :type boolean)))

(defclass zero-or-more-element (single-element-wrapper-element) ())

(defclass zero-or-one-element (single-element-wrapper-element) ())

;;; TODO: for now, not used (decide if needed)
(defclass one-or-more-element (single-element-wrapper-element) ())

(defclass multiple-elements-wrapper-element ()
  ((%elements :initarg :elements :initform nil :reader inner-elements :type sequence)))

(defclass sequence-element (multiple-elements-wrapper-element) ())

(defclass or-element (multiple-elements-wrapper-element) ())

(defclass inv-element (multiple-elements-wrapper-element) ()
  (:documentation "Equivalent to the caret (^) inside a square bracket pair in conventional regular
expressions. It matches any character other than the ones given in the inner elements.
Note: the inner elements have to be of type char/char-range."))
