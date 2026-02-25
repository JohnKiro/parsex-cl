(in-package :parsex-cl/rdp/grammar/constructs)

(defclass grammar-construct ()
  ((%construct-id :initarg :construct-id
                  :initform nil
                  :reader construct-id
                  :type symbol)
   (%first-set :reader first-set
               :type list))
  (:documentation "Abstract base class (generic grammar rule)."))

(defmethod print-object ((construct grammar-construct) stream)
  (print-unreadable-object (construct stream :type t :identity t)
    (with-slots ((id %construct-id)) construct
      (format stream "(ID: ~a)" id))))

(defclass token-construct (grammar-construct)
  ((%token :initarg :token
           :initform (error "Must specify a token upon construction!")
           :reader token)))

(defclass single-construct-wrapper (grammar-construct)
  ((%child-construct :initarg :child-construct
                     :initform nil
                     :reader child-construct
                     :type (or null grammar-construct)))
  (:documentation "An abstract wrapper class that captures the structure of a construct that has one
child construct (to avoid redundancy)."))

(defclass multiple-constructs-wrapper (grammar-construct)
  ((%child-constructs :initarg :child-constructs
                      :initform (make-array 10 :fill-pointer 0 :adjustable t)
                      :reader child-constructs
                      :type vector))
  (:documentation "A wrapper abstract class that captures the structure of a construct that has a set of
child constructs (to avoid redundancy)."))

(defclass one-or-more-construct (single-construct-wrapper)
  ()
  (:documentation "A grammar construct that can be repeated one or more times (e.g. a statements
block that can contain multiple statements)."))

(defclass zero-or-more-construct (single-construct-wrapper)
  ()
  (:documentation "A grammar construct that can be repeated zero or more times (e.g. a statements
block that can contain multiple statements, or can be empty)."))

(defclass zero-or-one-construct (single-construct-wrapper)
  ()
  (:documentation "A grammar construct that can be repeated zero or one times (e.g. an optional else
block)."))

(defclass sequence-construct (multiple-constructs-wrapper)
  ()
  (:documentation "A grammar construct that corresponds to a sequence of constructs."))

(defclass or-construct (multiple-constructs-wrapper)
  ()
  (:documentation "A grammar construct that corresponds to a choice between a set of constructs. TODO:
decide whether order is relevant, is implied or specified with a different flag."))

(defun set-child (grammar-construct child)
  (declare (type single-construct-wrapper grammar-construct)
           (type grammar-construct child))
  (if #1=(slot-value grammar-construct '%child-construct)
      (error "Child already specified during initialization!")
      (setf #1# child)))

(defun add-child (grammar-construct child)
  (declare (type multiple-constructs-wrapper grammar-construct)
           (type grammar-construct child))
  (vector-push-extend child (slot-value grammar-construct '%child-constructs)))

(defgeneric compute-first-set (grammar-construct)
  (:documentation "Compute and return the first set for `grammar-construct`. TODO: won't focus on it
for now, as I'm going in the direction of PEG and Packrat."))

(defmethod compute-first-set ((g or-construct))
  (let ((first-set nil))
    (loop for child in (child-constructs g)
          do (setf first-set (union (compute-first-set child) first-set)))
    first-set))

(defmethod compute-first-set ((g sequence-construct))
  (let ((first-set nil))
    (loop for child in (child-constructs g)
          do (progn (setf first-set (union (compute-first-set child) first-set))
                    (unless (member :epsilon first-set)
                      (setf first-set (remove :epsilon first-set))
                      (return))))
    first-set))

(defmethod compute-first-set ((g (eql :epsilon)))
  (list :epsilon))

(defmethod compute-first-set ((g symbol))
  ;; terminal (token)
  (list g))
  
