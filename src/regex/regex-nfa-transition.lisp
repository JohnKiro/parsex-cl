(in-package :parsex-cl/regex/nfa/transition)

;;; defines a normal NFA transition upon matching of ELEMENT, to NEXT-STATE.
;;; TODO: for now, element type restricted to one of three types with character hardcoded.

(defclass nfa-transition ()
  ((%element :initarg :element
             :initform (error "element must be specified!")
             :type (or null elm:simple-element)
             :reader element)
   (%next-state :initarg :next-state
                :initform (error "next-state must be specified!")
                ;; TODO: It's not possible for a transition to lead to a null state, adjust type
                ;; accordingly (update: I'm allowing nullability, since in negation, we clear
                ;; (invalidate) transitions.
                :type (or null nfa-state)
                :reader next-state)))

(defmethod initialize-instance :after ((transition nfa-transition) &key)
  "Transition constructor for element type validation."
  (with-slots (%element) transition
    (let ((required-element-type 'elm:simple-element))
      (unless (typep %element required-element-type)
        (error "Invalid transition element type for transition ~a. Expecting ~a, got ~a!"
               transition required-element-type %element)))))

(defun clear-transition (transition)
  "Clear (invalidate) transition. This is a simpler way to delete the transition, than removing it
from the transitions table of a certain state. Most probably won't be needed."
  (with-slots (%element %next-state) transition
    (setf %element nil
          %next-state nil)))
