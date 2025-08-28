(in-package :parsex-cl/regex/nfa/transition)

;;; defines a normal NFA transition upon matching of ELEMENT, to NEXT-STATE.
;;; TODO: for now, element type restricted to one of three types with character hardcoded.

(defclass nfa-transition ()
  ((%element :initarg :element
             :initform (error "element must be specified!")
             :type (elm:simple-element)
             :reader element)
   (%next-state :initarg :next-state
                :initform (error "next-state must be specified!")
                ;; TODO: It's not possible for a transition to lead to a null state, adjust type
                ;; accordingly.
                :type (or null nfa-state)
                :reader next-state)))

(defmethod initialize-instance :after ((transition nfa-transition) &key)
  "Transition constructor for element type validation."
  (with-slots (%element) transition
    (let ((required-element-type 'elm:simple-element))
      (unless (typep %element required-element-type)
        (error "Invalid transition element type for transition ~a. Expecting ~a, got ~a!"
               transition required-element-type %element)))))
