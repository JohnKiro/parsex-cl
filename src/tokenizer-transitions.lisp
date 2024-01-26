(in-package :parsex-cl.tokenizer-transitions)

(defclass transition ()
  ((atom-handling :initarg :atom-handling
                  :initform (error "Mandatory: atom-handling!")
                  :reader atom-handling)))

(defclass transition-to-other (transition)
  ((next-state :initarg :next-state
               :initform (error "Mandatory: next-state!")
               :type (or normal-state terminal-state))))

(defclass transition-to-self (transition)
  ())

;;; TODO: alternatively (probably for better performance, may change to defun.
;;; In this case, also combine transition hierarchy into a single class.
(defgeneric get-next-state (transition current-state))

(defmethod get-next-state ((transition transition-to-other) current-state)
  (slot-value transition 'next-state))

(defmethod get-next-state ((transition transition-to-self) current-state)
  current-state)
