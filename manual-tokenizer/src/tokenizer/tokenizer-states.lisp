(in-package :parsex-cl/manual-tokenizer/states)

;;; Scanner normal state.
;;; Slots:
;;; - transition finder function (atom -> transition)
;;; - token on no input: token to return when no atoms are left in input
(defclass normal-state ()
  ((transition-finder-func :initarg :transition-finder-func
                           :initform (error "Mandatory: transition-finder-func")
                           :reader transition-finder-func
                           :type function)
   (token-on-no-input :initarg :token-on-no-input
                      :initform (error "Mandatory: token-on-no-input")
                      :reader token-on-no-input)))

;;; Scanner terminal state.
;;; Slots:
;;; - token: Scanner's output token
(defclass terminal-state ()
  ((terminal-token :initarg :terminal-token
                   :initform (error "Mandatory: terminal-token")
                   :reader terminal-token)))
