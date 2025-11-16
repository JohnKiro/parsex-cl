(in-package :parsex-cl/manual-tokenizer/commons/common-atom-matchers)
;;;; Utility functions that create commonly used atom matcher.

;;;; Atom matcher: atom -> transition.
;;;; TODO: Validate function arguments (not null, valid type)

;;; The following functions are char-specific (since they use char-specific comparison functions)
;;; TODO: May use https://alex-gutev.github.io/generic-cl/ to provide generic comparison functions.
(defun create-matcher-for-single-char (char transition)
  (lambda (input-char)
    (if (char= input-char char) transition nil)))

(defun create-matcher-for-char-range (char-start char-end transition)
  (lambda (input-char)
    (if (char<= char-start input-char char-end) transition nil)))

(defun create-matcher-for-char-set (char-set transition)
  (lambda (input-char)
    (loop for ch across char-set
       do (if (char= ch input-char) (return transition) nil))))
