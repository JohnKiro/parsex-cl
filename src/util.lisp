;; Not used. May convert it later to a more generic one, for different list element types, to make
;; it more useful.
(defun insert-char-in-order (char chars)
  "Destructively insert character CHAR in a sorted list CHARS, maintaining ascending order, and also
while ignoring any duplicates.
Note: since CHAR might be inserted ahead of CHARS, the returned pointer must be saved (as is usually
the case for list-destructive operations).
NOTE: I'm deprecating this function, but keeping it as a UTIL function since it's interesting
(may convert it later to a more generic one, for different list element types)"
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
