(in-package :parsex-cl.regex-sexp)

;;; Handling of regex in the form of sexp (sexp --> regex object tree).

;;; TODO: check alternatives to define var / func for use in the macro, and ensure no error during
;;; expansion for "unknown var / func"!!!
(defconstant +regex-element-tags-package+ (let ((pkg-name :regex-sexp.element.tags))
                                            (or (find-package pkg-name)
                                                (make-package pkg-name)))
  "Package dedicated for regex tag names (SEQ, OR, +, etc.), for local use only.")

(defmacro case-list-regex (element-tag &body body)
  "Expands into an ECASE clause, with case variable evaluating into the value of ELEMENT-TAG, but
after being reinterned into the package specified in +REGEX-ELEMENT-TAGS-PACKAGE+. The BODY should
be in the form of an ECASE body. The idea is to be able to do equality tests on symbols that are
possibly coming from different packages, by reintering the two operands of the EQ function into
the package specified in +REGEX-ELEMENT-TAGS-PACKAGE+. This allows to specify regex tags in any
package."
  (labels ((prepare-body (body)
             (loop for (each-tag . each-clause) in body
                   collect (cons (sym:reintern each-tag +regex-element-tags-package+)
                                 each-clause))))
    (let ((reinterned-element-tag (gensym)))
      `(let ((,reinterned-element-tag (sym:reintern ,element-tag +regex-element-tags-package+)))
         (ecase ,reinterned-element-tag
           ,@(prepare-body body))))))

;;;; NOTE: since the reader will accept only a valid sexp, the "no more input" case is not possible.
;;;; TODO: ensure correct number of elements for each type (e.g. * accepts 1 & only 1 element)
;;;; TODO: may split function using other helper functions
;;;; TODO: flatten (simplify) specific cases such as seq within seq
(defun prepare-regex-tree (regex)
  "Takes an input list as regex element, and recursively constructs an equivalent regex element
 object."
  (etypecase regex
    (character (make-instance 'elm:single-char-element :single-char regex))
    (keyword regex) ;"any char" (TODO: pass unchanged, or generate any-char-element?)
    (list (case-list-regex (car regex)
            ;; note that I could use any symbols (no need for keywords), but keywords are better to
            ;; avoid confusing the syntax coloring
            (:char-range (make-instance 'elm:char-range-element
                                        :char-start (second regex)
                                        :char-end (third regex)))
            (:seq (make-instance 'elm:sequence-element
                                 :elements (map 'vector #'prepare-regex-tree (cdr regex))))
            (:or (make-instance 'elm:or-element
                                :elements (map 'vector #'prepare-regex-tree (cdr regex))))
            (:* (make-instance 'elm:zero-or-more-element :element (prepare-regex-tree
                                                                   (second regex))))
            (:+ (make-instance 'elm:one-or-more-element :element (prepare-regex-tree
                                                                  (second regex))))
            (:? (make-instance 'elm:zero-or-one-element :element (prepare-regex-tree
                                                                  (second regex))))
            (:not (make-instance 'elm:negated-element :element (prepare-regex-tree
                                                                (second regex))))
            ;; TODO: ENSURE TYPE OF ALL INNER REGEX ELEMENTS TO BE CHAR/CHAR-RANGE!!!
            ;; currently handled in the NFA code
            (:inv (make-instance 'elm:inv-element
                                 :elements (map 'vector #'prepare-regex-tree (cdr regex))))))
    (string (make-instance 'elm:sequence-element
                           :elements (map 'vector #'(lambda (ch)
                                                      (make-instance 'elm:single-char-element
                                                                     :single-char ch)) regex)))))
