(in-package :parsex-cl/regex/sexp)

;;; Handling of regex in the form of sexp (sexp --> regex object tree).

(defun ensure-tag-dsl-package (element-tag)
  "Ensure/re-intern element tag (e.g. SEQ, CHAR-RANGE etc.) in regex DSL package."
  (alex:ensure-symbol element-tag (find-package :parsex-cl/regex/sexp/dsl)))

;;;; NOTE: since the reader will accept only a valid sexp, the "no more input" case is not possible.
;;;; TODO: ensure correct number of elements for each type (e.g. * accepts 1 & only 1 element)
;;;; TODO: may split function using other helper functions
;;;; TODO: flatten (simplify) specific cases such as seq within seq
(defun prepare-regex-tree (regex)
  "Takes an input list as regex element, and recursively constructs an equivalent regex element
 object."
  (etypecase regex
    (character (make-instance 'elm:single-char-element :single-char regex))
    (keyword regex) ;supports :any-char, translated into a char-range-element in regex-to-nfa
    (list (ecase (ensure-tag-dsl-package (car regex))
            (dsl:char-range (make-instance 'elm:char-range-element
                                           :char-start (second regex)
                                           :char-end (third regex)))
            (dsl:seq (make-instance 'elm:sequence-element
                                    :elements (map 'vector #'prepare-regex-tree (cdr regex))))
            (dsl:or (make-instance 'elm:or-element
                                   :elements (map 'vector #'prepare-regex-tree (cdr regex))))
            (dsl:* (make-instance 'elm:zero-or-more-element :element (prepare-regex-tree
                                                                      (second regex))))
            (dsl:+ (make-instance 'elm:one-or-more-element :element (prepare-regex-tree
                                                                     (second regex))))
            (dsl:? (make-instance 'elm:zero-or-one-element :element (prepare-regex-tree
                                                                     (second regex))))
            (dsl:not (make-instance 'elm:negated-element :element (prepare-regex-tree
                                                                   (second regex))))
            ;; TODO: ENSURE TYPE OF ALL INNER REGEX ELEMENTS TO BE CHAR/CHAR-RANGE!!!
            ;; currently handled in the NFA code
            (dsl:inv (make-instance 'elm:inv-element
                                    :elements (map 'vector #'prepare-regex-tree (cdr regex))))
            (dsl:rep (destructuring-bind (element-sexp min-count &optional max-count) (cdr regex)
                       (make-instance 'elm:repeated-element
                                      :element (prepare-regex-tree element-sexp)
                                      :min-count min-count
                                      :max-count max-count)))
            (dsl:tok (destructuring-bind (element-sexp token) (cdr regex)
                       (make-instance 'elm:token-holder-element
                                      :element (prepare-regex-tree element-sexp)
                                      :token token)))))
    (string (make-instance 'elm:sequence-element
                           :elements (map 'vector #'(lambda (ch)
                                                      (make-instance 'elm:single-char-element
                                                                     :single-char ch)) regex)))))

(defmethod regex:parse-regex-expression ((regex-sexp list))
  "Parse regex expression in the form of lisp sexp, and produce a tree of regex elements."
  (prepare-regex-tree regex-sexp))
