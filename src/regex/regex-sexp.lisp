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
    (list (alex:destructuring-ecase (cons (ensure-tag-dsl-package (car regex)) (cdr regex))
            ((dsl:char-range start end)
             (make-instance 'elm:char-range-element :char-start start :char-end end))
            ((dsl:seq &rest elements)
             (make-instance 'elm:sequence-element :elements (map 'vector #'prepare-regex-tree elements)))
            ((dsl:or &rest elements)
             (make-instance 'elm:or-element :elements (map 'vector #'prepare-regex-tree elements)))
            ((dsl:* element)
             (make-instance 'elm:zero-or-more-element :element (prepare-regex-tree element)))
            ((dsl:+ element)
             (make-instance 'elm:one-or-more-element :element (prepare-regex-tree element)))
            ((dsl:? element)
             (make-instance 'elm:zero-or-one-element :element (prepare-regex-tree element)))
            ((dsl:not element)
             (make-instance 'elm:negated-element :element (prepare-regex-tree element)))
            ;; TODO: ENSURE TYPE OF ALL INNER REGEX ELEMENTS TO BE CHAR/CHAR-RANGE!!!
            ;; currently handled in the NFA code
            ((dsl:inv &rest elements)
             (make-instance 'elm:inv-element :elements (map 'vector #'prepare-regex-tree elements)))
            ((dsl:rep element min-count &optional max-count)
             (make-instance 'elm:repeated-element :element (prepare-regex-tree element)
                                                  :min-count min-count
                                                  :max-count max-count))
            ((dsl:tok element token)
             (make-instance 'elm:token-holder-element :element (prepare-regex-tree element)
                                                      :token token))))
    (string (make-instance 'elm:sequence-element
                           :elements (map 'vector #'(lambda (ch)
                                                      (make-instance 'elm:single-char-element
                                                                     :single-char ch)) regex)))))

(defmethod regex:parse-regex-expression ((regex-sexp list))
  "Parse regex expression in the form of lisp sexp, and produce a tree of regex elements."
  (prepare-regex-tree regex-sexp))
