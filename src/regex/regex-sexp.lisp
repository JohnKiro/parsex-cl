(in-package :parsex-cl/regex/sexp)

;;; Handling of regex in the form of sexp (sexp --> regex object tree).
(defconstant +dsl-package+ :parsex-cl/regex/sexp/dsl)

(defun find-tag-in-dsl-package (element-tag)
  "Check whether tag specified with `element-tag` is valid, i.e. found in regex DSL package. The argument
`element-tag` is expected to be a symbol in any package. If found, it is returned, otherwise, returns
NIL.
Note that we also ensure that the found tag is external, i.e. really belongs to the DSL vocabulary, not
just a symbol that was inadvertently interned in the package."
  (declare (type symbol element-tag))
  (multiple-value-bind (found-sym type) (find-symbol (symbol-name element-tag) +dsl-package+)
    (when (eq type :external)
      found-sym)))

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
    (list (alex:destructuring-ecase (cons (find-tag-in-dsl-package (car regex)) (cdr regex))
            ((dsl:char-range start end)
             (make-instance 'elm:char-range-element :char-start start :char-end end))
            ((dsl:seq &rest children)
             (make-instance 'elm:sequence-element :elements (map 'vector #'prepare-regex-tree children)))
            ((dsl:or &rest children)
             (make-instance 'elm:or-element :elements (map 'vector #'prepare-regex-tree children)))
            ((dsl:* child)
             (make-instance 'elm:zero-or-more-element :element (prepare-regex-tree child)))
            ((dsl:+ child)
             (make-instance 'elm:one-or-more-element :element (prepare-regex-tree child)))
            ((dsl:? child)
             (make-instance 'elm:zero-or-one-element :element (prepare-regex-tree child)))
            ((dsl:not child)
             (make-instance 'elm:negated-element :element (prepare-regex-tree child)))
            ;; TODO: ENSURE TYPE OF ALL INNER REGEX ELEMENTS TO BE CHAR/CHAR-RANGE!!!
            ;; currently handled in the NFA code
            ((dsl:inv &rest children)
             (make-instance 'elm:inv-element :elements (map 'vector #'prepare-regex-tree children)))
            ((dsl:rep child min-count &optional max-count)
             (make-instance 'elm:repeated-element :element (prepare-regex-tree child)
                                                  :min-count min-count
                                                  :max-count max-count))
            ((dsl:tok child token)
             (prepare-token-element child token))))
    (string (make-instance 'elm:sequence-element
                           :elements (map 'vector #'(lambda (ch)
                                                      (make-instance 'elm:single-char-element
                                                                     :single-char ch))
                                          regex)))))

(defun prepare-token-element (regex token-id)
  "Handy function to create a token holder element, given token id and corresponding regex. "
  (make-instance 'elm:token-holder-element :element (prepare-regex-tree regex) :token token-id))

(defmethod regex:parse-regex-expression ((regex-sexp list))
  "Parse regex expression in the form of lisp sexp, and produce a tree of regex elements."
  (prepare-regex-tree regex-sexp))
