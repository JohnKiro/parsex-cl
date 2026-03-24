(in-package :parsex-cl/rdp/grammar/sexp)

(defconstant +dsl-package+ :parsex-cl/rdp/grammar/sexp/dsl)
(defparameter +construct-tags+ '(dsl:or dsl:seq dsl:? dsl:* dsl:+))

(defun find-tag-dsl-package (dsl-tag)
  "Check whether tag specified with `dsl-tag` is valid, i.e. found in grammar DSL package. The argument
`dsl-tag` is expected to be a symbol in any package. If found, it is returned, otherwise, returns NIL.
Note that we also ensure that the found tag is external, i.e. really belongs to the DSL vocabulary, not
just a symbol that was inadvertently interned in the package."
  (declare (type symbol dsl-tag))
  (multiple-value-bind (found-sym type) (find-symbol (symbol-name dsl-tag) +dsl-package+)
    (when (eq type :external)
      found-sym)))

(defun create-grammar-construct (construct-tag &optional construct-id)
  "Given construct tag (symbol, one of: OR, SEQ, ?, *, +), creates corresponding object. The symbol
does not need to be in the DSL package, since the function uses `find-tag-dsl-package`.
Note that it initializes only the optional construct ID slot, which is initialized to rule ID/token ID.
The construct ID is optional because inner (unnamed) constructs have no ID.
Other slots will be initialized afterwards, using adequate setter."
  (make-instance (ecase (find-tag-dsl-package construct-tag)
                   (dsl:or 'constr::or-construct)
                   (dsl:seq 'constr::sequence-construct)
                   (dsl:? 'constr::zero-or-one-construct)
                   (dsl:* 'constr::zero-or-more-construct)
                   (dsl:+ 'constr::one-or-more-construct))
                 :construct-id construct-id))

(defun normalize-grammar (grammar)
  "Normalize sexp grammar forms, by ensuring grammar tags (rule, token, seq, etc.) are in the DSL
package. This is important for equality tests. Note that we don't touch regex DSL, that's why
token forms pass through unchanged (except for the TOKEN tag, which belongs to grammar DSL)."
  (labels ((recur (form)
             (etypecase form
               (symbol form)
               (cons (cons (let ((normalized-tag (find-tag-dsl-package (car form))))
                             ;; not enough to find tag in DSL, because RULE and TOKEN tags are not
                             ;; allowed within construct body, and also need to exclude any invalid
                             ;; symbol that sneaked into the DSL package
                             (unless (member normalized-tag +construct-tags+)
                               (error "Invalid grammar rule ~a! Must start with one of ~a."
                                      form +construct-tags+))
                             normalized-tag)
                           (loop for construct in (cdr form)
                                 collect (recur construct)))))))
    (mapcar (lambda (g)
              (destructuring-bind (g-tag g-id g-contents) g
                (let ((g-tag-normalized (find-tag-dsl-package g-tag)))
                  (ecase g-tag-normalized
                    (dsl:token (list g-tag-normalized g-id g-contents))
                    (dsl:rule (list g-tag-normalized g-id (recur g-contents)))))))
            grammar)))

;;; Parsing EBNF grammar (sexp forms) into a tree of grammar-construct objects
;; TODO: better grammar syntax verification and error reporting
;; TODO: detect (and reject or fix) grammar having left-recursive rules
;; TODO: may be generic?
(defun parse-grammar (grammar &optional (start-rule :root))
  "Parses grammar in sexp form `grammar` (for now, it is something similar to EBNF/PEG, to be refined
later), and produces a corresponding graph of construct objects. `start-rule` identifies the parsing root
element. Returns three values: the construct object for the `start-rule`, the tokenizer core, and for
convenience, a hash table mapping each rule ID to corresponding construct object. Note that we choose
:root as default start rule, i.e. a keyword, which is a sensible default package."
  (declare (optimize (debug 3) (speed 0)))
  (let ((grammar (normalize-grammar grammar))
        (grammar-table (make-hash-table)))
    (labels ((store (row-id row-value)
               "Low-level grammar table update function. It also checks for duplicate entries."
               #+nil(break "Adding entry to grammar table: key = ~a, value = ~a..~%" row-id row-value)
               (if #1=(gethash row-id grammar-table)
                   (error "Duplicate row (token/rule) ~a detected!" row-id)
                   (setf #1# row-value)))
             (add-grammar-element-to-table (grammar-form)
               "Add entry for grammar item in question, whether token or grammar rule."
               (alexandria:destructuring-ecase grammar-form
                 ;; it's a token, => insert it in hash table as it is (key = value = token id)
                 ;; TODO: later, may be better to use a wrapper class (i.e. a token class)
                 ((dsl:token token-id _)
                  (declare (ignorable _))
                  (store token-id (make-instance 'constr::token-construct :token token-id
                                                                          :construct-id token-id)))
                 ((dsl:rule rule-id (rule-key . _))
                  (declare (ignorable _))
                  (store rule-id (create-grammar-construct rule-key)))))
             (initialize-grammar-table ()
               "Initialize grammar table with entry per rule/token, mapping id -> construct object."
               #+nil(declare (optimize (debug 3) (speed 0)))
               (dolist (grammar-form grammar)
                 (add-grammar-element-to-table grammar-form)))
             (retrieve-construct (row-id)
               "Maps row id (rule id/token id) to corresponding grammar construct object."
               (let ((val (gethash row-id grammar-table)))
                 (unless val
                   (error "No rule found for grammar element ~a!" row-id))
                 val))
             (process-rule-form (element-obj element-form)
               "Fills construct object (`element-obj`) with child/children objects, which it also
 processes recursively. Child/children are extracted from `element-form`."
               (etypecase element-form
                 (symbol element-obj)
                 (cons (alexandria:destructuring-ecase element-form
                         (((dsl:? dsl:* dsl:+) child-form)
                          (etypecase child-form
                            (symbol
                             (constr::set-child element-obj (retrieve-construct child-form)))
                            (cons
                             (constr::set-child element-obj
                                                (process-rule-form
                                                 (create-grammar-construct (car child-form))
                                                 child-form)))))
                         (((dsl:seq dsl:or) &rest children-forms)
                          (dolist (child-form children-forms)
                            (etypecase child-form
                              (symbol
                               (constr::add-child element-obj (retrieve-construct child-form)))
                              (cons
                               (constr::add-child element-obj
                                                  (process-rule-form
                                                   (create-grammar-construct(car child-form))
                                                   child-form)))))))
                  element-obj))))
      ;; note that we initialize the table beforehand since rules may refer to other rules that appear
      ;; later in the grammar.
      (initialize-grammar-table)
      (multiple-value-bind (tokenizer-core-add-token-fn tokenizer-core-build-fn)
          (tok-core:make-tokenizer-core-builder)
        (loop for (g-tag g-id g-contents) in grammar
              do (ecase g-tag
                   (dsl:token (funcall tokenizer-core-add-token-fn g-id
                                       (regex-sexp:prepare-regex-tree g-contents)))
                   (dsl:rule  (process-rule-form (retrieve-construct g-id) g-contents))))
        (let ((tokenizer-core (funcall tokenizer-core-build-fn)))
          (values (retrieve-construct start-rule) tokenizer-core grammar-table))))))
