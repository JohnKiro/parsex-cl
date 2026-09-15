(in-package :parsex-cl/rdp/grammar/sexp)

(defparameter +dsl-package+ (find-package :dsl))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +construct-tags+ '(dsl:or dsl:seq dsl:? dsl:* dsl:+))
  (defun construct-tag-p (sym)
    (member sym +construct-tags+)))

(deftype construct-tag () '(satisfies construct-tag-p))

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
Note that it initializes only the optional construct ID slot, which is initialized to rule ID.
The construct ID is optional because inner (unnamed) constructs have no ID.
Other slots will be initialized afterwards, using adequate setter."
  (make-instance (ecase (find-tag-dsl-package construct-tag)
                   (dsl:or 'constr:or-construct)
                   (dsl:seq 'constr:sequence-construct)
                   (dsl:? 'constr:zero-or-one-construct)
                   (dsl:* 'constr:zero-or-more-construct)
                   (dsl:+ 'constr:one-or-more-construct))
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
(defun parse-grammar (grammar &key (start-rule :root) (normalized-p nil))
  "Parses grammar in sexp form `grammar` (for now, it is something similar to EBNF/PEG, to be refined
later), and produces a corresponding graph of construct objects. The grammar is optionally normalized, to
ensure the grammar vocab is in the DSL package. If the `normalize-p` flag is set, the function assumes
the input grammar to be already normalized.
`start-rule` identifies the parsing root element.
Returns three values: the construct object for the `start-rule`, the tokenizer core, and for convenience,
a hash table mapping each rule ID to corresponding construct object.
Note that we choose :root as default start rule, i.e. a keyword, which is a sensible default package."
  (unless normalized-p
    (setf grammar (normalize-grammar grammar)))
  (let ((grammar-table (make-hash-table)))
    (labels ((store (row-id row-value)
               "Low-level grammar table update function. It also checks for duplicate entries."
               (if #1=(gethash row-id grammar-table)
                   (error "Duplicate row (token/rule) ~a detected!" row-id)
                   (setf #1# row-value)))
             (add-grammar-element-to-table (grammar-form)
               "Add entry for grammar item in question, whether token or grammar rule."
               (alexandria:destructuring-ecase grammar-form
                 ((dsl:token token-id _)
                  (declare (ignorable _))
                  (let ((tok-constr (make-instance 'constr:token-construct :token token-id)))
                    (store token-id tok-constr)))
                 ((dsl:rule rule-id rule-details)
                  (let ((rule-obj
                          (etypecase rule-details
                            (symbol (retrieve-construct rule-details))
                            (cons (create-grammar-construct (car rule-details) rule-id)))))
                    (store rule-id rule-obj)))))
             (initialize-grammar-table ()
               "Initialize grammar table with entry per rule/token, mapping id -> construct object."
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
                             (constr:set-child element-obj (retrieve-construct child-form)))
                            (cons
                             (constr:set-child element-obj
                                               (process-rule-form
                                                (create-grammar-construct (car child-form))
                                                child-form)))))
                         (((dsl:seq dsl:or) &rest children-forms)
                          (dolist (child-form children-forms)
                            (etypecase child-form
                              (symbol
                               (constr:add-child element-obj (retrieve-construct child-form)))
                              (cons
                               (constr:add-child element-obj
                                                 (process-rule-form
                                                  (create-grammar-construct(car child-form))
                                                  child-form)))))))
                  (constr::initialize-first-set element-obj)
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

(defmacro token (token-id regex-form)
  "User interface macro to define a grammar token element."
  `(dsl:token ,token-id ,regex-form))

(defmacro rule (rule-id rule-form)
  "User interface macro to define a grammar rule element."
  `(dsl:rule ,rule-id ,rule-form))

(defmacro grammar (&body grammar-forms)
  "User interface macro to define grammar, in normalized form (vocab in DSL package)."
  `(normalize-grammar ',grammar-forms))

(defmacro define-grammar ((&key (start-rule :root)) &body grammar-forms)
  "User interface macro to parse and generate grammar. Returns three values returned by `parse-grammar`:
the construct object for the `start-rule`, the tokenizer core, and rule mapping hash table."
  `(parse-grammar ',grammar-forms :start-rule ',start-rule :normalized-p nil))
