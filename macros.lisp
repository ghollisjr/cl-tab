(in-package :tb)

(defmacro dotable ((vars table &optional result) &body body)
  (alexandria:with-gensyms
      (tab table-width table-length
          row i data)
    `(let ((,tab ,table))
       (with-accessors ((,table-width table-width)
                        (,table-length table-length)
                        (,data data))
           ,tab
         (do* ((,i 0 (1+ ,i)))
              ((>= ,i ,table-length) ,result)
           (let ((,row (map 'list (lambda (d)
                                    (aref d ,i))
                            ,data)))
             (symbol-macrolet
                 ,(if (listp vars)
                      ;; Two usages:
                      ;; 1. vars is list of symbols:
                      (loop
                        for v in vars
                        for j from 0
                        collecting `(,v (aref ,row ,j)))
                      ;; 2. vars is a symbol, so row vector should be
                      ;; bound to vars
                      `((,vars ,row)))
               ,@body)))))))

;;; Compound aggregation:
(defmacro with-aggregation ((&optional group group-fn) agg-bindings
                            agg-result
                            table-field-lambda-list
                            &body agg-body)
  "Macro that generates code to return a function that will create an
aggregation function using aggregate functions that are provided in
agg-bindings.

Each agg-binding must be of the form (fsym agg-form) where agg-form
evaluates to an aggregate closure.  This function will be bound to
fsym as a callable function, e.g. (fsym ...) will work correclty as
will (funcall #'fsym ...).  Simultaneously, for convenience, a
symbol-macrolet for that same symbol (e.g. fsym) will be bound to a
call to the closure with no arguments (e.g. (fsym)).  This allows the
aggregation symbol to be used to access the value of the
aggregation, e.g. in the agg-result.

agg-result is a list of aggregate values to return per-group, and will
be treated as rows in the resulting aggregate table.

table-field-lambda-list will be treated as if supplied to tlambda, so
it should be a list of field arguments to be received by the function
being mapped across the table row plists.

This is useful for creating an aggregation to use with
aggregate.

If group is NIL, then the group argument will not be accessible nor
used in the aggregation (assumes single-group and therefore reasonable
group-fn, e.g. (constantly t)).

The first forms in agg-body can be declarations for the generated
aggregation function, i.e. they can declare things about the arguments
in the lambda list but not about the group (might fix in future)."
  (alexandria:with-gensyms (args)
    (let* ((group (or group (gensym "group")))
           (aggs (loop
                   for b in agg-bindings collecting (gensym (string (first b)))))
           (declarations
             (remove-if-not (lambda (form) (and (listp form) (eq (first form) 'declare)))
                            agg-body))
           (agg-body
             (remove-if (lambda (form) (and (listp form) (eq (first form) 'declare)))
                        agg-body))
           (agg-fn-form
             `(lambda (,group)
                (let (,@(loop
                          for a in aggs
                          for b in agg-bindings
                          collecting `(,a ,(second b))))
                  (labels ,(loop
                             for a in aggs
                             for b in agg-bindings
                             collecting
                             (destructuring-bind (fsym form) b
                               (declare (ignore form))
                               `(,fsym (&rest ,args)
                                       (apply ,a ,args))))
                    (symbol-macrolet ,(loop
                                        for b in agg-bindings
                                        for fsym = (first b)
                                        collecting
                                        `(,fsym (,fsym)))
                      (tlambda (,@table-field-lambda-list)
                        ,@declarations
                        (if fields
                            (progn ,@agg-body)
                            ,agg-result))))))))
      (if group-fn
          `(list ,agg-fn-form ,group-fn)
          agg-fn-form))))
(setf (macro-function 'with-agg) (macro-function 'with-aggregation))

(defmacro tlambda ((&rest fields) &body body)
  "Generates a new lambda function which effectively transforms fields
into a lambda list of the form `(&key ,@fields &allow-other-keys) so
that any plist input can be parsed, and the fields that are desired by
the user are automatically assigned to a specific symbol.

fields is a variable bound to all fields in row as a plist.

Also provided in the body of the tlambda are macros and symbol macros
as follows:

* macro (field <string-or-keyword>): Returns field value from
  row.

* symbol macros for any fields requested specifically in the
  lambda-list.

Field arguments listed in the lambda-list, fields, and (field ...) are
all setf-able so that if desired, one can simply modify the fields in
a row and then return some function of that modified row.  Useful for
simply changing values, adding/removing columns etc."
  `(lambda (&rest fields)
     (macrolet ((field (spec)
                  (typecase spec
                    (symbol `(getf fields ,(intern (string spec)
                                                   :keyword)))
                    (keyword `(getf fields ,spec))
                    (string `(getf fields
                                   ,(intern spec :keyword))))))
       (symbol-macrolet (,@(loop
                             for field in fields
                             collect `(,field (field ,field))))
         ,@body))))

(defmacro tlambda* ((&rest fields) &body body)
  "Variant of tlambda that automatically binds unbound variables to a
field with the keyword version of that symbol if available, or NIL if
not.

NOTE: If a dynamic/global variable has already been defined, then this
function will use that value even if a field named like that symbol
exists in the table."
  (alexandria:with-gensyms (c)
    `(tlambda (,@fields)
       (handler-bind ((unbound-variable
                        (lambda (,c)
                          (use-value
                           (getf fields
                                 (intern (string
                                          (cell-error-name ,c))
                                         :keyword))))))
         ,@body))))

(defmacro on-keys (table keys1
                   &rest args)
  "Convenience macro for hash equijoin on key(s) for tables.

keys1 & keys2 can be lists of fields on which to equijoin, or just a
single field symbol denoting single-key equijoin.

If keys2 is not specified, then keys1 will be used from both tables.

Also supports key arguments:
* :type
* :test
type & test act just like they do for #'on.

Examples:

(on-keys tab x) ; join to tab on keys x from both tables
(on-keys tab x y) ; join to tab on keys x from left and y from right
(on-keys tab (x y) :type :full) ; full-join on x & y from both tables
(on-keys tab (x y) (y z) :type :left :test 'equalp) ; left join with test"
  (let ((keys2 nil))
    ;; parse args
    (unless (keywordp (car args))
      (setf keys2 (pop args)))
    (unless keys2 (setf keys2 keys1))
    (destructuring-bind (&key test type) args
      (unless type (setf type :inner))
      (unless test (setf test ''equal))
      (let ((keys1 (if (symbolp keys1) (list keys1) keys1))
            (keys2 (if (symbolp keys2) (list keys2) keys2)))
        `(on ,table
             (list (tlambda ,keys1 (list ,@keys1))
                   (tlambda ,keys2 (list ,@keys2)))
             :type ,type
             :test ,test)))))
