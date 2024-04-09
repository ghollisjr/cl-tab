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

Also provided in the body of the tlambda are macros and symbol macros
as follows:

* macro (field <string-or-keyword>): Returns field value from
  row.

* symbol-macro fields: Returns all fields in row as a plist.

Field arguments listed in the lambda-list, fields, and (field ...) are
setf-able so that if desired, one can simply modify the fields in a
row and then return some function of that modified row.  Useful for
simply changing values, adding/removing columns etc."
  (alexandria:with-gensyms (row)
    `(lambda (&rest ,row)
       (symbol-macrolet ((fields ,row))
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
             ,@body))))))
