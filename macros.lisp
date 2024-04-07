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

(defmacro tlet ((row table &key prefix (upcase-p t)) &body body)
  "Binds values from row to symbols with names taken from the table's
field names, optionally first converted to upper case as per upcase-p,
and executes body in that context.  Symbols are first prefixed by
prefix.  For SQL-like aesthetic, end the prefix with '.'

The prefix will be passed to #'string, so symbols are also safe to use
as a prefix.

Prefix defaults to the symbol value of the table plus a '.' if the
argument is a symbol, or \"\" if not.

Uses setf on raw symbols, so later fields with the same symbol will
clobber earlier fields."
  (let ((prefix
          (if (null prefix)
              (if (symbolp table)
                  (let ((s (string table)))
                    (concatenate 'string
                                 s
                                 (if (char= (elt s (1- (length s))) #\.)
                                     ""
                                     ".")))
                  "")
              prefix)))
    (alexandria:with-gensyms (r tab sym x val pre)
      `(let ((,r ,row)
             (,tab ,table)
             (,pre (string ',prefix)))
         (loop
           for ,sym in (mapcar (lambda (,x)
                                 (intern
                                  (,(if upcase-p
                                        'string-upcase
                                        'identity)
                                   (concatenate 'string ,pre ,x))))
                               (field-names ,tab))
           for ,val in ,r
           do
              (setf (symbol-value ,sym) ,val))
         ,@body))))

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

fields and (field ...) are setf-able so that if desired, one can
simply modify the fields in a row and then return some function of
that modified row.  Useful for simply changing values, adding/removing
columns etc."
  (alexandria:with-gensyms (row)
    `(lambda (&rest ,row)
       (destructuring-bind (&key ,@fields &allow-other-keys) ,row
         (symbol-macrolet ((fields ,row))
           (macrolet ((field (spec)
                        (typecase spec
                          (symbol `(getf fields ,(intern (string spec)
                                                         :keyword)))
                          (keyword `(getf fields ,spec))
                          (string `(getf fields
                                         ,(intern spec :keyword))))))
             ,@body))))))
