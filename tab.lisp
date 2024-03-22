(in-package :tb)

(defclass table ()
  ((data :initarg :data
         :documentation "table data list of column arrays"
         :accessor table-data
         :type list)
   (field-names :initarg :field-names
                :documentation "list of table field names"
                :accessor table-field-names
                :type list)
   (indices :initform nil
            :initarg :indices
            :documentation "alist of table indexes"
            :accessor table-indices
            :type list)))

(defgeneric table-length (table)
  (:documentation "Returns number of rows of table")
  (:method ((table table))
    (length (first (table-data table)))))

(defgeneric table-width (table)
  (:documentation "Returns number of fields/columns of table")
  (:method ((table table))
    (length (table-data table))))

(defgeneric make-table (data &key field-names &allow-other-keys)
  (:documentation
   "Generic table creation function.  Should have methods to e.g. create
from plists, alists, other tables, CSVs, SQL queries etc.")
  ;; basic method for tables:
  (:method ((data table)
            &key
              field-names
              empty-p
              (copy-indices-p t))
    (make-instance 'table
                   :data
                   (unless empty-p
                     (mapcar #'alexandria:copy-array
                             (table-data data)))
                   :field-names (if field-names
                                    field-names
                                    (copy-list (table-field-names data)))
                   :indices (when (and copy-indices-p
                                       (not empty-p))
                              (copy-tree (table-indices data))))))

;;; More on tables:
;;;
;;; A table's data is a list of column arrays.
;;;
;;; A table index is an alist mapping from column value to row number
;;; sorted by column value.  Not sure if I'm keeping this or not,
;;; might not bother implementing until later.
;;;
;;; need method to get columns by name, index etc.  can be generic,
;;; could get crazy with macros too to allow formulas of columns to be
;;; accessed and written without quotes.  just a thought, but pandas
;;; has string formulas so it might be interesting to see what Lisp
;;; can do with macros and S-expressions to allow computational
;;; columns.

;;; make-table methods:

;; possibly most used: parse different types of lists to create table.
(defmethod make-table ((data list)
                       &key
                         field-names)
  (declare (optimize (debug 3)))
  (labels (;; list-of-lists-p
           (lolp (x)
             (and (car x)
                  (listp (car x))))
           ;; list of plists
           (plistp (x)
             (and (lolp x)
                  (symbolp (caar x)))))
    (let (table-data table-length table-width)
      (cond
        ((plistp data)
         ;; get field names from plists:
         (let ((field-symbols
                 (mapcar #'first
                         (cl-ana.list-utils:group (first data) 2))))
           (setf field-names  (mapcar #'string field-symbols)
                 table-length (length data)
                 table-width  (length field-symbols)
                 table-data
                 (loop
                   for i below table-width
                   collecting (make-array table-length
                                          :adjustable t :fill-pointer t)))
           (loop
             for plist in data
             for i from 0
             do
                (let ((values
                        (mapcar (lambda (f)
                                  (getf plist f))
                                field-symbols)))
                  (map nil
                       (lambda (v a)
                         (setf (aref a i) v))
                       values table-data)))))
        ((lolp data)
         ;; can't get field names, leave for later
         (setf table-length (length data)
               table-width  (length (car data))
               table-data
               (loop
                 for i below table-width
                 collecting (make-array table-length
                                        :adjustable t :fill-pointer t)))
         (loop
           for list in data
           for i from 0
           do
              (map nil
                   (lambda (v a)
                     (setf (aref a i)
                           v))
                   list table-data)))
        ;; default case: empty table, no columns, no names
        (t nil))
      (make-instance 'table
                     :data table-data
                     :field-names (if field-names
                                      field-names
                                      (loop
                                        for x from 1 to table-width
                                        collecting
                                        (format nil "x~a" x)))))))

;;; Table row access:
(defgeneric table-ref (table index &key type &allow-other-keys)
  (:documentation "Return a row from the table as a sequence of data.
type controls type of sequence.")
  (:method ((table table) (index integer)
            &key (type 'list))
    (map type
         (lambda (column)
           (aref column index))
         (table-data table))))

;;; Table mapping:
(defun table-map (row-fn table
                  &key
                    (type 'table)
                    field-names)
  "Maps a function row-fn across table, supplying each field as a
distinct argument to row-fn, and returning a new table with fields
specified by field-names.  row-fn should return a list of new field
values.

type can be one of 'table, 'array, or 'list to yield difference result
types.  For array, first index yields row, second yields field/column.
For list, result is a list of field-lists."
  (let ((length (table-length table)))
    (case type
      (table
       (make-table
        ;; code reuse
        (table-map row-fn table
                   :type 'list)
        :field-names field-names))
      (array
       (let* ((width (table-width table))
              (result (make-array (list length width))))
         (loop
           for i below length
           do
              (let ((row (apply row-fn
                                (table-ref table i :type 'list))))
                (loop
                  for j below width
                  for x in row
                  do (setf (aref result i j)
                           x))))
         result))
      (list
       (loop
         for i below length
         collecting
         (apply row-fn
                (table-ref table i :type 'list)))))))

;;; Column addition:
(defgeneric table-add-field! (table name
                              &optional value-or-function
                              &key
                                end-p
                              &allow-other-keys)
  (:documentation "Adds a new column to table with name and either a fixed value or a
function which will be supplied the table's other fields as distinct
arguments and which should return a value to insert as the new field
value for each row.  If value-or-function is NIL, NIL will be inserted
as new value of field.  end-p controls whether to add the column to
the front of the columns list or the end.")
  (:method ((table table) name &optional value-or-function
            &key (end-p t)
            &allow-other-keys)
    (let* ((fn (typecase value-or-function
                 (null (constantly nil))
                 (function value-or-function)
                 (t (constantly value-or-function))))
           (column
             (apply #'map 'vector fn
                    (table-data table))))
      (with-accessors ((field-names table-field-names)
                       (data        table-data))
          table
        (if end-p
            (setf data (nconc data (list column))
                  field-names (nconc field-names (list name)))
            (setf data (cons column data)
                  field-names (cons name field-names)))))
    table))

(defgeneric table-del-field! (table name-or-index)
  (:documentation "Remove column from table specified by name or index.")
  (:method ((table table) name-or-index)
    (with-accessors ((field-names table-field-names)
                     (data table-data))
        table
      (let* ((index
               (typecase name-or-index
                 (string (position name-or-index
                                   field-names
                                   :test #'string=))
                 (integer name-or-index))))
        (setf field-names (loop
                            for i from 0
                            for fn in field-names
                            when (not (= i index))
                              collecting fn)
              data (loop
                     for i from 0
                     for d in data
                     when (not (= i index))
                       collecting d))
        table))))

(defgeneric table-insert! (table &rest rows)
  (:documentation "Insert row into table.  Rows should be lists.")
  (:method ((table table) &rest rows)
    (with-accessors ((data table-data)) table
      (flet ((insert (row)
               (map nil (lambda (col new-field)
                          (vector-push-extend new-field col))
                    data
                    row)))
        (map nil #'insert rows)
        table))))

(defgeneric table-delete! (table condition &key &allow-other-keys)
  (:documentation "Deletes rows indicated by condition.  condition can be one of:

* integer index: delete specific row indicated by index.

* list of integer indices: delete rows indicated by indices.

* function: A boolean-valued function that accepts the row values as
  distinct arguments and indicates when a row should be deleted by
  returning T or NIL if not to be deleted.")
  ;; Backbone method:
  ;;
  ;; :sorted-p t -> no need to sort indices
  (:method ((table table) (condition list) &key sorted-p &allow-other-keys)
    (declare (optimize (debug 3)))
    (with-accessors ((data table-data))
        table
      (let ((table-length (table-length table))
            (sorted (if sorted-p
                        (copy-list condition)
                        (sort (copy-list condition) #'<)))
            (n 0))
        (labels ((shift-array (array start end delta)
                   (loop
                     for i from start below end
                     when (and (not (minusp (+ i delta)))
                               (not (>= (+ i delta) table-length)))
                       do (setf (aref array (+ i delta))
                                (aref array i)))
                   array)
                 (shift (start end delta)
                   (map nil
                        (lambda (column) (shift-array column start end delta))
                        data)))
          (do () ((null sorted))
            (let ((i (pop sorted)))
              (shift (1+ i)
                     (or (first sorted) table-length)
                     (- (incf n)))))
          (map nil
               (lambda (column)
                 (adjust-array column
                               (- table-length n)
                               :fill-pointer t))
               data)
          table))))
  ;; Single-index case:
  (:method ((table table) (condition integer) &key &allow-other-keys)
    (table-delete! table (list condition)))
  ;; Boolean function case:
  (:method ((table table) (condition function) &key &allow-other-keys)
    (let ((indices nil))
      (let ((i 0))
        (dotable (row table)
          (when (apply condition
                       row)
            (push i indices))
          (incf i)))
      (table-delete! table
                     (nreverse indices)
                     :sorted-p t))))

;;; Aggregations:
;;;
;;; This was a surprisingly difficult concept for me to find a
;;; suitable, beautiful, Lispy expression, but ultimately the answer
;;; was simple: Closures.
;;;
;;; - There is a main aggregation function: #'table-aggregate.  This
;;;   function processes a table to compute one aggregation via a
;;;   supplied function argument.  This might seem limited, however:
;;;   This one aggregation is interpreted as a row of data, so that if
;;;   the results of the aggregation are lists, then the elements of
;;;   the list are interpreted as columns.
;;;
;;; - The aggregation function argument is actually a function which
;;;   accepts a group argument and returns a function with a specific
;;;   calling protocol: When called with no arguments, return the
;;;   aggregation.  When called with arguments, those arguments are
;;;   the fields of a row of the table which must be aggregated.
;;;
;;; - Many simple aggregations have been defined for use,
;;;   e.g. #'agg-sum or #'agg-mean, and there is a general purpose
;;;   helper function #'agg which can create a new aggregator function
;;;   using any associative function that could be used for computing
;;;   a reduction.  (NOTE: #'agg is not suitable for closures as the
;;;   state is not correctly maintained per-group.)
;;;
;;; - The macro with-aggregation helps define a new aggregation using
;;;   a set of existing aggregate functions (or functions created in
;;;   the aggregate bindings, advanced usage) that can immediately be
;;;   used to return an aggregate table using #'table-aggregate.

;; Some basic aggregate functions:
(defun agg (fn &optional (default-value 0))
  "Takes any function accepting at least 2 optional arguments and converts it to
an aggregate function.  This works perfectly for functions that need
no state management, aka pure functions.  It may work in other cases
on a case-by-case basis.

Use this as a reference implementation for your own aggregation functions: They must accept a context, and then optional arguments for the accumulation and new datum."
  (lambda (group)
    (declare (ignore group))
    (let ((acc default-value))
      (lambda (&optional
            (datum nil datum-supplied-p))
        (if datum-supplied-p
            (setf acc (funcall fn acc datum))
            acc)))))

;; Count aggregation
(setf (symbol-function 'agg-count)
      ;; alert: this is sneaky
      (agg (lambda (v x)
             (declare (ignore x))
             (1+ v))
           0))
(setf (documentation #'agg-count 'function)
      "Counting aggregation for use with table-aggregate.  See all aggregates
via (apropos \"agg-\")")

;; Sum aggregation
(setf (symbol-function 'agg-sum)
      (agg #'+ 0))
(setf (documentation #'agg-sum 'function)
      "Sum aggregation for use with table-aggregate.  See all aggregates
via (apropos \"agg-\")")

;; (Natural) Log-sum aggregration (log of product)
(setf (symbol-function 'agg-log-sum)
      (agg (lambda (v x)
             (+ v (log x)))
           0))
(setf (documentation #'agg-log-sum 'function)
      "Sum-of-logs or log-of-product aggregation for use with
table-aggregate.  See all aggregates via (apropos \"agg-\")")

;; Mean aggregation
(defun agg-mean (&optional (type 'number))
  "Mean aggregation function.  See all aggregates via (apropos \"agg-\")"
  (lambda (group)
    (declare (ignore group))
    (let ((count (coerce 0 type))
          (sum (coerce 0 type)))
      (lambda (&optional datum)
        (if datum
            (setf count (1+ count)
                  sum (+ sum datum))
            (cl-ana.gmath:protected-/ sum count))))))

(defun table-aggregate (aggregator table
                        &key
                          (group-fn (constantly t))
                          (test 'equal)
                          field-names)
  "Return the aggregation produced by

1. Grouping records by the group function group-fn, with distinction
given by the test-fn.

2. Calling row-reduce-fn on each row from each group as if by #'reduce, associating the accumulated result with its group.

3. Returning a table with rows given by the groups and columns given by the accumulated result of row-reduce-fn.

aggregator should accept all table fields as distinct input
arguments and return a list of values to be considered the fields for
the resulting aggregate table."
  ;; aggregation:
  (let* ((agg-map (make-hash-table :test test)))
    (dotable (row table)
      (let* ((group (apply group-fn row)))
        (let (aggfn)
          ;; ensure agg is in agg-map and get it into aggfn
          (unless (setf aggfn (gethash group agg-map))
            (setf aggfn
                  (setf (gethash group agg-map)
                        (funcall aggregator group))))
          ;; Execute aggregation
          (apply aggfn row))))
    ;; Form new table by a call to each aggfn in agg-map with no
    ;; argument:
    (make-table
     (loop
       for key being the hash-keys in agg-map
       for aggfn being the hash-values in agg-map
       collecting (alexandria:ensure-list (funcall aggfn)))
     :field-names field-names)))

;;; Compound aggregation:
(defmacro with-aggregation (group agg-bindings lambda-list agg-result
                            &body agg-body)
  "Macro that generates code to return a function that will create an
aggregation function using aggregate functions that are provided in
agg-bindings.

Each agg-binding must be of the form (fsym agg-form) where agg-form
evaluates to an aggregate closure.  This function will be bound to
fsym as a callable function, e.g. (fsym ...) will work correclty as
will (funcall #'fsym ...).

This is useful for creating an aggregation to use with
table-aggregate.

The first forms in agg-body can be declarations for the generated
aggregation function, i.e. they can declare things about the arguments
in the lambda list but not about the group (might fix in future)."
  (alexandria:with-gensyms (args)
    (let ((aggs (loop
                  for b in agg-bindings collecting (gensym (string (first b)))))
          (declarations
            (remove-if-not (lambda (form) (and (listp form) (eq (first form) 'declare)))
                           agg-body))
          (agg-body
            (remove-if (lambda (form) (and (listp form) (eq (first form) 'declare)))
                       agg-body)))
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
             (lambda (&rest ,args)
               (destructuring-bind ,lambda-list ,args
                 ,@declarations
                 (if ,args
                     (progn ,@agg-body)
                     ,agg-result)))))))))
