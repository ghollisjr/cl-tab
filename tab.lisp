(in-package :tb)

(defclass table ()
  ((data :initarg :data
         :documentation "table data list of column arrays"
         :accessor data
         :type list)
   (field-names :initarg :field-names
                :documentation "list of table field names"
                :accessor field-names
                :type list)
   (indices :initform nil
            :initarg :indices
            :documentation "alist of table indexes"
            :accessor table-indices
            :type list)))

(defgeneric table-length (table)
  (:documentation "Returns number of rows of table")
  (:method ((table table))
    (length (first (data table)))))

(defgeneric table-width (table)
  (:documentation "Returns number of fields/columns of table")
  (:method ((table table))
    (length (data table))))

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
                             (data data)))
                   :field-names (if field-names
                                    field-names
                                    (copy-list (field-names data)))
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
                  (caar x)
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
                                        for x from 1 to (or table-width 0)
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
         (data table)))
  (:method ((table table) (index string)
            &key
              (type 'list)
              (test #'equal))
    (let ((index (position index (field-names table) :test test)))
      (when index
        (coerce (elt (data table) index) type))))
  (:method ((table table) (index list)
            &key
              (type 'list)
              (test #'equal))
    (when index
      (map type
           (lambda (f) (table-ref table f :test test))
           index))))

;;; Table mapping:
(defun table-map (row-fn table
                  &key
                    (type 'table)
                    field-names)
  "Maps a function row-fn across table, supplying each field as a
distinct argument to row-fn, and returning a new table with fields
specified by field-names.  row-fn should return a list of new field
values.

type can be one of 'table, 'array, 'list, or 'plist to yield
difference result types.  For array, first index yields row, second
yields field/column.  For list, result is a list of field-lists."
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
                (table-ref table i :type 'list))))
      (plist
       (let* ((field-names
                (or field-names
                    (field-names table)))
              (field-syms (mapcar (lambda (s) (intern s :keyword))
                                  field-names)))
         (mapcar (lambda (row)
                   (mapcan (lambda (k f)
                             (list k f))
                           field-syms
                           row))
                 (table-map row-fn table :type 'list)))))))

;; Utilities using map:
(defun table->plist (table)
  (table-map #'list table :type 'plist))

(defun table->list (table)
  (table-map #'list table :type 'list))

(defun table->array (table)
  (table-map #'list table :type 'array))

;;; Column addition:
(defgeneric add-field! (table name
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
                    (data table))))
      (with-accessors ((field-names field-names)
                       (data        data))
          table
        (if end-p
            (setf data (nconc data (list column))
                  field-names (nconc field-names (list name)))
            (setf data (cons column data)
                  field-names (cons name field-names)))))
    table))

(defgeneric del-field! (table name-or-index)
  (:documentation "Remove column from table specified by name or index.")
  (:method ((table table) name-or-index)
    (with-accessors ((field-names field-names)
                     (data data))
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

(defgeneric insert! (table &rest rows)
  (:documentation "Insert row into table.  Rows should be lists.")
  (:method ((table table) &rest rows)
    (with-accessors ((data data)) table
      (flet ((insert (row)
               (map nil (lambda (col new-field)
                          (vector-push-extend new-field col))
                    data
                    row)))
        (map nil #'insert rows)
        table))))

(defgeneric delete! (table condition &key &allow-other-keys)
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
    (with-accessors ((data data))
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
    (delete! table (list condition)))
  ;; Boolean function case:
  (:method ((table table) (condition function) &key &allow-other-keys)
    (let ((indices nil))
      (let ((i 0))
        (dotable (row table)
          (when (apply condition
                       row)
            (push i indices))
          (incf i)))
      (delete! table
               (nreverse indices)
               :sorted-p t))))

(defgeneric truncate! (table)
  (:documentation "Truncates table (deletes all rows, preserves
 structure).")
  (:method ((table table))
    (map nil (lambda (column) (adjust-array column 0 :fill-pointer t))
         (data table))
    table))

;;; Aggregations:
;;;
;;; This was a surprisingly difficult concept for me to find a
;;; suitable, beautiful, Lispy expression, but ultimately the answer
;;; was simple: Closures.
;;;
;;; - There is a main aggregation function: #'aggregate.  This
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
;;;   used to return an aggregate table using #'aggregate.

;; Some basic aggregate functions:
(defun agg (fn &optional (default-value 0))
  "Takes any function accepting at least 2 optional arguments and converts it to
an aggregate function.  This works perfectly for functions that need
no state management, aka pure functions.  It may work in other cases
on a case-by-case basis.

Use this as a reference implementation for your own aggregation functions: They must accept a context, and then optional arguments for the accumulation and new datum."
  (lambda (&optional group)
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
      "Counting aggregation for use with aggregate.  See all aggregates
via (apropos \"agg-\")")

;; Sum aggregation
(setf (symbol-function 'agg-sum)
      (agg #'+ 0))
(setf (documentation #'agg-sum 'function)
      "Sum aggregation for use with aggregate.  See all aggregates
via (apropos \"agg-\")")

;; (Natural) Log-sum aggregration (log of product)
(setf (symbol-function 'agg-log-sum)
      (agg (lambda (v x)
             (+ v (log x)))
           0))
(setf (documentation #'agg-log-sum 'function)
      "Sum-of-logs or log-of-product aggregation for use with
aggregate.  See all aggregates via (apropos \"agg-\")")

;; Mean aggregation
(defun agg-mean (&optional (type 'number))
  "Mean aggregation function.  See all aggregates via (apropos \"agg-\")"
  (lambda (&optional group)
    (declare (ignore group))
    (let ((count (coerce 0 type))
          (sum (coerce 0 type)))
      (lambda (&optional datum)
        (if datum
            (setf count (1+ count)
                  sum (+ sum datum))
            (cl-ana.gmath:protected-/ sum count))))))

(defun aggregate (aggregator table
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
(defmacro with-aggregation (group agg-bindings
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

table-field-lambda-list can be a lambda list for the function that
will receive all table fields as distinct arguments, or it can just be
a list of symbols to be bound to those fields.  This is for
convienence, as an implicit &optional will be added to the front of
the lambda list.  Omitting the &optional was found to be a common
mistake, hence this special behavior.

This is useful for creating an aggregation to use with
aggregate.

If group is NIL, then the group argument will not be accessible nor
used in the aggregation (assumes single-group and therefore reasonable
group-fn, e.g. (constantly t)).

The first forms in agg-body can be declarations for the generated
aggregation function, i.e. they can declare things about the arguments
in the lambda list but not about the group (might fix in future)."
  (alexandria:with-gensyms (args)
    (let ((group (or group (gensym "group")))
          (table-field-lambda-list
            (if (or (member '&rest table-field-lambda-list)
                    (member '&optional table-field-lambda-list))
                table-field-lambda-list
                (cons '&optional table-field-lambda-list)))
          (aggs (loop
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
             (symbol-macrolet ,(loop
                                 for b in agg-bindings
                                 for fsym = (first b)
                                 collecting
                                 `(,fsym (,fsym)))
               (lambda (&rest ,args)
                 (destructuring-bind ,table-field-lambda-list ,args
                   ,@declarations
                   (if ,args
                       (progn ,@agg-body)
                       ,agg-result))))))))))

;;; Joins
;;;
;;; Joins are a place where some real benefits can be had through
;;; Lisp's flexible syntax and functional techniques.

;; Starting from simple approach: Binary join functions
(defun %join-loop (t1 t2 condition-fn
                   &key (type :inner)
                     field-names)
  "Accepts

* Tables t1 and t2.

* Condition function which accepts a row from each table and returns T
or NIL when a join of the rows should be accepted or rejected.

* Type of join: :inner (default), :left, :right, or :full."
  ;; I'm using a bit-vector to indicate which rows from the right
  ;; table have been included in the result to support the case of the
  ;; full join.  There might be other approaches that work better, but
  ;; this is easy to write and not that bad memory wise.
  (let* (;; right index vector:
         ;; 1 if already included in result, 0 if not.
         (ri (when (eq type :full)
               (make-array (table-length t2)
                           :element-type 'bit
                           :initial-element 0)))
         (field-names (or field-names
                          (append (field-names t1)
                                  (field-names t2))))
         ;; results pushed into list instead of table for efficiency
         (result nil)
         (i2 0)
         included-p)
    ;; switching arguments for right join
    (when (eq type :right)
      (rotatef t1 t2))
    (dotable (r1 t1)
      (setf i2 0
            included-p nil)
      (dotable (r2 t2)
        (let ((j (funcall condition-fn r1 r2)))
          (when j
            (push (append r1 r2) result)
            (when (and (not (eq type :inner))
                       (not included-p))
              (setf included-p t))
            (when (eq type :full)
              (setf (elt ri i2) 1))))
        (incf i2))
      (when (and (not (eq type :inner))
                 (not included-p))
        (push (append r1 (loop repeat (table-width t2) collect nil))
              result)))
    ;; fixing data & tables for right join:
    (when (eq type :right)
      (setf result
            (mapcar (lambda (fs)
                      (append (subseq fs (table-width t1))
                              (subseq fs 0 (table-width t1))))
                    result))
      (rotatef t1 t2))
    ;; post-processing missing right rows for full join:
    (when (eq type :full)
      (dotimes (i (table-length t2))
        (when (zerop (elt ri i))
          (push (append (loop repeat (table-width t1) collect nil)
                        (table-ref t2 i))
                result))))
    (make-table (nreverse result) :field-names field-names)))

;; Binary hash equijoin function:
;;
;; Arbitrary hashes of rows are also supported by passing (lfn rfn) as
;; eqs instead of a list of equivalent field names/indices.
;;
;; NOTE: This function still supports the index joining, but I have
;; not used this in the general #'join front-end function, as the
;; explicit function is sufficient.
(defun %join-hash (t1 t2 eqs
                   &key
                     (test 'equal)
                     (type :inner)
                     field-names)
  "Uses hash tables to match rows using lists of keys on which to join
the tables as specified in eqs.

eqs should be a list of equivalence lists or a list of two functions
that will be applied to the input rows (with fields supplied as
distinct arguments) to compute values to use as hash keys.

When using equivalence lists, each equivalence list should contain 2
elements:

* A field name or index from t1.
* A field name or index from t2.

These pairs of field specifiers indicate an equivalence relationship
between the fields specified, which in combination will be used to
perform the join.

test can be one of the supported tests for hash tables.

type can be one of :inner (default), :left, :right, or :full."
  (declare (optimize (debug 3)))
  (let* (;; left index vector:
         ;; 1 if already included in result, 0 if not.
         (li (when (or (eq type :full)
                       (eq type :left))
               (make-array (table-length t1)
                           :element-type 'bit
                           :initial-element 0)))
         ;; right index vector:
         ;; 1 if already included in result, 0 if not.
         (ri (when (or (eq type :full)
                       (eq type :right))
               (make-array (table-length t2)
                           :element-type 'bit
                           :initial-element 0)))
         (field-names (or field-names
                          (append (field-names t1)
                                  (field-names t2))))
         ;; results pushed into list instead of table for efficiency
         (result nil)
         (lmap (make-hash-table :test test))
         (rmap (make-hash-table :test test)))
    (let* ((leq-p (not (functionp (first eqs))))
           (req-p (not (functionp (second eqs))))
           (lfspecs (when leq-p (mapcar #'second eqs)))
           (rfspecs (when req-p (mapcar #'first eqs)))
           (lfindices (when leq-p
                        (sort (loop
                                for f in lfspecs
                                collecting
                                (if (integerp f)
                                    f
                                    (position f
                                              (field-names t1)
                                              :test #'equal)))
                              #'<)))
           (rfindices (when req-p
                        (sort (loop
                                for f in rfspecs
                                collecting
                                (if (integerp f)
                                    f
                                    (position f
                                              (field-names t1)
                                              :test #'equal)))
                              #'<))))
      (labels (;; functions to grab keys for right and left tables
               (getfields (row indices)
                 (do* ((r row (cdr r))
                       (f (car r) (car r))
                       (i 0 (1+ i))
                       (result nil)
                       (tail nil))
                      ((or (null indices)
                           (null r))
                       result)
                   (when (= i (car indices))
                     (if result
                         (setf (cdr tail) (cons f nil)
                               tail (cdr tail))
                         (setf result
                               (setf tail (cons f nil))))
                     (setf indices (cdr indices)))))
               (rfields (row)
                 (getfields row rfindices))
               (lfields (row)
                 (getfields row lfindices)))
        (let* ((lkey (if leq-p
                         #'lfields
                         (lambda (x) (apply (first eqs) x))))
               (rkey (if req-p
                         #'rfields
                         (lambda (x) (apply (second eqs) x)))))
          ;; setup maps
          (let ((i -1))
            (dotable (r1 t1)
              (push (incf i) (gethash (funcall lkey r1) lmap))))
          (let ((i -1))
            (dotable (r2 t2)
              (push (incf i) (gethash (funcall rkey r2) rmap)))))
        ;; Push intersection results:
        (loop
          for k being the hash-keys in lmap
          for lindices being the hash-values in lmap
          do
             (let ((rindices (gethash k rmap)))
               ;; mark rows as being inserted
               (when (and lindices rindices)
                 (map nil
                      (lambda (indices)
                        (map nil
                             (lambda (i)
                               (when (zerop (elt indices i))
                                 (setf (elt indices i) 1)))
                             indices))
                      (list li ri)))
               ;; insert intersection rows
               (when rindices
                 (let ((r1s (mapcar (lambda (i)
                                      (table-ref t1 i))
                                    lindices))
                       (r2s (mapcar (lambda (i)
                                      (table-ref t2 i))
                                    rindices)))
                   (dolist (r1 r1s)
                     (dolist (r2 r2s)
                       (push (append r1 r2)
                             result)))))))
        ;; Handle left/right/full non-intersections:
        (labels ((lins ()
                   (dotimes (i (length li))
                     (let ((present (plusp (elt li i))))
                       (unless present
                         (push (append (table-ref t1 i)
                                       (loop
                                         repeat (table-width t2)
                                         collect nil))
                               result)))))
                 (rins ()
                   (dotimes (i (length ri))
                     (let ((present (plusp (elt ri i))))
                       (unless present
                         (push (append (loop
                                         repeat (table-width t1)
                                         collect nil)
                                       (table-ref t2 i))
                               result))))))
          (case type
            (:full (lins) (rins))
            (:left (lins))
            (:right (rins))))
        ;; Return result:
        (make-table (nreverse result)
                    :field-names field-names)))))

;; Now provide nice front-end to the two previous binary join
;; operations:
(defgeneric on (table condition &key type &allow-other-keys)
  (:documentation "Returns a specification for a join condition that can be used by
#'join to compute the join using the best approach.")
  (:method ((table table) (condition function)
            &key (type :inner) &allow-other-keys)
    (list :loop table condition type))
  (:method ((table table) (condition list)
            &key (type :inner) (test 'equal) &allow-other-keys)
    (list :hash table condition type test)))

(defun reverse-group-list (list group-lengths)
  "Returns elements from list grouped into group sizes specified by
group-lengths."
  (let ((l list))
    (nreverse
     (mapcar (lambda (g)
               (prog1 (subseq l 0 g)
                 (setf l (nthcdr g l))))
             group-lengths))))

(defun join (table &rest joins)
  "Performs a series of joins on table using the specified list of joins.

Each join is a join specification returned by the #'on function.

Example of joining 3 tables first using a primary & foreign
key (efficient, uses hash tables to join) followed by a more general
condition (inefficient, uses nested loop):

(join t1
      ;; key/hash join:
      (on t2
          ;; List of functions to generate values to hash & compare,
          ;; one from left table(s), one from right table
          (list (lambda (r1)
                 (let ((foreign-key (elt r1 1)))
                  foreign-key))
                (lambda (r2)
                 (let ((primary-key (elt r2 0)))
                  primary-key)))
             :type :inner)
      ;; general condition join:
      (on t3 (lambda (r3 r2 r1) (evenp (+ (elt r3 0) (elt r2 1))))
          :type :inner))

* Note that the arguments to the first condition in a key/hash join's
  left function are the rows of the already-joined tables in reverse
  order of the table occurrences.  This is to make it convenient to
  ignore arguments for tables that are unlikely to be involved in
  future joins.

* Note that the arguments to the second join's condition function are
  rows from the tables supplied in reverse order to the occurence of
  tables in the join.  This was chosen to make it convenient to ignore
  arguments for tables that are unlikely to be involved in future
  joins."
  (declare (optimize (debug 3)))
  (let* ((result table)
         (tables (cons table
                       (mapcar #'second joins)))
         (table-widths (mapcar #'table-width tables)))
    (loop
      for join in joins
      for i from 0
      for algo = (first join) ; e.g. :loop, :hash
      for table = (second join)
      for condition = (third join)
      for type = (fourth join) ; e.g. :inner
      for join-test = (fifth join)
      for traversed = (list result) then (append traversed (list table))
      do
         (setf result
               (case algo
                 (:loop
                   (%join-loop result table
                               (let ((gls
                                       (subseq table-widths 0 (1+ i))))
                                 (lambda (left right)
                                   (apply condition right
                                          (reverse-group-list
                                           left gls))))
                               :type type))
                 (:hash
                  (let ((eqs condition))
                    (%join-hash result table
                                (destructuring-bind (lf rf) eqs
                                  (let ((gls
                                          (subseq table-widths 0 (1+ i))))
                                    (list
                                     (lambda (&rest left-fields)
                                       (apply lf
                                              (reverse-group-list
                                               left-fields gls)))
                                     (lambda (&rest right-fields)
                                       (funcall rf right-fields)))))
                                :type type
                                :test join-test))))))
    result))

(defun union (tables
              &key
                all-p
                (test #'equal))
  "Returns the union/union-all of the input tables using the test
function to compare rows of data.  If all-p is T, then union-all is
used rather than union (i.e. all rows are included)."
  (let ((fns (field-names (first tables))))
    (make-table
     (if all-p
         (apply #'append
                (mapcar #'table->list tables))
         (let ((result nil))
           (dolist (x (apply #'append
                             (mapcar #'table->list tables))
                      (nreverse result))
             (pushnew x result :test test))))
     :field-names fns)))
