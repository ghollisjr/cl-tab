(declaim (optimize (speed 3)))

(in-package :tb)

;;;; Control Parameters
(defparameter *field-name-case-convert*
  nil
  "Controls how field names are converted to-from symbols.

* NIL      Leave case untouched.  Can lead to ugly symbols.
* T        Up-case all field names.  Bad for preserving field names.
* :invert  All field name strings are down-cased, all symbols up-cased")

(defclass table ()
  ((data :initarg :data
         :documentation "table data list of column arrays"
         :accessor data
         :type list)
   (field-names :initarg :field-names
                :documentation "list of table field names"
                :reader field-names ; writer defined below
                :type list)
   (field-keywords :initarg :field-keywords
                   :documentation "list of table field name keyword symbols"
                   :reader field-keywords ; writer defined below
                   :type list)
   (field-map :initarg :field-map
              :initform (make-hash-table :test #'equal)
              :documentation "hash table mapping from field name to column index"
              :accessor field-map
              :type hash-table)
   (keyword-map :initarg :keyword-map
                :initform (make-hash-table :test #'eq)
                :documentation "hash table mapping from field keyword to column index"
                :accessor keyword-map
                :type hash-table)))

(defun %init-field-keyword-maps! (table)
  "Setup field-map & keyword-map using field-names in table"
  (clrhash (field-map table))
  (loop
    for f in (field-names table)
    for k in (field-keywords table)
    for i from 0
    do (setf (gethash k (keyword-map table))
             (setf (gethash f (field-map table)) i))))

(defmethod (setf field-names) (new (table table))
  (setf new (proper-field-names new)
        (slot-value table 'field-names) new)
  (setf (slot-value table 'field-keywords)
        (field-names->keywords new))
  (%init-field-keyword-maps! table)
  new)

(defmethod (setf field-keywords) (new (table table))
  ;; rely on (setf field-names) to handle everything
  (setf (field-names table)
        (mapcar #'string new))
  (field-keywords table))

;;; Proper field names:
;;;
;;; Proper field names are a list of field names so that every name is
;;; unique and obeys the case convention set by
;;; *field-name-case-convert*.
(defun proper-field-names (field-names)
  "Accepts a list of field names and modifies them to ensure that each
field is unique and adheres to the standard set by
*field-name-case-convert*.

Any field name that collides with a previous field name will have a
#\. character prepended, repeating until it no longer collides with
any other field names."
  (let ((result (make-array (length field-names)
                            :initial-contents field-names)))
    (flet ((collides-p (n &optional (end 0))
             (find n result :test #'equal :end end)))
      (loop
        for n in field-names
        for i from 0
        do
           (case *field-name-case-convert*
             ((T) (setf n (string-upcase n)))
             (:invert (setf n (string-downcase n))))
           (loop while (collides-p n i)
                 do (setf n (concatenate 'string "." n)))
           (setf (aref result i) n)
        finally (return (coerce result 'list))))))

(defun field-names->keywords (proper-field-names)
  "Returns a list of keyword symbols from proper-field-names using the
convention set by *field-name-case-convert*"
  (mapcar (lambda (s)
            (case *field-name-case-convert*
              (:invert (intern (string-upcase s) :keyword))
              (t (intern s :keyword))))
          proper-field-names))

(defmethod initialize-instance :after ((tab table) &rest init-args)
  (declare (ignorable init-args))
  ;; First fix field names
  (setf (slot-value tab 'field-names)
        (proper-field-names (field-names tab)))
  ;; Then set field symbols:
  (setf (slot-value tab 'field-keywords)
        (field-names->keywords (field-names tab)))
  ;; Establish name->index map
  (%init-field-keyword-maps! tab))

(defgeneric table-length (table)
  (:documentation "Returns number of rows of table")
  (:method ((table table))
    (length (first (data table)))))
(setf (symbol-function 'tlength)
      #'table-length)

(defgeneric table-width (table)
  (:documentation "Returns number of fields/columns of table")
  (:method ((table table))
    (length (data table))))
(setf (symbol-function 'twidth)
      #'table-width)

(defgeneric make-table (data &key field-names &allow-other-keys)
  (:documentation
   "Generic table creation function.  Should have methods to e.g. create
from plists, alists, other tables, CSVs, SQL queries etc.")
  ;; basic method for tables:
  (:method ((data table)
            &key
              field-names
              empty-p)
    (make-instance 'table
                   :data
                   (unless empty-p
                     (mapcar #'alexandria:copy-array
                             (data data)))
                   :field-names (if field-names
                                    field-names
                                    (copy-list (field-names data))))))

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
  (declare (list field-names))
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
  (:documentation "Return a row/column from the table as a sequence of data.
type controls type of sequence.  If index is integer/list of integers,
return row(s).  If index is string/list of strings, return
field/column(s).")
  (:method ((table table) (index integer)
            &key (type 'plist))
    (case type
      (plist
       (mapcan (lambda (k v)
                 (list k v))
               (field-keywords table)
               (mapcar (lambda (column) (aref column index))
                       (data table))))
      (t
       (map type
            (lambda (column)
              (aref column index))
            (data table)))))
  (:method ((table table) (index string)
            &key
              (type 'list))
    (let ((index (gethash index (field-map table))))
      (when index
        (coerce (elt (data table) index) type))))
  (:method ((table table) (index list)
            &key
              (type 'plist)
              (test #'equal))
    (when index
      (mapcar
       (lambda (f) (table-ref table f
                         :test test
                         :type (if (and (stringp f)
                                        (eq type 'plist))
                                   'list
                                   type)))
       index))))
(setf (symbol-function 'tref) #'table-ref)

(defmethod (setf table-ref) (new (table table) index &key (type 'plist))
  (with-accessors ((data data)
                   (keyword-map keyword-map))
      table
    (case type
      (vector
       (loop
         for c in data
         for n across new
         do (setf (aref c index) n)))
      (list
       (loop
         for c in data
         for n in new
         do (setf (aref c index) n)))
      (plist
       (do ((nnew new (cddr nnew)))
           ((null nnew))
         (let* ((k (car nnew))
                (v (cadr nnew))
                (i (gethash k keyword-map)))
           (setf (aref (elt data i) index)
                 v)))))
    new))
(setf (fdefinition '(setf tref))
      (function (setf table-ref)))

;;; Table mapping:
(defun table-map (row-fn table
                  &key
                    (type 'table)
                    (field-names nil field-names-supplied-p))
  "Maps a function row-fn across table, supplying each field as a
distinct keyword argument to row-fn using the field-keywords from
table, and returning a new table with fields specified either by the
keyword symbols in the result list or explicitly by field-names.
row-fn should return a list of new field values as a plist, or as a
list of field values if field-names is supplied.  (Not supplying
field-names but only supplying a list of results will result in
default field names being supplied, e.g. X1, X2....)

type can be one of NIL, 'table, 'array, 'list, or 'plist to yield
difference result types.  For NIL, no result is returned.  For array,
first index yields row, second yields field/column.  For list, result
is a list of field-lists."
  (let* ((length (table-length table))
         (field-names (proper-field-names field-names))
         (field-name-check-p t)
         (field-keywords (field-names->keywords field-names)))
    (flet ((row->plist (row)
             ;; on first call, check for field-names and set correctly
             ;; if needed
             (when (and (not field-names-supplied-p)
                        field-name-check-p)
               (setf field-name-check-p nil)
               (when (keywordp (car row))
                 (setf field-names
                       (proper-field-names
                        (loop
                          for i from 0
                          for s in row
                          when (evenp i)
                            collect (string s)))))
               (unless field-names
                 (setf field-names
                       (proper-field-names
                        (loop
                          for x from 1 to (length row)
                          collecting
                          (format nil "x~a" x)))))
               (setf field-keywords
                     (field-names->keywords field-names)))
             (typecase (car row)
               (null nil)
               (keyword row)
               (t (mapcan (lambda (k v)
                            (list k v))
                          field-keywords
                          row))))
           (row->list (row)
             (typecase (car row)
               (null nil)
               (keyword (loop
                          for i from 0
                          for x in row
                          when (oddp i)
                            collect x))
               (t row))))
      (case type
        ((nil)
         (loop
           for i below length
           do
              (apply row-fn
                     (table-ref table i :type 'plist))))
        (table
         (make-table
          ;; code reuse
          (apply #'table-map row-fn table
                 :type 'plist
                 (when field-names-supplied-p
                   (list :field-names field-names)))))
        (array
         (let* ((width (table-width table))
                (result (make-array (list length width))))
           (loop
             for i below length
             do
                (let ((row
                        (row->list
                         (apply row-fn
                                (table-ref table i :type 'plist)))))

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
           (let ((r (apply row-fn
                           (table-ref table i :type 'plist))))
             (cond
               ((and (eq type 'list)
                     (atom r))
                r)
               ((null r) nil)
               (t (row->list r))))))
        (plist
         (loop
           for i below length
           collecting
           (row->plist
            (apply row-fn
                   (table-ref table i :type 'plist)))))))))
(setf (symbol-function 'tmap) #'table-map)

;; Utilities using map:
(defun table->plist (table)
  (table-map #'list table :type 'plist))

(defun table->list (table &optional flat-p)
  (let ((r (table-map #'list table :type 'list)))
    (if flat-p
        (apply #'append r)
        r)))

(defun table->array (table)
  (table-map #'list table :type 'array))

;;; Column addition:
(defgeneric add-field! (table name
                        &optional value-or-function
                        &key
                          end-p
                        &allow-other-keys)
  (:documentation "Adds a new column to table with name and either a fixed value, a list
of values, or a function which will be supplied the table's other
fields as distinct arguments and which should return a value to insert
as the new field value for each row.  If value-or-function is NIL, NIL
will be inserted as new value of field.  end-p controls whether to add
the column to the front of the columns list or the end.")
  (:method ((table table) name &optional value-or-list-or-function
            &key (end-p t)
            &allow-other-keys)
    (let* ((fn (typecase value-or-list-or-function
                 (null (constantly nil))
                 (list (let ((l value-or-list-or-function))
                         (lambda (&rest args)
                           (declare (ignore args))
                           (pop l))))
                 (function value-or-list-or-function)
                 (t (constantly value-or-list-or-function))))
           (column
             (make-array (tlength table)
                         :initial-contents (tmap fn table :type 'list)
                         :adjustable t
                         :fill-pointer t)))
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
  (:documentation "Insert row into table.  Rows should be plists or lists.")
  (:method ((table table) &rest rows)
    (with-accessors ((data data)) table
      (flet ((insert (row)
               (typecase (car row)
                 (keyword
                  (let ((row
                          (loop
                            for k in (field-keywords table)
                            collect
                            (getf row k))))
                    (map nil (lambda (col new-field)
                               (vector-push-extend new-field col))
                         data
                         row)))
                 (t
                  (map nil (lambda (col new-field)
                             (vector-push-extend new-field col))
                       data
                       row)))))
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
      (loop
        for i below (table-length table)
        for row = (table-ref table i :type 'plist)
        when (apply condition row)
          do (push i indices))
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
(defun agg-function (fn &optional (default-value 0))
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
      (agg-function (lambda (v x)
                      (declare (ignore x))
                      (1+ v))
                    0))
(setf (documentation #'agg-count 'function)
      "Counting aggregation for use with aggregate.  See all aggregates
via (apropos \"agg-\")")

;; Sum aggregation
(setf (symbol-function 'agg-sum)
      (agg-function #'+ 0))
(setf (documentation #'agg-sum 'function)
      "Sum aggregation for use with aggregate.  See all aggregates
via (apropos \"agg-\")")

;; (Natural) Log-sum aggregration (log of product)
(setf (symbol-function 'agg-log-sum)
      (agg-function (lambda (v x)
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

aggregator should accept all table fields as distinct input keyword
arguments and return a plist of values to be considered the fields for
the resulting aggregate table.  If lists of values are returned, then
field-names should be supplied or default field names will be
generated."
  ;; aggregation:
  (let* ((agg-map (make-hash-table :test test)))
    (loop
      for i below (table-length table)
      for row = (table-ref table i :type 'plist)
      do
         (let* ((group (apply group-fn row)))
           (let (aggfn)
             ;; ensure agg is in agg-map and get it into aggfn
             (unless (setf aggfn (gethash group agg-map))
               (setf aggfn
                     (setf (gethash group agg-map)
                           (funcall aggregator group))))
             ;; Execute aggregation
             (apply aggfn row))))
    ;; Form new table by a call to each aggfn in agg-map with not
    ;; argument:
    (make-table
     (loop
       for key being the hash-keys in agg-map
       for aggfn being the hash-values in agg-map
       collecting (alexandria:ensure-list (funcall aggfn)))
     :field-names field-names)))
(setf (symbol-function 'agg) #'aggregate)

;;; Filters
;;;
;;; I thought about using #'where to be more like SQL, but #'filter
;;; suits my taste better.
(defgeneric filter (function table &key &allow-other-keys)
  (:documentation "Filters a table using boolean-valued function applied to each row
which receives fields as distinct arguments.")
  (:method (fn (tab table) &key &allow-other-keys)
    (make-table
     (remove-if-not (lambda (row)
                      (apply fn row))
                    (table->plist tab))
     :field-names (field-names tab))))


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
         (lfns (field-names t1))
         (rfns (field-names t2))
         (field-names (if field-names
                          field-names
                          (proper-field-names
                           (append lfns rfns))))
         (new-rfns (subseq field-names
                           (length lfns)))
         (rfns-map (let ((ht (make-hash-table :test 'eq)))
                     (loop
                       for old in (field-names->keywords rfns)
                       for new in (field-names->keywords new-rfns)
                       do (setf (gethash old ht) new))
                     ht))
         (field-names (or field-names
                          (append lfns
                                  rfns)))
         ;; results pushed into list instead of table for efficiency
         (result nil)
         included-p)
    (flet ((fix-right-fields (field-plist)
             ;; Fixes plist so that the fields coming from plist are
             ;; converted to match the new field names
             (let ((result (copy-list field-plist)))
               (loop
                 for c on result
                 for i from 0
                 when (evenp i)
                   do (setf (car c)
                            (gethash (car c) rfns-map))
                 finally (return result)))))

      ;; switching arguments for right join
      (when (eq type :right)
        (rotatef t1 t2))
      (loop
        for i1 below (tlength t1)
        for r1 = (if (eq type :right)
                     (fix-right-fields (tref t1 i1))
                     (tref t1 i1))
        do (setf included-p nil)
           (loop
             for i2 below (tlength t2)
             for r2 = (if (not (eq type :right))
                          (fix-right-fields (tref t2 i2))
                          (tref t2 i2))
             do
                (let ((j (apply condition-fn (append r1 r2))))
                  (when j
                    (push (if (eq type :right)
                              (append r2 r1)
                              (append r1 r2))
                          result)
                    (when (and (not (eq type :inner))
                               (not included-p))
                      (setf included-p t))
                    (when (eq type :full)
                      (setf (elt ri i2) 1)))))
           (when (and (not (eq type :inner))
                      (not included-p))
             (push r1
                   result)))
      ;; fixing data & tables for right join:
      (when (eq type :right)
        (rotatef t1 t2))
      ;; post-processing missing right rows for full join:
      (when (eq type :full)
        (dotimes (i (table-length t2))
          (when (zerop (elt ri i))
            (push (fix-right-fields (table-ref t2 i))
                  result))))
      (make-table (nreverse result) :field-names field-names))))

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
                                              (the list (field-names t1))
                                              :test #'equal)))
                              #'<)))
           (rfindices (when req-p
                        (sort (loop
                                for f in rfspecs
                                collecting
                                (if (integerp f)
                                    f
                                    (position f
                                              (the list (field-names t1))
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
                   (declare (fixnum i))
                   (when (= i (the fixnum (car indices)))
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
                         (first eqs)))
               (rkey (if req-p
                         #'rfields
                         (second eqs))))
          ;; setup maps
          (loop
            for i below (table-length t1)
            for r1 = (table-ref t1 i :type 'plist)
            do
               (push i (gethash (apply lkey r1) lmap)))
          (loop
            for i below (table-length t2)
            for r2 = (table-ref t2 i :type 'plist)
            do
               (push i (gethash (apply rkey r2) rmap))))
        ;; Push intersection results:
        (loop
          for k being the hash-keys in lmap
          for lindices being the hash-values in lmap
          do
             (let ((rindices (gethash k rmap)))
               ;; mark rows as being inserted
               (when (and lindices rindices)
                 (when li
                   (map nil
                        (lambda (i)
                          (when (zerop (elt li i))
                            (setf (elt li i) 1)))
                        lindices))
                 (when ri
                   (map nil
                        (lambda (i)
                          (when (zerop (elt ri i))
                            (setf (elt ri i) 1)))
                        rindices)))
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
  (let* ((result table))
    (loop
      for join in joins
      for i from 0
      for algo = (first join) ; e.g. :loop, :hash
      for table = (second join)
      for condition = (third join)
      for type = (fourth join) ; e.g. :inner
      for join-test = (fifth join)
      do
         (setf result
               (case algo
                 (:loop
                   (%join-loop result table
                               (the function condition)
                               :type type))
                 (:hash
                  (%join-hash result table condition
                              :type type
                              :test join-test)))))
    result))

(defun union (tables
              &key
                all-p
                (test #'equal))
  "Returns the union/union-all of the input tables using the test
function to compare rows of data.  If all-p is T, then union-all is
used rather than union (i.e. all rows are included)."
  (let* ((tables (remove nil tables))
         (fns (when tables
                (field-names (first tables)))))
    (make-table
     (if all-p
         (apply #'append
                (mapcar #'table->list
                        tables))
         (let ((result nil))
           (dolist (x (apply #'append
                             (mapcar #'table->list
                                     tables))
                      (nreverse result))
             (pushnew x result :test test))))
     :field-names fns)))

(defun distinct (table &key
                         (row-fn #'list)
                         (test 'equal))
  "Uses a hash-table to ensure that each row of the table is distinct
according to the values of row-fn applied to each row and using the
supplied hash test parameter."
  (let ((ht (make-hash-table :test test))
        (rows nil))
    (dotimes (i (table-length table))
      (let* ((row (table-ref table i))
             (key (apply row-fn row)))
        (unless (gethash key ht)
          (setf (gethash key ht) t)
          (push i rows))))
    (make-table
     (loop
       for i in (nreverse rows)
       collect (table-ref table i))
     :field-names (field-names table))))

(defun table-difference (table1 table2
                         &key
                           (test #'equal)
                           symmetric-p)
  "Returns rows from table1 that are not present in table2 according to
test function.

If symmetric-p is T, then any rows that are only in either table are
returned rather than rows only in table1."
  (let ((result
          (make-table
           (set-difference (table->plist table1)
                           (table->plist table2)
                           :test test))))
    (when symmetric-p
      (setf result
            (union (list result
                         (make-table
                          (set-difference (table->plist table2)
                                          (table->plist table1)
                                          :test test)))
                   :all-p t ; duplicates preserved
                   :test test)))
    result))
(setf (symbol-function 'tdiff) #'table-difference)

(defun update! (table set-fn
                &optional
                  (condition (constantly t))
                &key (type 'plist))
  "Updates table by applying set-fn to each row.  set-fn should return a
new row of values to replace the current row.

condition can be

* a function receiving a row as a plist
* an index denoting a specific row to update
* a list of indices denoting rows to update

type can be one of 'plist, 'list, or 'vector denoting the type of
sequence returned by the set-fn."
  (let ((indices
          (typecase condition
            (function
             (loop
               for i below (tlength table)
               for row = (tref table i :type 'plist)
               when (apply condition row)
                 collect i))
            (integer (list condition))
            (list condition))))
    (loop
      for i in indices
      do (setf (tref table i :type type)
               (apply set-fn
                      (tref table i :type 'plist))))
    table))

(defun top (table &optional (n 1))
  "Returns top row of table as p-list"
  (when (plusp (tlength table))
    (subseq (table->plist table) 0 n)))
