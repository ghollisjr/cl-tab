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

;; possibly most used: parse different types of lists to create table
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
           (setf field-names
                 (mapcar #'string field-symbols))
           (setf table-length (length data))
           (setf table-width (length field-symbols))
           (setf table-data
                 (loop
                   for i below table-width
                   collecting (make-array table-length)))
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
               table-width (length (car data)))
         (setf table-data
               (loop
                 for i below table-width
                 collecting (make-array table-length)))
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
