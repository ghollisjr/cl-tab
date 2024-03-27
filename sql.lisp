;;;; Utility functions for working with queries, e.g. execute a query
;;;; and return the results as a table.
(in-package :tb)

(defun query (query-expression
              &key (database clsql-sys:*default-database*))
  (multiple-value-bind (results columns)
      (clsql:query query-expression
                   :database database)
    (make-table results :field-names columns)))

(defun sql-type (obj)
  (typecase obj
    (character "CHAR")
    (string "VARCHAR(65535)")
    (symbol "VARCHAR(65535)")
    (integer "INTEGER")
    (float "FLOAT")
    ;; default to strings
    (t "VARCHAR(65535)")))

(defun sql-convert (obj)
  (typecase obj
    (character (format nil "'~a'" obj))
    (string (format nil "'~a'" obj))
    (null "null")
    (symbol (format nil "'~a'" obj))
    (integer (format nil "~a" obj))
    (float (format nil "~f" obj))
    (t (format nil "'~a'" obj))))

(defun table-sql-types (table)
  "Tries to infer SQL types from table"
  (mapcar (lambda (d)
            (sql-type (aref d 0)))
          (data table)))

(defgeneric table->sql (table name
                        &key
                          db-type
                          types
                          batch-size
                        &allow-other-keys)
  (:documentation "Generates a list of SQL queries to create the table and insert data.

* db-type (CURRENTLY UNUSED) controls type of db server,
  e.g. :postgresql.

* types is an optional list of SQL types for the columns.  If not
  supplied, they will be inferred via #'table-sql-types.

* batch-size controls the number of rows to attempt to insert in a
  single insert statement.")
  (:method ((table table) name &key db-type types (batch-size 1) &allow-other-keys )
    (declare (ignore db-type))
    (let ((types (or types (table-sql-types table))))
      (list* (format nil
                     "create table ~a (~{~a ~a~^, ~});"
                     name
                     (apply #'append
                            (mapcar #'list
                                    (field-names table)
                                    types)))
             (let ((nbatches (ceiling (table-length table)
                                      batch-size)))
               (loop
                 for i below nbatches
                 collect
                 (let* ((batch
                          (loop
                            for j from (* i batch-size)
                              below (min (table-length table)
                                         (* (1+ i) batch-size))
                            collect (table-ref table j)))
                        (vstrings
                          (mapcar (lambda (row)
                                    (format nil "(~{~a~^, ~})"
                                            row))
                                  batch)))
                   (format nil "insert into ~a values ~{~a~^, ~};"
                           name
                           vstrings))))))))
