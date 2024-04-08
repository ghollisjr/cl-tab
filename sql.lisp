;;;; Utility functions for working with queries, e.g. execute a query
;;;; and return the results as a table.
(in-package :tb)

(defgeneric query (expr &key database result-p &allow-other-keys)
  (:documentation "Runs query or queries based on expr using database and returning result depending on result-p"))

(defmethod query ((query-expression string)
                  &key
                    (database clsql-sys:*default-database*)
                    (result-p t)
                    &allow-other-keys)
  "Front-end to clsql's query and execute-command functions that returns
a table of results when result-p is non-NIL, and nothing when
result-p is NIL."
  (if result-p
      (multiple-value-bind (results columns)
          (clsql:query query-expression
                       :database database)
        (make-table results :field-names columns))
      (clsql:execute-command query-expression
                             :database database)))

(defmethod query ((q list)
                  &key
                    (database clsql-sys:*default-database*)
                    (result-p t)
                  &allow-other-keys)
  (if result-p
      (mapcar (lambda (s) (query s :database database :result-p result-p))
              q)
      (map nil (lambda (s) (query s :database database :result-p result-p))
           q)))

(defun sql-type (obj)
  "Infer SQL type from object"
  (typecase obj
    (character "CHAR")
    (string "VARCHAR(65535)")
    (symbol "VARCHAR(65535)")
    (integer "INTEGER")
    (float "FLOAT")
    ;; default to strings
    (t "VARCHAR(65535)")))

(defun table-sql-types (table)
  "Infer SQL types from table"
  (mapcar (lambda (d)
            (sql-type (aref d 0)))
          (data table)))

(defun sql-convert (obj)
  "Convert object to string suitable for SQL queries"
  (symbol-macrolet ((s (clsql-sys:sql-escape-quotes
                        (format nil "~a" obj))))
    (typecase obj
      (character (format nil "'~a'" s))
      (string (format nil "'~a'" s))
      (null "null")
      (symbol (format nil "'~a'" s))
      (integer (format nil "~a" obj))
      (float (format nil "~f" obj))
      (t (format nil "'~a'" s)))))

(defgeneric table->sql (table name
                        &key
                          execute-p
                          database
                          db-type
                          types
                          batch-size
                        &allow-other-keys)
  (:documentation "Generates a list of SQL queries to create the table and insert data.
If execute-p is non-NIL (default), executes those queries on the
database specified.

* execute-p: Boolean controlling whether statements should be executed
  or only returned as a list of query strings.

* database: Database used for the queries.

* db-type (CURRENTLY UNUSED) controls type of db server,
  e.g. :postgresql.

* types is an optional list of SQL types for the columns.  If not
  supplied, they will be inferred via #'table-sql-types.

* batch-size controls the number of rows to attempt to insert in a
  single insert statement.")
  (:method ((table table) name
            &key
              (execute-p t)
              (database clsql:*default-database*)
              db-type
              types
              (batch-size 1)
            &allow-other-keys )
    (declare (ignore db-type))
    (let* ((types (or types (table-sql-types table)))
           (queries
             (list* (format nil
                            "create table ~a (~{\"~a\" ~a~^, ~});"
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
                                   collect (table-ref table j :type 'list)))
                               (vstrings
                                 (mapcar (lambda (row)
                                           (format nil "(~{~a~^, ~})"
                                                   (mapcar #'sql-convert row)))
                                         batch)))
                          (format nil "insert into ~a values ~{~a~^, ~};"
                                  name
                                  vstrings)))))))
      (if execute-p
          (map nil
               (lambda (q) (clsql:execute-command q :database database))
               queries)
          queries))))
