;;;; Utility functions for working with queries, e.g. execute a query
;;;; and return the results as a table.
(in-package :tb)

(defun query (query-expression
              &key (database clsql-sys:*default-database*))
  (multiple-value-bind (results columns)
      (clsql:query query-expression
                   :database database)
    (make-table results :field-names columns)))
