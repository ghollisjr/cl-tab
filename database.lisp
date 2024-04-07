(in-package :tb)

(defstruct database
  server
  name
  connection-spec
  connection)

(defmacro define-database (symbol
                           &body connection-body)
  "Defines a new function bound to symbol which controls a smart database
connection object.  The idea is to define a database connection, and
then call that function with a query whenever you want to run queries
against that database.  However, unless the function is actually
called, no connections to the database are attempted.

Lists of queries are also supported.

If :info is passed to the function, the connection information is
shown.

If :disconnect is passed to the function, the connection is closed.
However, if another query is attempted after calling for :disconnect,
the connection will be restarted."
  (alexandria:with-gensyms (c cfn)
    `(let ((,c nil)
           (,cfn (lambda () ,@connection-body)))
       (defun ,symbol (query-or-op &optional (result-p t))
         "Controls a smart database connection object.  Call this function with
a query whenever you want to run queries against that database.
However, unless the function is actually called, no connections to the
database are attempted.

Lists of queries are also supported.

If :info is passed to the function, the connection information is
shown.

If :disconnect is passed to the function, the connection is closed.
However, if another query is attempted after calling for :disconnect,
the connection will be restarted."
         (flet ((ensure-connection ()
                  (cond
                    ((null ,c)
                     (setf ,c (funcall ,cfn)))
                    ((not (clsql-sys:is-database-open ,c))
                     (setf ,c
                           (funcall ,cfn)))
                    (t nil))))
           (typecase query-or-op
             (null nil)
             (symbol
              (case query-or-op
                (:info
                 (list :connection ,c
                       :name (clsql:database-name ,c)
                       :type (clsql:database-type ,c)))
                (:disconnect
                 (clsql:disconnect :database ,c))))
             (t
              (ensure-connection)
              (query query-or-op
                     :result-p result-p
                     :database ,c))))))))
