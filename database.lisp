(in-package :tb)

(defstruct database
  server
  name
  connection-spec
  connection)

(defmacro define-database (symbol
                           properties
                           &body connection-body)
  "Defines a new function bound to symbol which controls a smart database
connection object.  The idea is to define a database connection, and
then call that function with a query whenever you want to run queries
against that database.  However, unless the function is actually
called, no connections to the database are attempted.

properties is a let-like symbol-binding list for properties about the
database.  Standard properties are :url, :name, :connection-string.

Lists of queries are also supported.

If :info is passed to the function, the connection information is
shown.

If :disconnect is passed to the function, the connection is closed.
However, if another query is attempted after calling for :disconnect,
the connection will be restarted."
  (alexandria:with-gensyms (c cfn)
    `(let ((,c nil)
           (,cfn (lambda () ,@connection-body)))
       ,@(loop
           for prop in properties
           collect `(setf (get ',symbol ',(first prop)) ,(second prop)))
       (defun ,symbol (query-or-op &optional (result-p t))
         "Controls a smart database connection object.  Call this function with
a query whenever you want to run queries against that database.
However, unless the function is actually called, no connections to the
database are attempted.

Lists of queries are also supported.

If :conn is passed to the function, the connection is made if needed
and the connection object is returned.

If :info is passed to the function, the connection information is
returned as a plist.

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
                (:conn
                 (ensure-connection)
                 ,c)
                (:info
                 (list :connection ,c
                       :name (when ,c (clsql:database-name ,c))
                       :type (when ,c (clsql:database-type ,c))))
                (:disconnect
                 (when ,c
                   (clsql:disconnect :database ,c)
                   (setf ,c nil)))))
             (t
              (ensure-connection)
              (query query-or-op
                     :result-p result-p
                     :database ,c))))))))
