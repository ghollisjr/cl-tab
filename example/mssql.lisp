(require :cl-tab)
(in-package :tb)

(defun example1 ()
  "Connect to DB, run query, iterate & print results"
  ;; NOTE: Change ODBC connection string to suit your liking
  (clsql:connect '("MS-SQL ODBC Connection" "SA" nil
                   :connection-string
                   "Driver={ODBC Driver 17 for SQL Server};Server=SOMESERVER.COM;Database=master;Trusted_Connection=No;Encrypt=No;uid=SOMEUSER;Pwd=SOMEPASSWORD;")
                 :database-type :odbc)

  ;; Actual example: tb:query is a substitute for clsql:query that
  ;; returns a table of results.  Should feel familiar for Python
  ;; Pandas users.
  ;;
  ;; Then the results can be processed in a loop via dotable,
  ;; analogous to dolist but with a list of bound symbols rather than
  ;; a single symbol.
  (dotable ((x m) ; bind "x" to x, bind "message" to m
            (query "select x, message from test")
            'example-result-value)
    (print (list x m))))
