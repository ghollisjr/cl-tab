(require 'cl-tab)
(in-package :tb)

(defun psql-example ()
  (clsql:connect '("localhost" "test" "test" "password")
                 :database-type :postgresql)
  ;; get table content:
  (query "select * from some_table;"))
