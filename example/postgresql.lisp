(require 'cl-tab)
(in-package :tb)

(defun example-psql ()
  "Read table from PostgreSQL"
  (clsql:connect '("localhost" "some_database" "user" "password")
                 :database-type :postgresql)
  ;; get table content:
  (query "select * from some_table;"))

(defun example-psql2 ()
  "Write table to PostgreSQL"
  (clsql:connect '("localhost" "some_database" "user" "password")
                 :database-type :postgresql)
  ;; send table content to PostgreSQL server in a table:
  (map nil
       #'clsql:execute-command
       (table->sql
        (make-table '((1 2)
                      (3 4))
                    :field-names '("x" "y"))
        "some_database.public.test"
        :batch-size 10))
  (clsql:disconnect))
