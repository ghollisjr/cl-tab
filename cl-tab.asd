(asdf:defsystem #:cl-tab
  :serial t
  :author "Gary Hollis"
  :description "Common Lisp tabular data analysis library & DSL"
  :license "GPLv3"
  :depends-on (:alexandria
               :clsql
               :cl-csv
               :cl-ana)
  :components ((:file "package")
               (:file "macros")
               (:file "database")
               (:file "tab")
               (:file "csv")
               (:file "sql")
               (:file "misc")))
