(defpackage #:cl-tab
  (:nicknames :tb)
  (:use
   :cl)
  (:export
   :key-join
   :make-table
   :table-ref
   :table-length
   :table-fields
   :table-map
   :dotable
   :table-add-field!
   :table-del-field!
   ;; CSV
   :table-write-csv
   :table-read-csv
   ;; SQL
   :query))
