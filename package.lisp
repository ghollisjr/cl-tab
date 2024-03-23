(defpackage #:cl-tab
  (:nicknames :tb)
  (:use
   :cl)
  (:export
   :key-join
   :make-table
   :table-ref
   :table-length
   :table-field-names
   :table-map
   :table->list
   :table->plist
   :table->array
   :dotable
   :table-add-field!
   :table-del-field!
   :table-join
   :on
   :table-aggregate
   :agg
   :agg-sum
   :agg-count
   :agg-log-sum
   :agg-mean
   ;; CSV
   :table-write-csv
   :table-read-csv
   ;; SQL
   :query))
