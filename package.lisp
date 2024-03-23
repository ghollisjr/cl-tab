(defpackage #:cl-tab
  (:nicknames :tb)
  (:use
   :cl)
  (:export
   :key-join
   :make-table
   :table-ref
   :table-length
   :table-width
   :field-names
   :table-map
   :table->list
   :table->plist
   :table->array
   :dotable
   :add-field!
   :del-field!
   :insert!
   :delete!
   :join
   :on
   :aggregate
   :agg
   :agg-sum
   :agg-count
   :agg-log-sum
   :agg-mean
   ;; CSV
   :write-csv
   :read-csv
   ;; SQL
   :query))
