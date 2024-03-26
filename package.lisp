(defpackage #:cl-tab
  (:nicknames :tb)
  (:use
   :cl)
  (:shadow :union)
  (:export
   :key-join
   :make-table
   :data
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
   :union
   :on
   :aggregate
   :agg
   :agg-sum
   :agg-count
   :agg-log-sum
   :agg-mean
   :tlet
   ;; CSV
   :write-csv
   :read-csv
   ;; SQL
   :query))
