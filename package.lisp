(defpackage #:cl-tab
  (:nicknames :tb)
  (:use
   :cl)
  (:shadow :union)
  (:export
   :table
   :make-table
   :data
   :table-ref
   :table-length
   :table-width
   :field-names
   :table-map
   :filter
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
   :union
   :distinct
   :aggregate
   :agg
   :agg-sum
   :agg-count
   :agg-log-sum
   :agg-mean
   :with-aggregation
   :tlet
   ;; CSV
   :write-csv
   :read-csv
   ;; SQL
   :query))
