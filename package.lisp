(defpackage #:cl-tab
  (:nicknames :tb)
  (:use
   :cl)
  (:shadow :union)
  (:export
   ;; control parameters
   :*field-case-convert*
   ;; database connections
   :database
   :define-database
   ;; tables
   :table
   :make-table
   :data
   :table-ref
   :tref ; short-hand for table-ref
   :table-length
   :tlength ; short-hand for table-length
   :table-width
   :twidth ; short-hand for table-width
   :field-names
   :table-map
   :tmap ; shorthand for table-map
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
   :top
   :aggregate
   :agg
   :agg-sum
   :agg-count
   :agg-log-sum
   :agg-mean
   :with-aggregation
   :tlambda
   :fields
   :field
   ;; CSV
   :write-csv
   :read-csv
   ;; SQL
   :query
   ;; misc
   :plist
   ))
