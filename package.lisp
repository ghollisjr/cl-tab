(defpackage #:cl-tab
  (:nicknames :tb)
  (:use
   :cl)
  (:shadow :union)
  (:export
   ;; control parameters
   :*field-case-convert*
   :*nil-order*
   ;; database connections
   :database
   :define-database
   ;; tables
   :table
   :make-table
   :tab
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
   :table->sql
   :add-field!
   :del-field!
   :insert!
   :delete!
   :update!
   :join
   :on
   :on-keys
   :union
   :table-difference
   :tdiff
   :distinct
   :top
   :aggregate
   :agg
   :agg-function
   :agg-min
   :agg-max
   :agg-sum
   :agg-count
   :agg-log-sum
   :agg-mean
   :with-aggregation
   :with-agg
   :hist-agg
   :table-sort!
   :tsort!
   :order
   :asc
   :desc
   :tlambda
   :tlambda*
   :fields
   :field
   :field-plist
   ;; CSV
   :write-csv
   :read-csv
   ;; org-mode
   :table->org
   ;; SQL
   :query
   ;; misc
   :plist))
