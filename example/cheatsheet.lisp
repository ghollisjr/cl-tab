;;;; I thought it best to provide a cheat-sheet for those who are
;;;; familiar with SQL and want to know how to do what they already do
;;;; in SQL but using this Common Lisp library.

(in-package :tb)

;;; Selections: Without `where`, this is just a table object or a
;;; conversion of a table to some other data type:

;; select * from tab
tab ; as a table
(table->plist tab) ; as a plist
(table->list tab)  ; as a list (WARNING: field names are not included)
(table->array tab) ; as a 2-d array (WARNING: field names are not included)

;; You can also select from a table to a list, e.g.:
(tmap (tlambda (x) x) tab :type 'list) ; => (X1 X2 ...)

;;; Selections with `where`: This needs #'filter

;; select * from tab where X > 0
(filter (tlambda (x) (> x 0)) tab) ; returns table, see above for type conversions

;;; Selections with `group by`: This is aggregation.  Aggregation
;;; results are also tables and can be converted to different types as
;;; above.
;;;
;;; The with-aggregation (with-agg shorthand) macro returns a function
;;; that is expected by #'aggregate (agg shorthand).  As part of
;;; aggregation, specific aggregation closures are needed.  Various
;;; aggregation closures are already provided, e.g. #'agg-sum.  These
;;; can be used in conjunction with #'agg and with-agg to perform
;;; grouped and non-grouped aggregations.  Example grouped
;;; aggregation:

;; select x as "x", sum(y) as "sum(y)" from tab group by x
(agg (with-agg gx ; gx is bound to grouped value, x in this case
         ((ysum (agg-sum))) ; agg bindings
         (list :|x| gx ; gx is bound to grouped value, x in this case
               :|sum(y)| ysum) ; ysum from agg binding above, can also be (ysum)
         (|y|) ; we want field y from each row
       (ysum |y|)) ; now we sum y from each row
     tab
     :group-fn (tlambda (|x|) |x|)) ; group by x

;;; Aggregations without group-by: Simpler form of aggregation

;; select sum(y) as "sum(y)" from tab
(agg (with-agg () ; no need to group, singleton group
         ((ysum (agg-sum))) ; same agg binding as above
         (list :|sum(y)| ysum) ; ysum from agg binding above
         (|y|) ; we want field y from each row
       (ysum |y|)) ; now we sum it each row
     tab)

;;; Selections with different fields in result: This needs tmap

;; select x as "x", y as "y", x+y as "z" from tab
(tmap (tlambda (|x| |y|)
        (list :|x| |x|
              :|y| |y|
              :|z| (+ |x| |y|)))
      tab)
;; can also do this without copying all fields manually via fields & (field ...)
(tmap (tlambda ()
        (append fields
                (list :|z| (+ (field |x|) (field |y|)))))
      tab)

;; select x as "x" from tab -- example of ignoring columns
(tmap (tlambda (|x|) (list :|x| |x|))
      tab)

;;; Joins: There are two different joins supported, and they are
;;; specified using different arguments to the #'on function which
;;; produces join specifications for the #'join function.  The two
;;; join types are
;;;
;;; loop join:     General join.  Requires nested loop over tables.
;;; hash equijoin: Equivalence join using hashed keys.
;;;
;;; (Merge equijoin might be added in future, but not a priority.)

;; select * from a
;; inner join b
;;   on a.x = b.x

;; As a loop join:
(join a
      (on b
          ;; Notice |.x| for second x field: This is a fundamental
          ;; part of how field names are handled.  Field names must be
          ;; unique, so they are automatically prepended with a '.'
          ;; character to fields from the right table in a join if
          ;; they collide with a field name in a left table.
          (tlambda (|x| |.x|)
            (= |x| |.x|))
          :type :inner)) ; :inner is default type

;; As a hash equijoin:
(join a
      (on b
          ;; Instead of a single function, a list of two functions
          ;; which each return a key to be hashed and compared is
          ;; supplied as the join condition.
          ;;
          ;;
          ;; Notice no need for '.' prefix in either function this
          ;; time, as functions are applied to pre-joined table rows,
          ;; not post-joined table.
          (list (tlambda (|x|) |x|)
                (tlambda (|x|) |x|))
          :type :inner))

;; More complex joins are handled by adding more #'on statements and
;; specifing different types, e.g.:

;; select * from a
;; left join b
;;   on a.x = b.y
;; full join c
;;   on b.z > c.z
(join a
      (on b
          (list (tlambda (|x|) |x|)
                (tlambda (|y|) |y|))
          :type :left)
      (on c
          (tlambda (|z| |.z|) ; z from b, .z from c
            (> |z| |.z|))
          :type :full))

;;; Unions: Unions are accomplished via #'union.  tb:union shadows
;;; cl:union, so be aware if you use the :cl-tab or :tb package.
;;; Exclusive and inclusive unions are supported (e.g. `union` and
;;; `union all` in SQL).

;; Example:
;; select * from a
;; union
;; select * from b
(union (list a b))

;; Example:
;; select * from a
;; union all
;; select * from b
(union (list a b) :all-p t)

;;; select distinct...: This is supported via #'distinct.
;; Example:
;; select distinct * from tab
(distinct tab)

;; Example:
;; select distinct x, y from tab
(distinct tab
          :row-fn
          (tlambda (|x| |y|)
            (list :|x| |x|
                  :|y| |y|)))

;;; Except: The `except` or table/set difference operation is
;;; supported by #'table-difference (shorthand #'tdiff).
;; Example:
;; select * from a
;; except
;; select * from b
(tdiff a b)

;;; Since symmetric differences are commonly used, these are supported
;;; directly in #'tdiff.
;; Example: Symmetric difference between tables
;; (select * from a except select * from b)
;; union all
;; (select * from b except select * from a)
(tdiff a b :symmetric-p t) ; note: duplicates are preserved

;;; Here is a more complex example of generating queries, executing,
;;; and collecting the results into a table using this library.

;; Example: We want distinct counts of values from all columns in a
;; table, and we don't mind querying table once per column.  Then we
;; want all of the results unioned into a single table along with
;; table_name and column_name values.
;;
;; In SQL, we might do something like:
;; 1. Create a table to store count results.
;; 2. Create a cursor based on selecting (table_name,column_name) from information_schema.columns
;; 3. Generate queries to insert distinct column value counts into the table & execute the queries inside of a cursor loop.
;;
;; Using this library, we would do the following to get distinct value
;; counts for all columns in the Chinook customer datbase using a
;; PostgreSQL connection:
(union
 ;; assumes connection already made
 (query
  (tmap (tlambda (|table_name| |column_name|)
          (concatenate 'string
                       "
select '"|table_name|"' as \"table_name\",
       '"|column_name|"' as \"column_name\",
       count(distinct "|column_name|") as \"count_distinct\"
from "|table_name|))
        ;; assumes connection already made
        (query "
select table_name, column_name
from information_schema.columns
  where table_name like 'customer'
    and table_schema like 'public'")
        :type 'list))
 :all-p t) ; no dups 'cause good query
