(in-package :tb)

;; Example database: local PostgreSQL server's Chinook database
;;
;; define-database defines a function that accepts a query/operation
;; and takes action acocrdingly.  Argument behavior:
;;
;; * Query string => Returns table result if result-p is T
;; * List of query strings => Returns list of table results if result-p is T
;; * :disconnect => Disconnects database
;;
;; The connection is automatically activated if it is not available
;; and a query or list of queries is supplied.
(define-database psql
  (clsql:connect '("localhost" "chinook" "user" "password")
                 :database-type :postgresql))

(defun example-database1 ()
  ;; chinook table names
  (tref (psql "
select * from INFORMATION_SCHEMA.TABLES
 where TABLE_SCHEMA like 'public'")
        "table_name"))

(defun example-database2 ()
  (join (psql "select * from customer")
        (on (psql "select * from invoice")
            ;; hash equijoin on addresses (yes, customer_id is better,
            ;; but I was curious how realistic chinook's data is, as
            ;; real data will have spurious entries)
            (list (tlambda ()
                    (list (field |address|)
                          (field |city|)
                          (field |state|)
                          (field |country|)
                          (field |postal_code|)))
                  (tlambda ()
                    (list (field |billing_address|)
                          (field |billing_city|)
                          (field |billing_state|)
                          (field |billing_country|)
                          (field |billing_postal_code|))))
            :type :full)))
