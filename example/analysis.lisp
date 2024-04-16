;;;; This is an example analysis where a Chinook database on a local
;;;; PostgreSQL server is analyzed using tools from both cl-tab and
;;;; cl-ana.
(require 'cl-tab)
(require 'cl-ana)

(defpackage #:ana
  (:use :cl))
(in-package :ana)
(cl-ana.package-utils:use-package-group :cl-ana)
(shadowing-use-package :tb)

;; Convenience function:
(setf (symbol-function 'regex)
      #'ppcre:all-matches-as-strings)

;; Setup cl-ana project
(defproject chinook "chinook-analysis"
  (list #'macrotrans #'branchtrans #'progresstrans)
  (fixed-cache 5))

;; Define server connection
(define-database psql
  (clsql:connect '("localhost" "chinook" "user" "password")
                 :database-type :postgresql))

;;; At the start of any project, there is exploration, so here are
;;; examples of things I might do.
(defun chinook-tables ()
  (psql "select * from information_schema.tables where table_schema like 'public'"))

(defun chinook-columns ()
  (psql "select * from information_schema.columns where table_schema like 'public'"))

(defun chinook-column-hist ()
  "histogram of columns.  counts number of tables with given column
name. results ordered most common to least common column.  might
suggest potential joins."
  (tsort! (agg (hist-agg |column_name| |data_type|)
               (chinook-columns))
          (order (list (field |count|)
                       :desc))))
;; result: lots of names (likely different types) & update info, then
;; music info

(defun track-tables ()
  "tables that have some kind of track column"
  (tmap (field |table_name|)
        (filter (tlambda (|column_name|)
                  (regex ".*track.*" |column_name|))
                (chinook-columns))))

(defun price-tables ()
  "tables that have some kind of price column"
  (tmap (field |table_name|)
        (filter (tlambda (|column_name|)
                  (regex ".*price.*" |column_name|))
                (chinook-columns))))

;;; Now for some analysis
(defres (track length)
  (tref (psql "select count(*) from track")
        0 :type 'list))

(defres (composer distinct-count)
  (car
   (tref (psql "select count(distinct composer) from track")
         0 :type 'list)))

(defres (composer hist)
  ;; histogram of composers in track list, i.e. number of tracks per
  ;; composer.
  (agg (hist-agg |composer|)
       (psql "select composer from track")))

;; Question: How much money does each composer make?
;;
;; Needed tables:
;; invoice_line: track_id, unit_price, quantity
;; track: track_id, composer

(defres (composer earnings)
  (if nil
      ;; For comparison, SQL version:
      (psql "
select composer,
cast(sum(cast(invoice_line.unit_price as money)*quantity) as float) as \"earnings\"
from track
join invoice_line
  on track.track_id = invoice_line.track_id
group by composer
order by \"earnings\" desc")
      (tsort!
       (agg (with-agg (composer (field |composer|))
                      ((earnings (agg-sum)))
                      (append composer
                              (list :|earnings| earnings))
                      (|unit_price| |quantity|)
              (earnings (* (read-from-string |unit_price|)
                           |quantity|)))
            (join (psql "select track_id, composer from track")
                  (on-keys
                   (psql "select track_id, unit_price, quantity from invoice_line")
                   |track_id|)))
       (order (list (field |earnings|) :desc)))))
