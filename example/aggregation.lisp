(require 'cl-tab)
(in-package :tb)

(defun example-aggregate ()
  "Example aggregation: sum x grouped by y"
  (let ((table (make-table '((1 2) (3 4) (5 4)) :field-names '("X" "Y"))))
    (agg (with-agg (y (tlambda (y) y)) ; group by y
                   ((sumx (agg-sum)))
                   (list y (sumx))
                   ;; Could also do:
                   ;; (list y sumx)
                   ;; using the convenient symbol-macrolet
                   (x)
           (sumx x))
         table
         :field-names '("Y" "SUM(X)"))))
