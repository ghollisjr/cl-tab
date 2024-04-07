(require 'cl-tab)
(in-package :tb)

(defun example-aggregate ()
  "Example aggregation: sum x grouped by y"
  (let ((table (make-table '((1 2) (3 4) (5 4)) :field-names '("X" "Y"))))
    (agg (with-agg gy ((sumx (agg-sum gy)))
             (list gy (sumx))
             ;; Could also do:
             ;; (list gy sumx)
             ;; using the convenient symbol-macrolet
             (x)
           (sumx x))
         table
         ;; group by y
         :group-fn (tlambda (y)
                     y)
         :field-names '("Y" "SUM(X)"))))
