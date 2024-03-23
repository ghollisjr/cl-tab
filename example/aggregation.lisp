(require 'cl-tab)
(in-package :tb)

(defun example-aggregate ()
  "Example aggregation: sum x grouped by y"
  (let ((table (make-table '((1 2) (3 4) (5 4)) :field-names '("x" "y"))))
    (aggregate (with-aggregation gy ((sumx (agg-sum gy)))
                   (&optional x y)
                   (list gy (sumx))
                 (declare (ignore y))
                 (sumx x))
               table
               ;; group by y
               :group-fn (lambda (&rest row)
                           (second row))
               :field-names '("y" "sum(x)"))))
