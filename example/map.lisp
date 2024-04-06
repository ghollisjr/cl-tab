(require 'cl-tab)
(in-package :tb)

(defun example-map ()
  (tmap (tlambda (x) ; can mention select fields you want by namex
          (setf (field y) 15) ; fields can be set
          (append (list :new (* 2 x))
                  fields)) ; also all fields returned
        (make-table '((1 2) (3 4))
                    :field-names '("X" "Y"))))

(defun example-map2 ()
  ;; This example shows an imperative/stateful way of using tmap to
  ;; manipulate a row of data.  Notice that the (field ...) macro and
  ;; fields symbol macro allow setf.  The value returned by the
  ;; mapping function is taken as the new row data for the new table,
  ;; and field names are automatically gleaned from the plist if
  ;; plists are returned.
  (tmap (tlambda ()
          ;; double X
          (setf (field x)
                (* (field x) 2))
          ;; rename X to Z
          (subst :z :x fields))
        (make-table '((1 2) (3 4))
                    :field-names '("X" "Y"))))
