(require 'cl-tab)
(in-package :tb)

(defun example-union1 ()
  "Example: union (no duplicates)"
  (let ((t1 (make-table '((1 2)
                          (3 4))
                        :field-names '("x" "y")))
        (t2 (make-table '((3 4)
                          (5 6))
                        :field-names '("a" "b"))))
    (union (list t1 t2))))

(defun example-union2 ()
  "Example: union (with duplicates)"
  (let ((t1 (make-table '((1 2)
                          (3 4))
                        :field-names '("x" "y")))
        (t2 (make-table '((3 4)
                          (5 6))
                        :field-names '("a" "b"))))
    (union (list t1 t2) :all-p t)))
