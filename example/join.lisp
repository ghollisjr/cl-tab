(require 'cl-tab)
(in-package :tb)

(defun example-join1 ()
  "Example: Join tables using indices"
  (let ((t1 (make-table '((1 "hello" 3)
                          (2 "world" 4))
                        :field-names '("pk" "m" "fk")))
        (t2 (make-table '((0 "another")
                          (1 "message")
                          (2 "to")
                          (3 "read")
                          (4 "quietly"))
                        :field-names '("pk" "v"))))
    (table-join (list t1 t2)
                '((0 0)
                  (1 1)
                  (0 1)
                  (1 1)
                  (nil 0)
                  (0 nil)))))

(defun example-join2 ()
  "Example: Join tables using condition and #'on function"
  (let ((t1 (make-table '((1 "hello" 3)
                          (2 "world" 4))
                        :field-names '("pk" "m" "fk")))
        (t2 (make-table '((0 "another")
                          (1 "message")
                          (2 "to")
                          (3 "read")
                          (4 "quietly"))
                        :field-names '("pk" "v"))))
    (table-join (list t1 t2)
                (on (lambda (row1 row2)
                      (destructuring-bind (pk1 m fk) row1
                        (destructuring-bind (pk2 v) row2
                          (if (equal fk pk2)
                              (list t t)
                              (list nil nil))))))
                :row-fn
                (lambda (row1 row2)
                  (destructuring-bind (pk1 m fk) row1
                    (destructuring-bind (pk2 v) row2
                      ;; we just want some of the data:
                      (list pk1 m pk2 v))))
                :field-names '("pk1" "m" "pk2" "v"))))
