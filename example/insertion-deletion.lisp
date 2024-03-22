(require :cl-tab)
(in-package :tb)

(defun insertion-example ()
  (let ((table (make-table '(("hello" 1)
                             ("world" 2))
                           :field-names
                           '("message" "index"))))
    (table-insert! table
                   (list "this is" 3)
                   (list "more data" 4))
    table))

(defun deletion-example ()
  (let ((table (make-table '(("hello"     1)
                             ("world"     2)
                             ("this is"   3)
                             ("more data" 4)
                             ("to ponder" 5))
                           :field-names
                           '("message" "index"))))
    ;; can delete a specific row by index
    (table-delete! table 1) ; -> ("world" 2) is gone
    ;; or a list of rows by indices
    (table-delete! table (list 2 3)) ; -> last 2 rows gone
    ;; or using a boolean function
    (table-delete! table
                   (lambda (message index)
                     (> index 2))))) ; -> only ("hello" 1) remaining
