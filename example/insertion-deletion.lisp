(require :cl-tab)
(in-package :tb)

(defun insertion-example ()
  (let ((table (make-table '(("hello" 1)
                             ("world" 2))
                           :fields '("message" "index"))))
    (table-insert! table
                   (list "this is" 3)
                   (list "more data" 4))
    table))
