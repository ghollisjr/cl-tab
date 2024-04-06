(require :cl-tab)
(in-package :tb)

(defun insertion-example ()
  (let ((table (make-table '(("hello" 1)
                             ("world" 2))
                           :field-names
                           '("message" "index"))))
    (insert! table
             (list "this is" 3)
             (list "more data" 4))
    table))

(defun insertion-example2 ()
  (let ((table (make-table '(("hello" 1)
                             ("world" 2))
                           :field-names
                           '("message" "index"))))
    (insert! table
             ;; also can use plists
             (list :|message| "this is" :|index| 3)
             (list :|message| "more data" :|index| 4))
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
    (delete! table 1) ; -> ("world" 2) is gone
    ;; or a list of rows by indices
    (delete! table (list 2 3)) ; -> last 2 rows gone
    ;; or using a boolean function
    (delete! table
             (tlambda (|message| |index|)
               (> |index| 2))))) ; -> only ("hello" 1) remaining
