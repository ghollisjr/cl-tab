(require :cl-tab)
(in-package :tb)

(defun example-write-csv ()
  "Example writing CSV to a string with Lisp-formatted data and
Unix line endings."
  (write-csv
   (make-table '(("hello with space" 1)
                 ("world"            2))
               :field-names (list "message" "x"))
   :newline (string #\newline)))

(defun example-read-csv ()
  "Example reading CSV from a string with Lisp-formatted data."
  (let ((csv-data (format nil "message,x~%\"\"\"hello, world!\"\"\",1.0d0~%")))
    (format t "Example: Read CSV data~%")
    (format t "Input CSV file:~%~c~%" #\page)
    (format t "~a~c~%" csv-data #\page)
    (read-csv csv-data)))
