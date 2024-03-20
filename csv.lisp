;;;; CSV I/O
(in-package :tb)

(defun table-write-csv (table
                        &key
                          stream
                          (separator ",")
                          (field-names nil field-names-supplied-p))
  "Writes table as CSV to output stream using delimiter and either the
field-names from the table or those provided as a keyword argument.
If :field-names NIL is used, then no field names will be written and
only data will be written.

stream uses same semantics as cl-csv:write-csv, so NIL means return string."
  (dotable (row table)
    (cl-csv:write-csv-row (coerce row 'list)
                          :stream stream
                          :separator separator
