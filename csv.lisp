;;;; CSV I/O
;;;;
;;;; General notes:
;;;;
;;;; * CSV I/O defaults to Lisp-friendly formatting.  This means you
;;;;   can naively save and load CSV files using Lisp code without any
;;;;   issues.  However, it also means that e.g. loading a CSV written
;;;;   using this library with a tool like Excel or Libreoffice Calc
;;;;   will have some blemishes, e.g. strings will include quotation
;;;;   marks.
;;;;
;;;; * To generate data more useful for foreign tools, use the
;;;;   :lisp-output-p argument and set it to NIL when running
;;;;   #'table-write-csv.  This will lead to default formatting for
;;;;   output as per the other arguments passed through to the cl-csv
;;;;   functions, which usually leads to useful interpretation by
;;;;   foreign tools.
;;;;
;;;; * Writing is low-memory use, but reading does lead to at least 2x
;;;;   memory use for the table that is loaded until GC is called.
;;;;   This could be improved in the future.
;;;;
;;;; * The functions here are ordinary functions since they rely on
;;;;   generic table operations.  This could be changed in the future.
(in-package :tb)

(defun table-write-csv (table
                        &key
                          stream
                          (lisp-output-p t)
                          (separator cl-csv:*separator*)
                          (field-names nil field-names-supplied-p)
                          (quote cl-csv:*quote*)
                          (escape cl-csv:*quote-escape*)
                          (always-quote cl-csv::*always-quote*)
                          (newline cl-csv::*write-newline*))
  "Writes table as CSV to output stream using delimiter and either the
field-names from the table or those provided as a keyword argument.
If :field-names NIL is used, then no field names will be written and
only data will be written.

stream uses same semantics as cl-csv:write-csv, so NIL means return string.

If lisp-output-p is T (default), then fields are written so that they
can be read back as Lisp data through #'read.  Otherwise, follows
cl-csv behavior for data.

quote and escape arguments are passed directly to
cl-csv:write-csv... functions, so they have the same usage."
  (let ((cl-csv-keyargs
          (list :quote quote
                :escape escape
                :always-quote always-quote
                :newline newline))
        (nil-stream (null stream))
        (stream
          (or stream
              (make-string-output-stream)))
        (field-names
          (cond
            ;; Three usages:
            ;; 1. Just use table's field names
            ((null field-names-supplied-p)
             (table-field-names table))
            ;; 2. Use custom field names that were supplied:
            (field-names field-names)
            ;; 3. :field-names nil was supplied, don't write field names
            ((null field-names) nil))))
    (when field-names
      (apply #'cl-csv:write-csv-row
             (table-field-names table)
             :stream stream
             :separator separator
             cl-csv-keyargs))
    (dotable (row table
                  (when nil-stream
                    (get-output-stream-string stream)))
      (apply #'cl-csv:write-csv-row
             (map 'list
                  ;; Look Ma, functional programming
                  (if lisp-output-p
                      (lambda (f)
                        (format nil "~s" f))
                      #'identity)
                  row)
             :stream stream
             :separator separator
             cl-csv-keyargs))))

(defun table-read-csv (stream-or-string
                       &key
                         (lisp-input-p t)
                         (field-names nil field-names-supplied-p)
                         csv-reader row-fn map-fn data-map-fn sample skip-first-p
                         (separator cl-csv:*separator*)
                         (quote cl-csv:*quote*)
                         (escape cl-csv:*quote-escape*)
                         (unquoted-empty-string-is-nil cl-csv::*unquoted-empty-string-is-nil*)
                         (quoted-empty-string-is-nil cl-csv::*quoted-empty-string-is-nil*)
                         (trim-outer-whitespace cl-csv::*trim-outer-whitespace*)
                         (newline cl-csv::*read-newline*)
                         (escape-mode cl-csv::*escape-mode*))
  "Read CSV file into a table from stream-or-string using cl-csv read
functions.

If lisp-input-p is T (default), then fields are assumed to be written
in a format that can be read by #'read.  Otherwise, follows cl-csv
behavior."
  (let ((cl-csv-keyargs
          (list :CSV-READER CSV-READER
                :ROW-FN ROW-FN
                :MAP-FN MAP-FN
                :DATA-MAP-FN DATA-MAP-FN
                :SAMPLE SAMPLE
                :SKIP-FIRST-P SKIP-FIRST-P
                :SEPARATOR SEPARATOR
                :QUOTE QUOTE
                :ESCAPE ESCAPE
                :UNQUOTED-EMPTY-STRING-IS-NIL UNQUOTED-EMPTY-STRING-IS-NIL
                :QUOTED-EMPTY-STRING-IS-NIL QUOTED-EMPTY-STRING-IS-NIL
                :TRIM-OUTER-WHITESPACE TRIM-OUTER-WHITESPACE
                :NEWLINE NEWLINE
                :ESCAPE-MODE ESCAPE-MODE)))
    (destructuring-bind (read-field-names &rest data)
        (apply #'cl-csv:read-csv
               stream-or-string
               cl-csv-keyargs)
      (make-table (if lisp-input-p
                      (mapcar (lambda (row)
                                (mapcar (lambda (field)
                                          (read-from-string field nil nil))
                                        row))
                              data)
                      data)
                  :field-names
                  (cond
                    ((null field-names-supplied-p)
                     read-field-names)
                    (field-names field-names)
                    ((null field-names) nil))))))
