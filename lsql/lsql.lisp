;;;; This is jibberish that I'm using to prototype a new Lisp-SQL
;;;; language.  Let me show you its features!
;;;;
;;;; - Expression-based language: Everything is an expression,
;;;;   including objects.  Every expression has a value.
;;;;
;;;;   Not true for SQL.  E.g.:
;;;;
;;;;   select * from tab;
;;;;
;;;;   returns a table from a table. But:
;;;;
;;;;   tab;
;;;;
;;;;   returns an error since it is not a valid SQL query.
;;;;
;;;; - lsql is closed: Every lsql object can be an argument to another
;;;;   lsql expression.
;;;;
;;;;   Not true for SQL.  E.g., given t1(x,y) and t2(x,z):
;;;;
;;;;   select * from t1 join t2 on t1.x=t2.x
;;;;
;;;;   returns a table with two columns named "x".  This will be
;;;;   returned to you as a "table" from the SQL client, but if you
;;;;   try to use this in a CTE or insert the values into a table, the
;;;;   SQL server will signal an error since columns must have
;;;;   different names.
;;;;
;;;; - Multi-server: lsql expressions can refer to data located on any
;;;;   system which has been defined, including the host system.  lsql
;;;;   expressions can be compiled to execute as SQL queries on SQL
;;;;   servers or as Lisp code executed on the host system.

(ql:quickload :cl-tab)

(defpackage lsql
  (:use :cl))

(in-package :lsql)

(defmacro defsys (&rest args) nil)

(defsys build1
    (odbc "build1"
          "QcDidSwDb001.ucsfmedicalcenter.org"
          nil
          :connection-string
          "Driver={ODBC Driver 17 for SQL Server};Server=QcDidSwDb001.ucsfmedicalcenter.org;Database=home_gary;Trusted_Connection=Yes;"
          :encoding :latin-1)
  (*system* :ucsf)
  (*db-conn* #'build1)
  (clsql:*default-database* (funcall *db-conn* :conn))
  (uffi:*default-foreign-encoding* :latin-1)
  (*src-db-name* "CDW_SRC")
  (*stage-db-name* "CDW_DEID_STAGE")
  (*map-db-name* "CDW_DEID_MAP")
  (*new-db-name* "CDW_NEW"))

(defclass field ()
  ((source
    :initform nil
    :initarg :source
    :accessor field-source
    :documentation "field source (table, object etc.)")
   (symbol
    :initform nil
    :initarg :symbol
    :accessor field-symbol
    :documentation "field symbol (name)")))

(defun field (source symbol)
  (setf symbol
        (typecase symbol
          (symbol symbol)
          (string (intern symbol))))
  (make-instance 'field
                 :source source
                 :symbol symbol))

(defmethod print-object ((f field) stream)
  (format stream "@~s(~s)"
          (field-source f)
          (field-symbol f)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun field-reader (stream char)
    (declare (ignore char))
    (let ((source (read stream))
          (symbols (read stream t nil t)))
      (if (cdr symbols)
          `(list ,@(mapcar (lambda (sym)
                             (typecase sym
                               (null nil)
                               (symbol `(field ,source ',sym))
                               (string `(field ,source ',(intern sym)))
                               (t (error "Field symbol must be NIL, symbol, or string"))))
                           symbols))
          (let ((sym (first symbols)))
            (typecase sym
              (null nil)
              (symbol `(field ,source ',sym))
              (string `(field ,source ',(intern sym)))
              (t (error "Field symbol must be NIL, symbol, or string"))))))))

(set-macro-character #\@ #'field-reader)

(defmacro sel (fields
               &optional
                 from
                 where)
  "Just a stub for now"
  `'(sel ,fields ,from ,where))

;; example for a join query
(defun join-example (a b)
  "
select a.x, a.y as \"y\",
       b.y as \".y\", b.z
from a
left join b
  on a.y = b.y
"
  (sel (@a(x) @a(y) @b(y) @b(z)) ; sel shorthand for select
       (join a
             (to b ; to instead of on for grammatical reasons
                 (= @a(y) @b(y))
                 :left))))

#| The @tab(fields...) syntax is my proposed solution to the
table-field dereference problem.  Examples:

@tab(f1 f2)          => tab.f1, tab.f2
@tab((f1 field1) f2) => tab.f1 as field1, tab.f2

In lsql, fields are guaranteed to be unique in any given table, and
since all expressions that return tables must obey this guarantee, the
result is that sel must enforce field renaming if a collision occurs.

In the above example, none of the fields collide.  However, if they do
collide then the automatic #\. prefix rule applies, e.g.:

(sel (@a(x y) @b(y z)) tab) =>
select a.x as x,
a.y as y,
b.y as ".y",
b.z as z
from tab

where the names are explicitly stated in the SQL query for clarity.

Any field with a nickname that is incompatible with the collision rule
will signal an error.

The system works by generating the query at runtime.  There will be a
function which reads the query object and interprets the object
accordingly.  This is necessary because the computations can involve
both local and remote systems.  The computation specifications must be
examined to determine the necessary resources.  If a computation can
be evaluated locally, a function can be compiled and then executed.
If a computation must be executed remotely, then it will be compiled
into SQL queries which will be sent to the servers as needed along
with necessary data transfer operations.

|#

(defun tab-example ()
  (let ((tab (tb:table '((:x 1 :y 2)))))
    @tab(x)))
