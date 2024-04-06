(require 'cl-tab)
(in-package :tb)

;;;; Examples to show the use of *field-name-case-convert*
;;;;
;;;; There are currently 3 values for *field-name-case-convert*:
;;;;
;;;; * NIL     => do not modify case.  Symbols & strings match
;;;;              and are case-sensitive.
;;;;
;;;; * T       => Up-case strings & symbols.  This is convenient 
;;;;              for the usual settings in the Lisp reader.
;;;;
;;;; * :invert => Down-case strings & up-case symbols.  This is
;;;;              similar to the inverted case setting in the
;;;;              Lisp reader, and I happen to like the style.

(defun default-case-example ()
  (let ((*field-name-case-convert* nil))
    (tmap (tlambda (|x| |y|)
            (list :|x| (+ |x| 2)
                  :|y| |y|))
          (make-table '((1 2) (3 4)) :field-names '("x" "y")))))

(defun up-case-example ()
  ;; The lower case ("x" "y") field names will be automatically
  ;; up-cased when creating the table, and can thus be referenced with
  ;; :x & :y keyword symbols in a plist.
  (let ((*field-name-case-convert* t))
    (tmap (tlambda (x y)
            (list :x (+ x 2)
                  :y y))
          (make-table '((1 2) (3 4)) :field-names '("x" "y")))))

(defun invert-case-example ()
  ;; The lower case ("x" "y") field names will be retained, but can be
  ;; referenced with :x & :y keyword symbols in a plist since the
  ;; symbols are up-cased.
  (let ((*field-name-case-convert* :invert))
    (tmap (tlambda (x y)
            (list :x (+ x 2)
                  :y y))
          (make-table '((1 2) (3 4)) :field-names '("x" "y")))))
