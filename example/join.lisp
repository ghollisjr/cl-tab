(require 'cl-tab)
(in-package :tb)

(defun example-join1 ()
  (let ((t1 (make-table '((1 2)
                          (3 4))
                        :field-names '("a" "b")))
        (t2 (make-table '((1 2)
                          (3 4))
                        :field-names '("c" "d")))
        (t3 (make-table '((1 2)
                          (3 4))
                        :field-names '("e" "f"))))
    (join t1
          (on t2 (tlambda (|b| |c|)
                   (< |b| |c|))
              :type :left)
          (on t3 (tlambda (|c| |e|)
                   ;; note: need to check for NIL fields after prior
                   ;; left-join above
                   ;;
                   ;; Another option in this case would be something
                   ;; like:
                   ;;
                   ;; (unless (member nil fields)
                   ;;   (= |c| |e|))
                   ;;
                   ;; to check for any missing fields due to prior
                   ;; failed join.  But, each table's content is
                   ;; different and can require different join
                   ;; conditions; the above would assume no fields
                   ;; should be NIL, whereas this does not.
                   (and |c| |e| (= |c| |e|)))
              :type :left))))

(defun example-join2 ()
  (let ((t1 (make-table '((1 2)
                          (3 4))
                        :field-names '("a" "b")))
        (t2 (make-table '((1 2)
                          (3 4))
                        :field-names '("c" "d")))
        (t3 (make-table '((1 2)
                          (3 4))
                        :field-names '("e" "f"))))
    (join t1
          (on t2
              ;; equijoin on t1.b = t2.d
              (list (tlambda (b) b)
                    (tlambda (d) d))
              :type :left)
          (on t3
              ;; equijoin on t2.c = t3.e
              (list (tlambda (c) c)
                    (tlambda (e) e))
              :type :full))))

(defun example-join3 ()
  (let ((t1 (make-table '((1 2)
                          (3 4))
                        :field-names '("a" "b")))
        (t2 (make-table '((1 2)
                          (3 4))
                        :field-names '("c" "d"))))
    (join t1
          (on t2
              ;; equijoin on t1.a = t2.c and t1.b = t2.d
              (list (tlambda (|a| |b|)
                      (list |a| |b|))
                    (tlambda (|c| |d|)
                      (list |c| |d|)))
              :type :left))))

(defun loop-join-test ()
  (join (make-table '((1 2) (3 4)) :field-names '("X" "Y"))
        (on (make-table '((1 1) (2 2)) :field-names '("Y" "Z"))
            ;; note: for loop-join, field names are pre-converted, so
            ;; you accept field names as they are in the joined table.
            (tlambda (x .y)
              (equal x .y)))))

(defun hash-join-test ()
  (join (make-table '((1 2) (3 4)) :field-names '("X" "Y"))
        (on (make-table '((1 1) (2 2)) :field-names '("Y" "Z"))
            (list (tlambda (x) x)
                  ;; note: for hash-join, field names are not
                  ;; pre-converted, so you accept the field names as
                  ;; they are in the left and right tables pre-join.
                  (tlambda (y) y)))))
