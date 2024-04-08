(in-package :tb)

;;;; Miscellaneous tools

(defun %center-pad (string width)
  "Pads a string into a given width with space characters"
  (let ((gap (-  width (length string))))
    (unless (plusp gap) ; bad args
      (return-from %center-pad string))
    (let* ((lgap (floor gap 2))
           (rgap (- gap lgap)))
      (concatenate 'string
                   (make-string lgap :initial-element #\Space)
                   string
                   (make-string rgap :initial-element #\Space)))))

;; Print an org-mode table:
(defun table->org (table &optional stream)
  "Prints a table in org-mode syntax to stream.  If stream is nil,
returns a string."
  (let (null-stream)
    (unless stream
      (setf null-stream t))
    (when null-stream
      (setf stream (make-string-output-stream)))
    (let ((widths
            (mapcar (lambda (name col)
                      (+ 2
                         (reduce #'max
                                 (map 'list
                                      (lambda (c)
                                        (length (format nil "~a" c)))
                                      col)
                                 :initial-value (length name))))
                    (field-names table)
                    (data table))))
      ;; header
      (format stream
              "|")
      (loop
        for n in (field-names table)
        for w in widths
        do (format stream "~a|"
                   (%center-pad n w)))
      (terpri stream)
      ;; header-data divider line
      (princ #\| stream)
      (loop
        for i from 0
        for w in widths
        do
           (unless (zerop i)
             (princ #\+ stream))
           (princ (make-string w :initial-element #\-) stream))
      (princ #\| stream)
      (terpri stream)
      ;; data
      (dotimes (i (tlength table))
        (unless (zerop i) (terpri stream))
        (let ((row (tref table i :type 'list)))
          (format stream "|")
          (loop
            for c in row
            for w in widths
            do (format stream "~a|"
                       (%center-pad (format nil "~a" c)
                                   w))))))
    (when null-stream
      (prog1 (get-output-stream-string stream)
        (close stream)))))
