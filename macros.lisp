(in-package :tb)

(defmacro dotable ((vars table &optional result) &body body)
  (alexandria:with-gensyms
      (tab table-width table-length
          row i data)
    `(let ((,tab ,table))
       (with-accessors ((,table-width table-width)
                        (,table-length table-length)
                        (,data data))
           ,tab
         (do* ((,i 0 (1+ ,i)))
              ((>= ,i ,table-length) ,result)
           (let ((,row (map 'list (lambda (d)
                                    (aref d ,i))
                            ,data)))
             (symbol-macrolet
                 ,(if (listp vars)
                      ;; Two usages:
                      ;; 1. vars is list of symbols:
                      (loop
                        for v in vars
                        for j from 0
                        collecting `(,v (aref ,row ,j)))
                      ;; 2. vars is a symbol, so row vector should be
                      ;; bound to vars
                      `((,vars ,row)))
               ,@body)))))))
