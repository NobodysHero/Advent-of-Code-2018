;;;; day20.lisp

(in-package :advent-of-code-2018)

(defun day20-parse-file (&optional (file (puzzlefile 20)))
  (with-open-file (in file)
    (labels ((parse-group ()
               (loop
                 :with result := nil
                 :with path := nil
                 :for char := (read-char in)
                 :do (case char
                       ((#\N #\S #\W #\E) (push char path))
                       (#\(
                        (push (coerce (nreverse path) 'string) result)
                        (setf path nil)
                        (push (list (parse-group)) result))
                       ((#\) #\$)
                        (push (coerce (nreverse path) 'string) result)
                        (return (nreverse result)))
                       (#\|
                        (push (coerce (nreverse path) 'string) result)
                        (setf path nil)
                        (setf result (list (nreverse result))))))))
      (read-char in)
      (parse-group))))

(defun print-paths (node &optional (acc nil))
  (cond
    ((stringp node)
     (mapcar (lambda (prior) (format nil "~a-~a~%" prior node))
             acc))
    ((null node)
     acc)
    ((stringp (first node))
     (let ((new-prior (format nil "~a-~a" prior (first node))))
       )
     (print-paths (rest node) ))
    ((listp (first node))
     (loop
       :for obj :in (first node)
       :do (print-paths (rest node) (print-paths obj prior nil) out)))))
