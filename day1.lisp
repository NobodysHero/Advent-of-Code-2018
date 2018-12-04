;;;; day1.lisp

(in-package #:advent-of-code-2018)


(defun day1 ()
  (let* ((changes (mapcar #'parse-integer (read-puzzlefile "input1.txt")))
         (total (reduce #'+ changes))
         (seen (make-hash-table :size 1024)) ;use hashtable as a set
         (first-repetition
           (loop
                                        ;loop continuously over the changes
             :for change :in (setf (cdr (last changes)) changes)
             :sum change :into freq
             :until (gethash freq seen) ;check the set
             :do (setf (gethash freq seen) t)
             :finally (return freq))))
    (format t "The resulting frequency is ~a.~%The first frequency reached twice is ~a.~%"
            total first-repetition)))
