;;;; day1.lisp

(in-package #:advent-of-code-2018)

(defun day1 ()
  (let* ((changes (mapcar #'parse-integer (read-puzzlefile "input1.txt")))
         (total (reduce #'+ changes)) ;answer to the first part
         (seen (make-hash-table :size 1024)) ;use hashtable as a set
         (first-repetition
           (loop
             :for change :in (setf (cdr (last changes)) changes) ;loop continuously over the changes
             :sum change :into freq
             :when (gethash freq seen) :return freq ;check the set
             :do (setf (gethash freq seen) t))))
    (format t "The resulting frequency is ~a.~%The first frequency reached twice is ~a.~%"
            total first-repetition)))
