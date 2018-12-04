;;;; day3.lisp

(in-package #:advent-of-code-2018)


(defun day3-parse-line (line)
  ;; example "#1 @ 896,863: 29x19"
  (ppcre:register-groups-bind ((#'parse-integer id xpos ypos width height))
      ("#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)" line)
    (mapcan #'list '(:id :xpos :ypos :width :height) (list id xpos ypos width height))))

(defun make-claim (claim board)
  (loop
    :for x :from (getf claim :xpos) :below (+ (getf claim :xpos)
                                              (getf claim :width))
    :do (loop
          :for y :from (getf claim :ypos) :below (+ (getf claim :ypos)
                                                    (getf claim :height))
          :do (push (getf claim :id) (aref board x y)))))

(defun day3 ()
  (let ((board (make-array '(1000 1000) :initial-element nil))
        (ids (make-hash-table :test 'equal)))
    (loop-line-by-line (puzzlepath "input3.txt")
      :for claim := (day3-parse-line line)
      :do (make-claim claim board)
      :do (setf (gethash (getf claim :id) ids) t))
    (loop
      :with overlaps := 0
      :for index :below (reduce #'* (array-dimensions board))
      :for entries := (row-major-aref board index)
      :when (>= (length entries) 2)
      :do (incf overlaps) :and
      :do (dolist (id entries) (remhash id ids))
      :finally
      (format t "There are ~a square inches of fabric within two or more claims.~%Id ~a has no overlaps at all." overlaps (first (hash-keys ids))))))

