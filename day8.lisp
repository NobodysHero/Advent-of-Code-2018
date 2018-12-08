;;;; day8.lisp

(in-package :advent-of-code-2018)

(defun day8-build-tree ()
  (with-open-file (in (puzzlepath "input8.txt"))
    (labels ((build-node ()
               (let ((num-children (read in))
                     (num-metadata (read in)))
                 (list (loop :repeat num-children
                             :collect (build-node))
                       (loop :repeat num-metadata
                             :collect (read in))))))
      (build-node))))

(defun day8-sum-metadata (node)
  (+ (reduce #'+ (mapcar #'day8-sum-metadata (first node)))
     (reduce #'+ (second node))))

(defun day8-node-value (node)
  (flet ((nth-child-value (n)
           (if (<= 1 n (length (first node)))
               (day8-node-value (nth (1- n) (first node)))
               0)))
    (if (null (first node))
        (reduce #'+ (second node))
        (reduce #'+ (mapcar #'nth-child-value (second node))))))

(defun day8 ()
  (let ((root (day8-build-tree)))
    (format t "The sum of all metadata is ~a and the score for the root node is ~a.~%"
            (day8-sum-metadata root) (day8-node-value root))))
