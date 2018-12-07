;;;; day9.lisp

(in-package :advent-of-code-2018)

(defun day7-parse-line (line)
  (list (char line 5) (char line 36)))

(defun day7-build ()
  (let ((counts (make-hash-table :test 'equal))
        (links (make-hash-table :test 'equal)))
    (loop-line-by-line (puzzlepath "input7.txt")
      :for (before later) := (day9-parse-line line)
      :do (push later (gethash before links nil))
      :do (incf (gethash later counts 0))
      :do (setf (gethash before counts) (gethash before counts 0))
      :finally (return (list counts links)))))

(defun day7-next (counts)
  (loop
    :with next-key := nil
    :for key :being :the :hash-keys :of counts
    :when (and (or (null next-key)
                   (char< key next-key))
               (= 0 (gethash key counts)))
    :do (setf next-key key)
    :finally (return next-key)))

(defun day7-task-time (letter)
  (- (char-code (char-upcase letter)) 4)); #\A -> 65 - 4 -> 61

(defun day7-shortest-worker (w1 w2)
  (if (< (first w1) (first w2))
      w1
      w2))

(defun day7-solve (&optional (worker-count 5))
  (destructuring-bind (counts links) (day9-build)
    (loop
      :with seq := nil
      :with workers := nil
      :with time := 0
      :for next := (day9-next counts)
      :while (or workers next)
      :when (or (= (length workers) worker-count)
                (null next))
      :do (let ((earliest (reduce #'day9-shortest-worker workers)))
            (setf workers (remove earliest workers))
            (dolist (child (gethash (cdr earliest) links))
              (decf (gethash child counts)))
            (push (cdr earliest) seq)
            (setf time (car earliest)))
      :when next
      :do (push (cons (+ time (day9-task-time next)) next) workers)
      :and :do (remhash next counts)
      :finally (return (list (coerce (nreverse seq) 'string) time)))))

(defun day7 ()
  (destructuring-bind (seq time) (day9-solve 1)
    (format t "With 1 worker the tasks are completed in this order: ~a and take ~a seconds.~%"
            seq time))
  (destructuring-bind (seq time) (day9-solve 5)
      (format t "With 5 workers the tasks are completed in this order: ~a and take ~a seconds.~%"
              seq time)))
