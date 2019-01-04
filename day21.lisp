;;;; day21.lisp

(in-package :advent-of-code-2018)

;;; works for my input
;;; how to find the correct register? would need more puzzle inputs probably
(defun day21-next (state)
  (loop
    :while (day19-execute! state)
    :until (= 28 (pstate-ip state))
    :finally (return (aref (pstate-registers state) 1))))

;;; Part 2 slow
;;; further reassembling the code by hand would probably allow for optimization
(defun day21 ()
  (let* ((state (day19-initialize-program (day19-parse-input (puzzlefile 21))))
         (first-value (day21-next state)))
    (format t "A value of ~a ends the program fastest.~%" first-value)
    (loop :with seen := (make-hash-table)
          :for prev-value := nil :then next-value
          :for next-value := first-value :then (day21-next state)
          :until (gethash next-value seen nil)
          :do (setf (gethash next-value seen) t)
          :finally (format t "A value of ~a ends the program the latest.~%" prev-value))))
