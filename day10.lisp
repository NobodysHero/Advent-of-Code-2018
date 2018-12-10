;;;; day10.lisp

(in-package :advent-of-code-2018)

(defun day10-parse-input ()
  (let ((input
          (loop-line-by-line (puzzlepath "input10.txt")
            :collect (ppcre:register-groups-bind ((#'parse-integer x y dx dy))
                         ("position=<( ?-?\\d+), ( ?-?\\d+)> velocity=<( ?-?\\d+), ( ?-?\\d+)>" line)
                       (list x y dx dy)))))
    (make-array (length input) :element-type 'list :initial-contents input)))

(defun day10-step (points &optional (amount 1))
  (loop :for point :across points
        :do (incf (first point) (* (third point) amount))
        :do (incf (second point) (* (fourth point) amount)))
  points)

(defun day10-boundary (points)
  (loop :for (x y) :across points
        :minimize x :into min-x
        :maximize x :into max-x
        :minimize y :into min-y
        :maximize y :into max-y
        :finally (return (list min-x min-y (- max-x min-x) (- max-y min-y)))))

(defun day10-pretty-print (points)
  (destructuring-bind (dx dy width height) (day10-boundary points)
    (loop
      :with positions := (build-set points :test 'equal :key (lambda (p) (cons (first p) (second p))))
      :for y :upto height
      :do (loop :for x :upto width
                :do (format t (if (gethash (cons (+ dx x) (+ dy y)) positions) "#" " ")))
      :do (format t "~%"))))

(defun day10 ()
  (loop :for points := (day10-parse-input) :then (day10-step points)
        :for (nil nil width height) := (day10-boundary points)
        :for last-area := nil :then area
        :for area := (* width height)
        :until (and last-area (> area last-area))
        :count t :into seconds
        :finally (progn
                   (format t "After ~a seconds the following message appears:~%~%" (1- seconds))
                   (day10-pretty-print (day10-step points -1)))))
