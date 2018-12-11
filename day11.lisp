;;;; day11.lisp

(in-package :advent-of-code-2018)

(defun day11-grid-value (x y serial)
  (- (mod (floor
           (* (+ x 10)
              (+ (* (+ x 10) y)
                 serial))
           100)
          10)
     5))

(defun day11-compute-grid (serial)
  (loop :with grid := (make-array '(300 300) :element-type 'fixnum)
        :for x :below 300
        :do (loop :for y :below 300
                  :do (setf (aref grid x y) (day11-grid-value (1+ x) (1+ y) serial))
                  :when (> x 0) :do (incf (aref grid x y) (aref grid (1- x) y))
                  :when (> y 0) :do (incf (aref grid x y) (aref grid x (1- y)))
                  :when (and (> x 0) (> y 0)) :do (decf (aref grid x y) (aref grid (1- x) (1- y))))
        :finally (return grid)))

(defun day11-grid-sum (grid x y &optional (size 3))
  (let ((delta (1- size)))
    (- (aref grid (+ x delta) (+ y delta))
       (if (> y 0) (aref grid (+ x delta) (1- y)) 0)
       (if (> x 0) (aref grid (1- x) (+ y delta)) 0)
       (- (if (and (> x 0) (> y 0)) (aref grid (1- x) (1- y)) 0)))))

(defun day11-max-square (grid size)
  (loop :with max-x := nil
        :with max-y := nil
        :with max := (* -5  size size)
        :for x :from 1 :upto (- 301 size)
        :do (loop :for y :from 1 :upto (- 301 size)
                  :for sum := (day11-grid-sum grid (1- x) (1- y) size)
                  :when (> sum max) :do (setf max sum max-x x max-y y))
        :finally (return (list max-x max-y max))))

(defun day11 (&optional (serial 9306))
  (let ((grid (day11-compute-grid serial)))
    (destructuring-bind (x y charge) (day11-max-square grid 3)
      (format t "The highest charged 3x3 square has its top-left corner at (~a, ~a) and a charge of ~a.~%"
              x y charge))
    (loop
      :with max-charge := 0
      :with max-x := nil
      :with max-y := nil
      :with max-size := nil
      :for size :from 1 :to 300
      :for (x y charge) := (day11-max-square grid size)
      :when (> charge max-charge)
      :do (setf max-charge charge max-x x max-y y max-size size)
      :finally (format t "The square with the highest charge lies at (~a, ~a), is ~ax~:*~a and with a total charge of ~a.~%Solution: ~0@*~a,~a,~a" max-x max-y max-size max-charge))))
