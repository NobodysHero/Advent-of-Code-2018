;;;; day6.lisp

(in-package :advent-of-code-2018)

(defun manhattan (x1 y1 x2 y2)
  (declare (optimize (speed 3) (debug 0) (safety 1))
           (fixnum x1 y1 x2 y2))
  (the fixnum (+ (abs (- x1 x2))
                 (abs (- y1 y2)))))

(defun day6-insert (newx newy name grid)
  (let ((width (array-dimension grid 0))
        (height (array-dimension grid 1)))
    (loop
      :for x :below width
      :do (loop
            :for y :below height
            :for dist := (manhattan x y newx newy)
            :for cell := (aref grid x y)
            :when (or (null cell) (< dist (first cell)))
            :do (setf (aref grid x y) (list dist name (or (third cell) 0)))
            :else :when (= dist (first cell))
            :do (setf (second (aref grid x y)) nil)
            :end
            :do (incf (third (aref grid x y)) dist)))))

(defun day6-shift-coords (dx dy)
  (lambda (coord)
    (cons (- (car coord) dx) (- (cdr coord) dy))))

(defun day6-count (grid)
  (let ((counters (make-hash-table))
        (infinites (make-hash-table))
        (width (array-dimension grid 0))
        (height (array-dimension grid 1))
        (safe-dist 10000)
        (good-region 0))
    (loop :for x :below width
          :do (loop :for y :from 0 :below height
                    :for (id total) := (cdr (aref grid x y))
                    :when id
                    :do (incf (gethash id counters 0))
                    :and :when (or (= x 0) (= x (1- width))
                              (= y 0) (= y (1- height)))
                    :do (setf (gethash id infinites) t) :end
                    :when (< total safe-dist) :do (incf good-region)))
    ;remove infinite areas
    (maphash (lambda (k v) (when v) (remhash k counters)) infinites)
    (values (second (multiple-value-list (max-key counters )))
            good-region)))

(defun day6-prep-input ()
  (loop-line-by-line (puzzlepath "input6.txt")
    :for (x . y) := (ppcre:register-groups-bind
                        ((#'parse-integer x y)) ("(\\d+), (\\d+)" line)
                      (cons x y))
    :collecting (cons x y) :into coords
    :minimizing x :into min-x
    :maximizing x :into max-x
    :minimizing y :into min-y
    :maximizing y :into max-y
    :finally (return (list coords min-x max-x min-y max-y))))

;;; this is correct for part 1, but not for part 2.
;;; Luckily it gives the right result for my input
(defun day6 ()
  (destructuring-bind
      (raw-coords min-x max-x min-y max-y) (day6-prep-input)
    (let ((grid (make-array (list (- max-x min-x -2) (- max-y min-y -2)) :initial-element nil))
          (coords (mapcar (day6-shift-coords (- min-x 1) (- min-y 1)) raw-coords)))
      ;; insert all points
      (loop
        :for (x . y) :in coords
        :for id :from 0
        :do (day6-insert x y (intern (format nil "~a" id)) grid))
      (multiple-value-bind (largest-area save-spots) (day6-count grid)
        (format t "The largest finite region is ~a.~%There are ~a safe spots.~%"
                largest-area save-spots)))))

;;; performant stand-alone solution for part 2
;;; runtime ~0.13s
;;; with (debug 0) runtime ~0.1s
;;; OPTIMIZATION: compute better padding values for each side
(defun day6-extra ()
  (declare (optimize (speed 3) (debug 0) (safety 1)))
  (format t "There are ~a safe spots.~%"
          (let ((max-dist 10000))
            (destructuring-bind (coords min-x max-x min-y max-y) (day6-prep-input)
              (declare (fixnum min-x max-x min-y max-y) (list coords))
              (loop
                :with padding := (ceiling max-dist (length coords))
                :for x :from (- min-x padding) :upto (+ max-x padding)
                :sum (loop
                       :for y :from (- min-y padding) :to (+ max-y padding)
                       :count (> max-dist (loop :for (cx . cy) :in coords
                                                :sum (manhattan x y cx cy) fixnum)))
                fixnum)))))
