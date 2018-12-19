;;;; day18.lisp

(in-package :advent-of-code-2018)

(defun day18-parse-input ()
  (let ((raw (read-puzzlefile "input18.txt")))
    (make-array (list (length raw) (length (first raw)))
                :initial-contents raw
                :element-type 'standard-char)))

(defun day18-count (grid x y)
  (loop
    :with trees := 0
    :with lumberyards := 0
    :for ix :from (max 0 (1- x)) :upto (min (1+ x) (1- (array-dimension grid 0)))
    :do (loop :for iy :from (max 0 (1- y)) :upto (min (1+ y) (1- (array-dimension grid 1)))
              :unless (and (= ix x) (= iy y))
              :do (case (aref grid ix iy)
                    (#\| (incf trees))
                    (#\# (incf lumberyards))))
    :finally (return (list trees lumberyards))))

(defun day18-next-state (state counts)
  (destructuring-bind (trees lumberyards) counts
    (case state
      (#\. (if (>= trees 3) #\| #\.))
      (#\| (if (>= lumberyards 3) #\# #\|))
      (#\# (if (and (>= lumberyards 1)
                    (>= trees 1))
               #\# #\.)))))

(defun day18-step (grid &optional (into nil))
  (loop
    :with new := (or into (make-array (array-dimensions grid)))
    :for x :below (array-dimension grid 0)
    :do (loop :for y :below (array-dimension grid 1)
              :do (setf (aref new x y) (day18-next-state
                                        (aref grid x y)
                                        (day18-count grid x y))))
    :finally (return new)))

(defun day18-score (grid)
  (let ((view (make-array (array-total-size grid) :displaced-to grid
                                                  :element-type 'standard-char)))
    (* (count #\| view) (count #\# view))))

(defun day18 ()
  (let ((it 0 ))
    (loop :with grid := (day18-parse-input)
          :with into := (make-array (array-dimensions grid) :element-type 'standard-char)
          :repeat 1000
          :do (shiftf into grid (day18-step grid into))
          :do (format t "~4d: ~10d~%" (incf it) (day18-score grid))
          :finally (format t "The resource score after 10mins is: ~a~%"
                           (day18-score grid)))))
