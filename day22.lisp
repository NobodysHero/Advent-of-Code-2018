;;;; day22.lisp

(in-package :advent-of-code-2018)

(defun day22-parse-input ()
  (mapcar #'extract-integers (read-puzzlefile "input22.txt")))

(defun day22-generate-cave (width height cave-depth &optional (target-x 0) (target-y 0))
  "Generates the cave."
  (let ((indices (make-array (list (1+ width) (1+ height)) :element-type '(mod 20183))))
    ;; compute the geological indices
    (loop :for x :from 0 :upto width
          :do (setf (aref indices x 0) (mod (+ cave-depth (* x 16807)) 20183)))
    (loop :for y :from 0 :upto height
          :do (setf (aref indices 0 y) (mod (+ cave-depth (* y 48271)) 20183)))
    (loop :for y :from 1 :upto height
          :do (loop :for x :from 1 :upto width
                    :when (and (= x target-x) (= y target-y))
                    :do (setf (aref indices x y) 0)
                    :else :do (setf (aref indices x y)
                                    (mod
                                     (+ cave-depth
                                        (* (aref indices (1- x) y)
                                           (aref indices x (1- y))))
                                     20183))))
    ;; take mod 3 for type
    (loop :with cave := (make-array (list (1+ width) (1+ height)) :element-type '(mod 3))
          :for i :from 0 :below (array-total-size indices)
          :do (setf (row-major-aref cave i) (mod (row-major-aref indices i) 3))
          :finally (return cave))))

;;for debugging
(defun day22-print-cave (cave)
  "Used for debugging. Displayes the cave as seen on the AoC website."
  (loop :for y :below (array-dimension cave 1)
        :do (format t "~{~a~}~%" (loop for x :below (array-dimension cave 0)
                                       :collect (case (mod (aref cave x y) 3)
                                                  (0 #\.)
                                                  (1 #\=)
                                                  (2 #\|)
                                                  (t #\x))))))

(defun day22-shortest-path (cave target-x target-y)
  "Finds the shortest path from the 0,0 coordinate to the target's coordinate. The torch is equipped at the start and end."
  (flet ((neighbours (a)
           ;;neighbours are either displaced 1 coordinate step or change gear
           (destructuring-bind (x y gear) a
             (loop :for (dx dy) :in '((-1 0) (1 0) (0 -1) (0 1))
                   :for nx := (+ dx x)
                   :for ny := (+ dy y)
                   :when (and (array-in-bounds-p cave nx ny)
                              (not (= gear (aref cave nx ny))))
                   :collect (list nx ny gear) :into neighbours
                   ;append gear change to the neighbours and return
                   :finally (return (cons (list x  y (first (delete gear (delete (aref cave x y) (list 0 1 2)))))
                                          neighbours)))))
         (distance (a b)
           (let ((from-gear (third a))
                 (target-gear (third b)))
             (if (= target-gear from-gear) 1 7))))
    (second (multiple-value-list (dijkstra '(0 0 1)
                                           (list target-x target-y 1)
                                           #'distance
                                           #'neighbours
                                           :test 'equalp)))))

(defun day22 ()
  (destructuring-bind ((depth) (target-x target-y)) (day22-parse-input)
    (let ((cave (day22-generate-cave (+ 40 target-x) (+ 40 target-y) depth target-x target-y)))
      (format t "The total risk index is: ~a~%"
              (loop :for x :upto target-x
                    :sum (loop :for y :upto target-y
                               :sum (aref cave x y))))
      (format t "The quickest path to the target takes ~a minutes.~%"
              (day22-shortest-path cave target-x target-y)))))
