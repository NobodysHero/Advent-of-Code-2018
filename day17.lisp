;;;; day17.lisp

(in-package :advent-of-code-2018)

(defun day17-sand-p (char)
  (char= char #\.))

(defun day17-passable-p (char)
  (or (day17-sand-p char)
      (char= #\| char)))

(defun day17-parse-line (line)
  (let ((x (ppcre:register-groups-bind ((#'parse-integer from to))
               ("x=(\\d+)\.\.?(\\d+)?" line)
             (list from (or to from))))
        (y (ppcre:register-groups-bind ((#'parse-integer from to))
               ("y=(\\d+)\.\.?(\\d+)?" line)
             (list from (or to from)))))
    (list (list x y))))

(defun day17-parse-input ()
  (loop-line-by-line (puzzlepath "input17.txt")
    :append (day17-parse-line line)))

(defun day17-bbox (veins)
  (loop :for ((x1 x2) (y1 y2)) :in veins
        :minimizing x1 :into min-x
        :maximizing x2 :into max-x
        :minimizing y1 :into min-y
        :maximizing y2 :into max-y
        :finally (return (list min-x max-x min-y max-y))))

(defun day17-insert-vein! (board vein &optional (dx 0) (dy 0))
  (destructuring-bind ((min-x max-x) (min-y max-y)) vein
    (loop :for x :from (- min-x dx) :upto (- max-x dx)
          :do (loop :for y :from (- min-y dy) :upto (- max-y dy)
                    :do (setf (aref board x y) #\#))))
  board)

(defun day17-initialize-board (input)
  (destructuring-bind (raw-min-x max-x min-y max-y) (day17-bbox inp)
    (let* ((width  (+ 3 (- max-x raw-min-x)));pad-x
           (height (+ 1 (- max-y min-y)))
           (min-x (1- raw-min-x))
           (board (make-array (list width height) :initial-element #\.)))
      (loop :for vein :in input
            :do (day17-insert-vein! board vein min-x min-y))
      (list board min-x min-y))))

(defun day17-flow-horizontal (board x y)
  (unless (array-in-bounds-p board x y)
    (return-from day17-flow-horizontal nil))
  (when (char= #\| (aref board x y))
    (return-from day17-flow-horizontal nil))
  (unless (day17-sand-p (aref board x y))
    (return-from day17-flow-horizontal t))
  (unless (day17-flow-horizontal board x (1+ y))
    (setf (aref board x y) #\|)
    (return-from day17-flow-horizontal nil))
  (destructuring-bind (left left-wall?)
      (loop :for ix :from (1- x) :downto 0
            :unless (day17-sand-p (aref board ix y)) :return (list (1+ ix) t)
            :when (and (day17-passable-p (aref board ix (1+ y)))
                       (not (day17-flow-horizontal board ix (1+ y))))
            :return (list ix nil)
            :finally (return (list 0 nil)))
    (destructuring-bind 
        (right right-wall?)
        (loop :for ix :from (1+ x) :below (array-dimension board 0)
              :unless (day17-sand-p (aref board ix y)) :return (list (1- ix) t)
              :when (and (day17-passable-p (aref board ix (1+ y)))
                         (not (day17-flow-horizontal board ix (1+ y))))
              :return (list ix nil)
              :finally (return (list (1- (array-dimension board 0)) nil)))
      (loop :for ix :from left :upto right
            :do (setf (aref board ix y) (if (and left-wall? right-wall?)
                                            #\~
                                            #\|)))
      (and left-wall? right-wall?))))

(defun day17-save-board (board path)
  (with-open-file (out path :direction :output
                            :if-exists :overwrite
                            :if-does-not-exist :create) 
    (loop :for y :below (array-dimension board 1)
          :do (loop :for x :below (array-dimension board 0)
                    :do (format out "~a" (aref board x y)))
          :do (format out "~%"))))

(defun day17-count (board)
  (let ((view (make-array (array-total-size board) :displaced-to board)))
    (values (count #\~ view) (count #\| view))))

(defun day17 ()
  (destructuring-bind (board dx dy) (day17-initialize-board (day17-parse-input))
    (declare (ignore dy))
    (day17-flow-horizontal board (- 500 dx) 0)
    (day17-save-board board (puzzlepath "output17.txt"))
    (multiple-value-bind (still flowing) (day17-count board)
      (list (+ still flowing) still))))
