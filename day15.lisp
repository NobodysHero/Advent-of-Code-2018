;;;; day15.lisp

(in-package :advent-of-code-2018)

(defun day15-unit-p (obj)
  (and (listp obj)
       (or (eq (first obj) 'G)
           (eq (first obj) 'E))))

(defun day15-goblin-p (unit)
  (eq (first unit) 'G))

(defun day15-alive-p (unit)
  (> (second unit) 0))

(defun day15-target-p (attacker target)
  (and (day15-alive-p target)
       (not (eq (first attacker) (first target)))))

(defun day15-parse-input (&optional (file "input15.txt"))
  (let* ((raw (read-puzzlefile file))
         (output (make-array (list (length (first raw)) (length raw)))))
    (loop :for line :in raw
          :for y := 0 :then (1+ y)
          :do (loop :for char :across line
                    :for x := 0 :then (1+ x)
                    :do (setf (aref output x y)
                              (case char
                                (#\G (list 'G 200))
                                (#\E (list 'E 200))
                                (#\# 'W)
                                (t nil)))))
    output))

(defun day15-lexicographic-p (coord1 coord2)
  (destructuring-bind (x1 y1) coord1
    (destructuring-bind (x2 y2) coord2
      (or (< y1 y2)
          (and (= y1 y2) (< x1 x2))))))

(defun day15-break-tie (&rest coordinates)
  (loop
    :with (best-x best-y) := (first coordinates)
    :for (other-x other-y) :in (rest coordinates)
    :when (or (< other-y best-y)
              (and (= other-y best-y)
                   (< other-x best-x)))
    :do (setf best-x other-x best-y other-y)
    :finally (return (list best-x best-y))))

(defun day15-neighbours (coord)
  (destructuring-bind (x y) coord
    (list
     (list x (1- y))
     (list (1- x) y) (list (1+ x) y)
     (list x (1+ y)))))

(defmacro day15-aref-list (array list)
  `(row-major-aref ,array (apply #'array-row-major-index ,array ,list)))

(defmacro day15-map-board ((position value board) &body body)
  (let ((index (gensym "INDEX-")))
    `(loop :for ,index :below (array-total-size board)
           :for ,position := (multiple-value-list (floor ,index (array-dimension ,board 1)))
           :for ,value := (row-major-aref ,board ,index)
           ,@body)))

(defun day15-flood (board start)
  (let ((filled (make-array (array-dimensions board) :initial-element nil)))
    (loop :for todo := (list start) :then next-todo :while todo
          :for next-todo := nil :then nil
          :for level := 0 :then (1+ level)
          :do (loop :for coordinate :in todo
                    :unless (day15-aref-list filled coordinate)
                    :do (setf (day15-aref-list filled coordinate) level)
                        (dolist (n (day15-neighbours coordinate))
                          (when (and (apply #'array-in-bounds-p board n)
                                     (not (day15-aref-list board n)))
                            (push n next-todo)))))
    filled))

(defun day15-dump-board (board &optional (min-width 1))
  (format t "~%")
  (loop :with fmt := (format nil "~~~aa" min-width)
        :for y :below (array-dimension board 1)
        :do (loop :for x :below (array-dimension board 0)
                  :for obj := (aref board x y)
                  :do (format t fmt (cond
                                      ((eq obj 'W) #\#)
                                      ((null obj) " ")
                                      ((listp obj) (first obj))
                                      (t obj))))
        (format t "~%"))
  (format t "~%"))

(defun day15-turn! (board unit position attack-power)
  (let ((targets (day15-map-board (at obj board)
                   :when (and (day15-unit-p obj)
                              (day15-target-p unit obj))
                   :collect at))
        (distances (day15-flood board position)))
    (when targets
      (let ((target-field 
              (first (stable-sort
                      (sort
                       (remove nil
                               (mapcan #'day15-neighbours targets)
                               :key (lambda (coord)
                                      (day15-aref-list distances coord)))
                       #'day15-lexicographic-p)
                      #'< :key (lambda (coord)
                                 (day15-aref-list distances coord))))))
        (when target-field
          (unless (= 0 (day15-aref-list distances target-field))
            (let* ((step-distances (day15-flood board target-field))
                   (move (first (stable-sort (sort (remove nil (day15-neighbours position)
                                                           :key (lambda (coord)
                                                                  (numberp (day15-aref-list step-distances coord))))
                                                   #'day15-lexicographic-p)
                                             #'< :key (lambda (coord)
                                                        (day15-aref-list step-distances coord))))))
              (setf (day15-aref-list board position) nil)
              (setf (day15-aref-list board move) unit)
              (setf position move)))
          (let ((attackable (loop :for n :in (day15-neighbours position)
                                  :when (and (apply #'array-in-bounds-p board n)
                                             (day15-unit-p (day15-aref-list board n))
                                             (day15-target-p unit (day15-aref-list board n)))
                                  :collect n)))
            (when attackable
              (let ((target (first (stable-sort (sort attackable #'day15-lexicographic-p)
                                                #'< :key (lambda (coord)
                                                           (second (day15-aref-list board coord)))))))
                (when (<= (decf (second (day15-aref-list board target)) attack-power) 0)
                  (setf (day15-aref-list board target) nil)))))))
      t)))

(defun day15-round! (board &optional (elf-power 3))
  (let ((units (sort (day15-map-board (at obj board)
                       :when (day15-unit-p obj) :collect (list at obj))
                     #'day15-lexicographic-p :key #'first)))
    (loop :for (pos unit) :in units
          :when (day15-alive-p unit)
          :collect (day15-turn! board unit pos (if (day15-goblin-p unit) 3 elf-power)) :into found-something?
          :finally (return (not (some #'null found-something?))))))

(defun day15-copy-board (board)
  (loop :with copy := (make-array (array-dimensions board))
        :for index :below (array-total-size board)
        :for orig := (row-major-aref board index)
        :do (setf (row-major-aref copy index) (if (listp orig)
                                                  (copy-list orig)
                                                  orig))
        :finally (return copy)))

(defun day15 ()
  (let ((initial-board (day15-parse-input)))
    (loop :with board := (day15-copy-board initial-board)
          :while (day15-round! board)
          :count t :into rounds
          :finally
          (let* (team
                 (total-hp (day15-map-board (pos obj board)
                            :when (and (day15-unit-p obj)
                                       (day15-alive-p obj))
                            :sum (second obj)
                            :and :do (setf team (first obj)))))
            (format t "Initially the ~a win!~%" (if (eq team 'G) "goblins" "elves"))
            (format t "Rounds: ~a~%" rounds)
            (format t "Total HP: ~a~%" total-hp)
            (format t "Score: ~a~%~%" (* rounds total-hp))))
    (loop :for elf-power := 4 :then (1+ elf-power)
          :until (loop :with board := (day15-copy-board initial-board)
                       :with elves := (day15-map-board (pos val board)
                                        :when (and (day15-unit-p val)
                                                   (not (day15-goblin-p val)))
                                        :collect val)
                       :while (every #'day15-alive-p elves)
                       :while (day15-round! board elf-power)
                       :count t :into rounds
                       :finally (when (every #'day15-alive-p elves)
                                  (format t "The elves win without casualties after a boost of ~a.~%" elf-power)
                                  (format t "The outcome then is ~a.~%" (* rounds (reduce #'+ elves :key #'second)))
                                  (return t))))))



(defun day15-clean-test (&optional (file "input15test.txt"))
  (let ((inp (read-puzzlefile file)))
    (with-open-file (out (puzzlepath file) :direction :output :if-exists :supersede)
      (loop :for line :in inp
            :do (format out "~a~%" (subseq line 0 7))))))
