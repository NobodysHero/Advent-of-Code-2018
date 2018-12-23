;;;; day23.lisp

(in-package :advent-of-code-2018)

(defun manhattan (pos1 pos2)
  (reduce #'+ (mapcar #'abs (map 'list #'- pos1 pos2))))

(defun day23-position (robot)
  (butlast robot))

(defun day23-radius (robot)
  (fourth robot))

(defun day23 ()
  (let* ((data (sort (mapcar #'extract-integers (read-puzzlefile "input23.txt"))
                     #'> :key #'day23-radius))
         (biggest (first data)))
    (format t "The bot with the biggest range has ~a bots in range.~%"
            (count-if (lambda (pos) (<= (manhattan pos (day23-position biggest)) (day23-radius biggest)))
                      data))))

(defun day23-dist-to-origin (bot)
  (- (manhattan '(0 0 0) (day23-position bot))
     (day23-radius bot)))

(defun max-overlap (nodes possibilities adjacent)
  (if (null possibilities)
      (return-from max-overlap nodes))
  (let* ((new-nodes (aref adjacent (first possibilities)))
         ; next line is not correct. should just be (rest possibilities)
         ; but then it runs forever
         (todo (set-difference (rest possibilities) new-nodes))
         (best (max-overlap (cons (first possibilities) nodes)
                            (intersection possibilities new-nodes)
                            adjacent))
         (other-best (max-overlap nodes todo adjacent)))
    (if (> (length best) (length other-best))
        best
        other-best)))

(defun day23-build-adjacency-matrix (bots)
  (loop :with adjacent := (make-array (length bots) :initial-element nil)
        :for todo := bots :then (rest todo)
        :while todo
        :for index := 0 :then (1+ index)
        :for bot := (first todo)
        :do (loop :for other :in (rest todo)
                  :for other-index := (1+ index) :then (1+ other-index)
                  :when (day23-has-overlap-p bot other)
                  :do (push index (aref adjacent other-index))
                  :and :do (push other-index (aref adjacent index)))
        :finally (return adjacent)))

(defun day23-extra ()
  (let* ((bots (mapcar #'extract-integers (read-puzzlefile "input23.txt")))
         (adjacent (day23-build-adjacency-matrix  bots))
         (overlap-ids (max-overlap nil (loop for i below (length bots) collect i)
                       adjacent))
         (overlap-bots (mapcar (lambda (id) (nth id bots)) overlap-ids)))
    (format t "The best points are ~a units from the origin.~%"
            (reduce #'max (mapcar #'day23-dist-to-origin overlap-bots)))))
