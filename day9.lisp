;;;; day9.lisp

(in-package :advent-of-code-2018)

(defun day9-parse-input ()
  (let ((input (split-seq (first (read-puzzlefile "input9.txt")) #\Space)))
    (list (parse-integer (first input))
          (parse-integer (nth 6 input)))))

(defun day9-make-board ()
  (let ((zero (list 0)))
    (setf (cdr zero) zero)))

(defun day9-insert-after (board marble)
  (let ((new (list marble)))
    (setf (cdr new) (cdr board)
          (cdr board) new)))

(defun day9-remove-next (board)
  (let ((removed (cdr board)))
    (setf (cdr board) (cddr board))
    removed))

(defun day9-play (players last-marble)
  (declare (optimize (debug 3) (safety 3)))
  (loop
    :with scores := (make-hash-table)
    :with board := (day9-make-board)
    :with trailing := board
    :with length := 1
    :for marble :from 1 :upto last-marble
    :when (= marble 8) :do (setf trailing board)
    :unless (> 5 (mod marble 23)) :do (setf trailing (cddr trailing))
    :unless (= 0 (mod marble 23))
    :do (setf board (day9-insert-after (cdr board) marble))
    :and :do (incf length)
    :else :do (let ((removed (day9-remove-next trailing)))
                (setf board (cdr removed) trailing (cdr removed))
                (incf (gethash (mod marble players) scores 0) (+ marble (car removed)))
                (decf length)
                ;(format t "~a points for player ~a.~%" (+ marble (car removed)) (1+ (mod marble players)))
                )
    :finally (return (multiple-value-list (max-key scores)))))

(defun day9 ()
  (destructuring-bind (players marbles) (day9-parse-input)
    (destructuring-bind (player points) (day9-play players marbles)
      (format t "The first game wins elf #~a with ~a points.~%" player points))
    (destructuring-bind (player points) (day9-play players (* 100 marbles))
      (format t "The second game wins elf #~a with ~a points.~%" player points))))
