;;;; day13.lisp

(in-package :advent-of-code-2018)

;;;minecart = (list row-major-pos direction turn-count)

(defstruct minecart
  pos
  direction
  (turns 0)
  (crashed nil))

(defun day13-minecart-p (char)
  (find char "<>^v"))

(defun day13-horizontal-p (char)
  (find char "<>"))

(defun day13-parse-input (&optional (input (read-puzzlefile "input13.txt")))
  (let* ((board (make-array (list (length input)
                                  (length (first input)))
                            :initial-contents input))
         (minecarts (loop 
                      :for index :below (array-total-size board)
                      :for sign := (row-major-aref board index)
                      :when (day13-minecart-p sign)
                      :collect (make-minecart :pos index :direction sign)
                      :and :do (setf (row-major-aref board index)
                                     (if (day13-horizontal-p sign)
                                         #\-
                                         #\|)))))
    (list board minecarts)))

(defun day13-turn (minecart)
  (setf (minecart-direction minecart)
        (char "<^>v<^>v<^>v" (+ 3
                                (mod (minecart-turns minecart) 3)
                                (position (minecart-direction minecart) "<^>v"))))
  (incf (minecart-turns minecart)))

(defun day13-move-cart! (board cart)
  (with-accessors ((pos minecart-pos) (direction minecart-direction) (turns minecart-turns)) cart
    (let* ((width (array-dimension board 1)))
      (incf pos (case direction
                  (#\> 1)
                  (#\< -1)
                  (#\^ (- width))
                  (#\v width)
                  (t (error "This ain't no minecart's direction: ~a~%" direction))))
      (case (row-major-aref board pos)
        ((#\- #\|) t)
        (#\\ (setf direction (case direction
                               (#\> #\v)
                               (#\v #\>)
                               (#\< #\^)
                               (#\^ #\<))))
        (#\/ (setf direction (case direction
                               (#\> #\^)
                               (#\v #\<)
                               (#\< #\v)
                               (#\^ #\>))))
        (#\+ (day13-turn cart)))
      cart)))

(defun day13-check-collision! (cart minecarts)
  (let ((collisions
          (remove-if #'minecart-crashed
                     (remove (minecart-pos cart) minecarts :key #'minecart-pos :test (complement #'=)))))
    (unless (= 1 (length collisions))
      (mapc (lambda (c) (setf (minecart-crashed c) t)) collisions))))

(defun day13-step! (board minecarts)
  (loop
    :for minecart :in (sort (copy-list minecarts) #'< :key #'minecart-pos)
    :unless (minecart-crashed minecart)
    :do (day13-move-cart! board minecart)
    :and :do (day13-check-collision! minecart minecarts)))

(defun day13-pos->coord (board pos)
  (multiple-value-bind (y x) (floor pos (array-dimension board 1))
    (list x y)))

(defun day13 ()
  (destructuring-bind (board carts) (day13-parse-input)
    (loop
      :until (some #'minecart-crashed carts)
      :do (day13-step! board carts)
      :finally
      (format t "The first crash occurs at (~{~a~^,~}).~%"
              (day13-pos->coord board (minecart-pos (find-if #'minecart-crashed carts)))))
    (loop
      :for uncrashed := (remove-if #'minecart-crashed carts)
      :until (= 1 (length uncrashed))
      :do (day13-step! board uncrashed)
      :finally
      (format t "After the last crash the last minecart is at (~{~a~^,~}).~%"
                (day13-pos->coord board (minecart-pos (first uncrashed)))))))

;;helper - bit ugly
(defun day13-print-state (board minecarts)
  (let* ((width (array-dimension board 1))
         (copy (make-array (array-dimensions board)
                           :displaced-to (copy-seq
                                          (make-array (array-total-size board) :displaced-to board)))))
    (loop :for cart :in minecarts
          :do (setf (row-major-aref copy (minecart-pos cart)) (minecart-direction cart))
          :finally
          (format t "~{~a~%~}~%"
                  (loop :for y :below (array-dimension board 0)
                        :collect (coerce (make-array width :displaced-to copy :displaced-index-offset (* y width))
                                         'string))))))
