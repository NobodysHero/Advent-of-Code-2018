;;;; day5.lisp

(in-package :advent-of-code-2018)

(defun day5-next-unit (string &optional (start 0))
  (position-if (lambda (c) (not (equal c #\0))) string :start start))

(defun day5-prior-unit (string end)
  (or (position-if (lambda (c) (not (equal c #\0))) string :from-end t :end end)
      0))

(defun day5-react! (polymer)
  (loop
    :for index1 := 0 :then index2
    :for index2 := (day5-next-unit polymer (1+ index1))
    :while index2
    :when (and (equalp (char polymer index1) (char polymer index2))
               (not (equal (char polymer index1) (char polymer index2))))
    :do (setf (char polymer index1) #\0 (char polymer index2) #\0 index2 (day5-prior-unit polymer index1))
    :finally (return (remove #\0 polymer))))

(defun day5 ()
  (let ((base-polymer (day5-react! (first (read-puzzlefile "input5.txt")))))
    (format t "The reacted polymer has still length ~a.~%" (length base-polymer))
    (format t "With removing the right unit, the shortest possible polymer has length ~a.~%"
            (loop
              :for problematic-unit :from (char-code #\a) :upto (char-code #\z)
              :minimize (length (day5-react! (remove (code-char problematic-unit)
                                                     base-polymer
                                                     :test #'equalp)))))))
