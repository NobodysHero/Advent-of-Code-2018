;;;; day25.lisp

(in-package :advent-of-code-2018)

(defun day25-make-in-range-p (p1)
  (lambda (p2) (<= (manhattan p1 p2) 3)))

(defun day25 ()
  (loop
    :for p :in (mapcar #'extract-integers (read-puzzlefile "input25.txt"))
    :for sets := (list (list p)) :then (loop :for set :in sets
                                             :if (some (lambda (p2) (in-range-p p p2)) set)
                                             :append set :into in-range
                                             :else
                                             :collect set :into other
                                             :finally (return (cons (cons p in-range) other)))
    :finally (return (format t "There are ~a constellations.~%" (length sets)))))
