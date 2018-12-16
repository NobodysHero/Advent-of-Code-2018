;;;; day14.lisp

(in-package :advent-of-code-2018)

(defun day14-prepare-recipes (length)
  (let ((recipes
          (make-array (+ length 10) :fill-pointer 0
                                    :element-type '(mod 10)
                                    :adjustable t)))
    (vector-push 3 recipes)
    (vector-push 7 recipes)
    recipes))

(defun day14-digits (number)
  (nreverse (loop
              :for (rest digit) := (multiple-value-list (floor number 10))
              :then (multiple-value-list (floor rest 10))
              :collect digit
              :while (> rest 0))))

(defun day14 (&optional (input 327901))
  (loop :with recipes := (day14-prepare-recipes input)
        :for elf1 := 0 :then (mod (+ elf1 1 recipe1) (length recipes))
        :for elf2 := 1 :then (mod (+ elf2 1 recipe2) (length recipes))
        :for recipe1 := (aref recipes elf1)
        :for recipe2 := (aref recipes elf2)
        :do (dolist (new-recipe (day14-digits (+ recipe1 recipe2)))
              (vector-push new-recipe recipes))
        :while (< (length recipes) (array-dimension recipes 0))
        :finally (format t "Answer Part1: ~a~%"
                         (map 'string #'digit-char (subseq recipes input)))))

(defun day14-extra (&optional (input 327901))
  (loop :with recipes := (day14-prepare-recipes 21000000)
        :with code := (day14-digits input)
        :with at fixnum := 0
        :for elf1 fixnum := 0 :then (mod (+ elf1 1 recipe1) (length recipes))
        :for elf2 fixnum := 1 :then (mod (+ elf2 1 recipe2) (length recipes))
        :for recipe1 := (aref recipes elf1)
        :for recipe2 := (aref recipes elf2)
        :do (dolist (new-recipe (day14-digits (+ recipe1 recipe2)))
              (vector-push-extend new-recipe recipes 10000)
              (cond
                ((null (nth at code)) (incf at))
                ((= (nth at code) new-recipe) (incf at))
                (t (setf at 0))))
        :while (nth at code)
        :finally (format t "Answer Part2: ~a~%"
                         (- (length recipes) at))))
