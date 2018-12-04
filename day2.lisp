;;;; day2.lisp

(in-package #:advent-of-code-2018)

(defun count-chars (string)
  (loop
    :with counts := nil
    :for char :across string
    :for count = (assoc char counts)
    :if count :do
    (incf (cdr count))
    :else :do
    (push (cons char 1) counts)
    :finally (return counts)))

(defun differences (s1 s2)
  (count nil (map 'list #'equal s1 s2)))

(defun common-parts (s1 s2)
  (coerce
   (remove nil (map 'list (lambda (char1 char2)
                            (when (equal char1 char2)
                              char1))
                    s1 s2))
   'string))

(defun day2 ()
  (loop-line-by-line (puzzlepath "input2.txt")
    :for counts := (count-chars line)
    :counting (rassoc 2 counts) :into doubles
    :counting (rassoc 3 counts) :into triples
    :finally (format t "There are ~a phrases with double character and ~a phrases with triple characters.~%Solution: ~a~%" doubles triples (* doubles triples))))

(defun day2-extra ()
  (loop :for lines := (read-puzzlefile "input2.txt") :then (rest lines)
        :while lines
        :for orig := (first lines)
        :until (loop :for other :in (rest lines)
                     :when (= 1 (differences orig other))
                     :do (format t "They solution is ~s.~%" (common-parts orig other))
                     :and :return t)))
