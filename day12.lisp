;;;; day12.lisp

(in-package :advent-of-code-2018)

(defun day12-state->id (state start)
  (loop :for multiplier := 1 :then (* 2 multiplier)
        :for index :from start :below (+ start 5)
        :sum (* multiplier (aref state index))))

(defun day12-parse-input ()
  (labels ((char->state (char)
             (cond
             ((char= #\# char) 1)
             ((char= #\. char) 0)
             (t nil)))
           (parse-rule (raw)
             (when (char= #\# (char raw 9))
               (day12-state->id (map 'simple-vector #'char->state (subseq raw 0 5)) 0))))
    (let* ((input (read-puzzlefile "input12.txt"))
           (initial-state (remove nil (map 'list #'char->state (first input))))
           (raw-rules (rest (rest input)))
           (rules (make-hash-table)))
      (loop :for raw-rule :in raw-rules
            :for rule := (parse-rule raw-rule)
            :when rule :do (setf (gethash rule rules) t))
      (list (make-array (length initial-state) :initial-contents initial-state)
            rules))))

(defun day12-pad-initial-state (istate padding)
  (let ((state (make-array (+ padding (length istate) padding) :element-type 'bit :initial-element 0)))
    (loop :for index :from padding :below (+ padding (length istate))
          :for initial-val :across istate
          :do (setf (aref state index) initial-val))
    state))

(defun day12-score-state (state initial-value)
  (loop :for index :below (length state)
        :for val := initial-value :then (incf initial-value)
        :sum (* val (aref state index))))

(defun day12-evolve (state rules)
  (loop :with new-state := (make-array (length state) :element-type 'bit :initial-element 0)
        :for index :below (- (length state) 5)
        :when (gethash (day12-state->id state index) rules) :do (setf (aref new-state (+ 2 index)) 1)
        :finally (return new-state)))

(defun day12-do-generations (istate rules generations)
  (loop 
    :for state := (day12-pad-initial-state istate (+ 5 generations)) :then (day12-evolve state rules)
    :repeat generations
    :finally (return state)))

(defun day12 ()
  (destructuring-bind (istate rules) (day12-parse-input)
    (let ((gen20 (day12-do-generations istate rules 20)))
      (format t "The sum of pot numbers with plants at generation 20 is ~a.~%"
              (day12-score-state gen20 -25))
      (let* ((gen300 (day12-do-generations gen20 rules 280))
             (gen301 (day12-evolve gen300 rules))
             (score-301 (day12-score-state gen301 -310))
             (delta-score (- score-301 (day12-score-state gen300 -310))))
        (format t "The sum of pot numbers at generation 50000000000 is ~a.~%"
                (+ score-301 (* delta-score (- 50000000000 301))))))))
