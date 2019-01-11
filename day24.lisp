;;;; day24.lisp

(in-package :advent-of-code-2018)

;;; group struct

(defstruct (day24-group (:conc-name group-))
  units
  hp
  dmg
  dmg-type
  initiative
  weaknesses
  immunities)

(defun group-effective-power (group)
  (* (group-units group) (group-dmg group)))

(defun group-quicker-p (group1 group2)
  (if (= (group-effective-power group1) (group-effective-power group2))
      (> (group-initiative group1) (group-initiative group2))
      (> (group-effective-power group1) (group-effective-power group2))))

(defun group-dead-p (group)
  (<= (group-units group) 0))

;;; input parsing

(defun day24-read-types (string)
  (loop :with start := 0
        :while (< start (length string))
        :for (type index) := (multiple-value-list (read-from-string string nil nil :start start))
        :when type :collect type
        :do (setf start (1+ index))))

(defun day24-parse-types (types)
  (list
   (ppcre:register-groups-bind ((#'day24-read-types weak))
       ("weak to ([\\w\\s,]+;?)" types)
     weak)
   (ppcre:register-groups-bind ((#'day24-read-types immune))
       ("immune to ([\\w\\s,]+;?)" types)
     immune)))

(defun day24-parse-unit (line)
  (ppcre:register-groups-bind
      ((#'parse-integer amount hp) types (#'parse-integer dmg) (#'read-from-string dmg-type) (#'parse-integer initiative))
      ("(\\d+) units each with (\\d+) hit points (?:\\(([\\s\\w;,]*)\\) )?with an attack that does (\\d+) (\\w+) damage at initiative (\\d+)" line)
    (destructuring-bind (weaknesses immunities) (day24-parse-types types)
      (make-day24-group :units amount
                        :hp hp
                        :dmg dmg
                        :dmg-type dmg-type
                        :initiative initiative
                        :weaknesses weaknesses
                        :immunities immunities))))

(defun day24-parse-input (&optional (file (puzzlefile 24)))
  (with-open-file (in file)
    (read-line in)
    (loop :for line := (read-line in)
          :until (= 0 (length line))
          :collect (day24-parse-unit line) :into immune
          :finally (read-line in)
                   (return (list immune (loop :for line := (read-line in nil) :while line
                                              :collect (day24-parse-unit line)))))))

;;; solving the task

(defun day24-calc-dmg (attacker target)
  (* (group-effective-power attacker)
     (if (find (group-dmg-type attacker) (group-immunities target)) 0 1)
     (if (find (group-dmg-type attacker) (group-weaknesses target)) 2 1)))

(defun day24-make-target-decider (attacker)
  (lambda (target1 target2)
    (let ((dmg2 (day24-calc-dmg attacker target2)))
      (cond
        ((= dmg2 0) target1)
        ((null target1) target2)
        ((> (day24-calc-dmg attacker target1) dmg2) target1)
        ((< (day24-calc-dmg attacker target1) dmg2) target2)
        ((> (group-effective-power target1) (group-effective-power target2)) target1)
        ((< (group-effective-power target1) (group-effective-power target2)) target2)
        ((> (group-initiative target1) (group-initiative target2)) target1)
        (t target2)))))

(defun day24-choose-targets (attackers defenders)
  (loop :for attacker :in (sort (copy-list attackers) #'group-quicker-p)
        :for unchosen-defenders := defenders :then (remove target unchosen-defenders)
        :while unchosen-defenders
        :for target := (reduce (day24-make-target-decider attacker) unchosen-defenders :initial-value nil)
        :when target
        :collect (list attacker target)))

(defun day24-resolve-combat! (attacks)
  (loop :for (attacker target) :in (sort attacks #'> :key (lambda (a) (group-initiative (first a))))
        :unless (group-dead-p attacker)
        :do (decf (group-units target) (floor (day24-calc-dmg attacker target) (group-hp target)))))

(defun day24-fight! (immune-system-init infection-init)
  (loop :initially (setf immune-system (copy-list immune-system-init))
                   (setf infection (copy-list infection-init))
        :for immune-system := (remove-if #'group-dead-p immune-system)
        :for infection := (remove-if #'group-dead-p infection)
        :for last-unit-count := -1 :then unit-count
        :for unit-count := (+ (reduce #'+ immune-system :key #'group-units)
                              (reduce #'+ infection :key #'group-units))
        :until (= unit-count last-unit-count)
        :until (null immune-system)
        :until (null infection)
        :for isystem-targets := (day24-choose-targets immune-system infection)
        :for infection-targets := (day24-choose-targets infection immune-system)
        :do (day24-resolve-combat! (append isystem-targets infection-targets) boost)
        :finally (return (list immune-system infection))))

(defun day24-apply-boost! (immune-system boost)
  (mapc (lambda (group) (incf (group-dmg group) boost)) immune-system))

(defun day24 ()
  (destructuring-bind (immune-system-init infection-init) (day24-parse-input)
    (symbol-macrolet ((immune-system (mapcar #'copy-structure immune-system-init))
                      (infection (mapcar #'copy-structure infection-init)))
      (format t "Without boost the infection would win with ~a units.~%"
              (reduce #'+ (second (day24-fight! immune-system infection)) :key #'group-units))
      (loop :for boost := 1 :then (incf boost)
            :for (is-survivors infection-survivors) := (day24-fight! (day24-apply-boost! immune-system boost)
                                                                     infection)
            :for infection-units := (reduce #'+ infection-survivors :key #'group-units)
            :until (= 0 infection-units)
            :finally (format t "With a boost of ~a the immune-system wins with ~a units.~%" boost
                             (reduce #'+ is-survivors :key #'group-units))))))
