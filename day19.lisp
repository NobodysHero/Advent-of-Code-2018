;;;; day19.lisp

(in-package :advent-of-code-2018)

(defstruct (program-state (:conc-name pstate-))
  instructions
  (ip 0)
  (ip-binding nil)
  (registers (make-array 6 :initial-element 0 :element-type 'integer)))

(defun day19-parse-input (&optional (file (puzzlepath "input19.txt")))
  (loop-line-by-line file
    :when (char= #\# (char line 0)) :collect (list 'ip (parse-integer line :start 4))
    :else :collect (cons (read-from-string line) (extract-integers line))))

(defun day19-initialize-program (instruction-list)
  (make-program-state :instructions (make-array (1- (length instruction-list))
                                                :initial-contents (rest instruction-list))
                      :ip-binding (second (first instruction-list))))

(defun day19-execute! (program-state)
  (with-accessors ((instr pstate-instructions)
                   (ip pstate-ip)
                   (ip-binding pstate-ip-binding)
                   (registers pstate-registers))
      program-state
    (when ip-binding
      (setf (aref registers ip-binding) ip))
    ;(format t "before: ~a~%" registers)
    (let ((command  (aref instr ip)))
      (if (eq 'ip (first command))
          (setf ip-binding (second command))
          (day16-execute! registers command))
     ; (format t "command: ~a~%~%" command)
      )
    (when ip-binding
      (setf ip (aref registers ip-binding)))
    (incf ip)
    (< -1 ip (length instr))))

;;; runs FOREVER
(defun day19-brute-force ()
  (loop :with prog := (day19-initialize-program (day19-parse-input))
        :count t :into loops
        :while (day19-execute! prog)
        :finally (format t "The first background process exits with the value ~a in the zeroth register.~%"
                         (aref (pstate-registers prog) 0)))
  (let ((prog (day19-initialize-program (day19-parse-input))))
    (setf (aref (pstate-registers prog) 0) 1)
    (loop :while (day19-execute! prog)
          :finally
          (format t "The second background process exits with the value ~a in the zeroth register.~%"
                  (aref (pstate-registers prog) 0)))))

(defun day19-analytic (input)
  (loop :for a :from 1 :upto (ceiling (sqrt input))
        :when (= 0 (mod input a)) :sum (+ a (/ input a))))

(defun day19 ()
  (format t "The first background process exits with the value ~a in the zeroth register.~%"
          (day19-analytic 931))
  (format t "The second background process exits with the value ~a in the zeroth register.~%"
          (day19-analytic 10551331)))
