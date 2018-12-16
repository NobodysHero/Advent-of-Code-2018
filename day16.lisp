;;;; day16.lisp

(in-package :advent-of-code-2018)

(defun day16-parse-input ()
  (with-open-file (in (puzzlepath "input16.txt"))
    (loop :for line := (read-line in nil)
          :while line
          :unless (= 0 (length line))
          :when (char= #\B (char line 0))
          :collect (mapcar #'extract-integers (list line (read-line in) (read-line in))) :into test-cases
          :else :collect (extract-integers line) :into test-prog
          :finally (return (list test-cases test-prog)))))

(defun day16-execute! (register command)
  (destructuring-bind (cmd inp1 inp2 out) command
    (macrolet ((reg (x)
                 `(nth ,x register)))
      (setf (reg out)
            (case cmd
              (addr (+ (reg inp1) (reg inp2)))
              (addi (+ (reg inp1) inp2))
              (mulr (* (reg inp1) (reg inp2)))
              (muli (* (reg inp1) inp2))
              (banr (logand (reg inp1) (reg inp2)))
              (bani (logand (reg inp1) inp2))
              (borr (logior (reg inp1) (reg inp2)))
              (bori (logior (reg inp1) inp2))
              (setr (reg inp1))
              (seti inp1)
              (gtir (if (> inp1 (reg inp2)) 1 0))
              (gtri (if (> (reg inp1) inp2) 1 0))
              (gtrr (if (> (reg inp1) (reg inp2)) 1 0))
              (eqir (if (= inp1 (reg inp2)) 1 0))
              (eqri (if (= (reg inp1) inp2) 1 0))
              (eqrr (if (= (reg inp1) (reg inp2)) 1 0))
              (t (error "What is this command ~a with arguments ~a, ~a and ~a.~%~%" cmd inp1 inp2 out))))
      register)))

(defun day16-test-case (test-case &optional (possible-opcodes '(addr addi mulr muli banr bani borr bori setr seti gtir gtri gtrr eqir eqri eqrr)))
  (destructuring-bind (before instr after) test-case
    (loop :for opcode :in possible-opcodes
          :when (equalp after (day16-execute! (copy-seq before) (cons opcode (rest instr))))
          :collect opcode)))

(defun day16-find-next-opcode (test-cases opcodes)
  (loop :for test-case :in test-cases
        :for possibilities := (day16-test-case test-case opcodes)
        :when (= 1 (length possibilities))
        :return (list (first (second test-case)) (first possibilities))))

(defun day16-construct-opcode-ids (test-cases)
  (loop :with id->opcode := (make-array 16 :initial-element nil)
        :with opcodes := (list 'addr 'addi 'mulr 'muli 'banr 'bani 'borr 'bori 'setr 'seti 'gtir 'gtri 'gtrr 'eqir 'eqri 'eqrr)
        :for (id opcode) := (day16-find-next-opcode test-cases opcodes)
        :unless id :do (error "Could not reconstruct all opcodes. Left: ~{~a~^,~}~%" opcodes)
        :do (setf (aref id->opcode id) opcode)
        :do (setf opcodes (delete opcode opcodes))
        :while opcodes
        :finally (return id->opcode)))

(defun day16 ()
  (destructuring-bind (cases prog) (day16-parse-input)
    (loop :for case :in cases
          :count (<= 3 (length (day16-test-case case))) :into count
          :finally (format t "There are ~a test cases with 3 or more possible instructions.~%" count))
    (loop :with id->opcode := (day16-construct-opcode-ids cases)
          :with registers := (list 0 0 0 0)
          :for (id . args) :in prog
          :do (setf registers (day16-execute! registers (cons (aref id->opcode id) args)))
          :finally (format t "After executing the test program the value in register 0 is ~a.~%" (first registers)))))
