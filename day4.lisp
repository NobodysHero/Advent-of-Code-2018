;;;; day4.lisp

(in-package #:advent-of-code-2018)

(defun day4-parse-time (time)
  ;; ex. "[1518-05-25 00:11] some more text here"
  ;;ignore the year
  (ppcre:register-groups-bind ((#'parse-integer nil month day hour min))
      ("\\[(\\d+)-(\\d+)-(\\d+) (\\d+):(\\d+)\\]" time)
    (mapcan #'list '(:month :day :hour :min) (list month day hour min))))

(defun day4-time< (log1 log2)
  (loop :for entry :in '(:month :day :hour :min)
        :when (< (getf log1 entry 0)
                 (getf log2 entry 0))
        :return t
        :when (> (getf log1 entry 0)
                 (getf log2 entry 0))
        :return nil))

(defun day4-parse-event (event)
  (case (read-from-string event)
    (guard (list :event 'guard :id (read-from-string (subseq event 7))))
    (falls (list :event 'sleep))
    (wakes (list :event 'wake-up))))

(defun day4-parse-line (line)
  (let ((index (position #\] line)))
    (append (day4-parse-time line)
            (day4-parse-event (subseq line (+ 2 index))))))

(defun day4-organise-log (raw-data)
  (labels ((rec (data shift start log)
             (if (null data)
                 (nreverse (cons (nreverse shift) log))
                 (let ((event (getf (first data) :event))
                       (minute (getf (first data) :min)))
                   (case event
                     (guard (rec (rest data) (list (getf (first data) :id)) nil (cons (nreverse shift) log)))
                     (wake-up (rec (rest data) (cons (cons start minute) shift) nil log))
                     (sleep (rec (rest data) shift minute log)))))))
    (rec (rest raw-data) (list (getf (first raw-data) :id)) nil nil)))

(defun day4-asleep-counters (log)
  (let ((counters (make-hash-table)))
    (loop :for entry :in log
          :for counter := (gethash (first entry) counters (make-array 60 :initial-element 0))
          :do (loop :for (start . end) :in (rest entry)
                    :do (loop :for index :from start :below end
                              :do (incf (aref counter index))))
          :do (setf (gethash (first entry) counters) counter)
          :finally (return counters))))

(defun day4-prep-logdata ()
  (day4-organise-log
   (loop-line-by-line (puzzlepath "input4.txt")
     :collect (day4-parse-line line) into log
     :finally (return (sort log #'day4-time<)))))

(defun day4 ()
  (let* ((asleep-counters (day4-asleep-counters (day4-prep-logdata)))
         (most-asleep-id (max-key asleep-counters :accessor (lambda (c) (reduce #'+ c))))
         (most-asleep-minute (max-index (gethash most-asleep-id asleep-counters)))
         (most-freq-asleep-id (max-key asleep-counters :accessor (lambda (c) (reduce #'max c))))
         (most-freq-asleep-minute (max-index (gethash most-freq-asleep-id asleep-counters))))
    (format t "The most asleep guard is #~a and it's most asleep during minute ~a -> Answer: ~a~%"
            most-asleep-id most-asleep-minute (* most-asleep-id most-asleep-minute))
    (format t "The most frequently asleep guard is #~a and it's most asleep during minute ~a -> Answer: ~a~%"
            most-freq-asleep-id most-freq-asleep-minute (* most-freq-asleep-id most-freq-asleep-minute))))

