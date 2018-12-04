;;;; advent-of-code-2018.lisp

(in-package #:advent-of-code-2018)

(defconstant +path+ "D:/Daten/lisp/advent-of-code-2018/inputs/")

(defun puzzlepath (file)
  (concatenate 'string +path+ file))

(defmacro loop-line-by-line (file &body body)
  (let ((in (gensym)))
    `(with-open-file (,in ,file)
       (when ,in
         (loop for line = (read-line ,in nil)
               while line
               ,@body)))))

(defun read-puzzlefile (file)
  (loop-line-by-line (puzzlepath file)
    collect line)) 

(defun split-seq (seq denom)
  (labels
      ((rec (start accum)
         (let ((index (position denom seq :start start)))
           (if index
               (rec (+ 1 index) (cons (subseq seq start index) accum))
               (cons (subseq seq start) accum)))))
    (nreverse (rec 0 nil)))) 

(defun hash-keys (hashtable)
  (loop :for key :being :the :hash-keys :of hashtable
        :collect key))

(defun id (x) x)

(defun max-key (hashtable &key (test #'>) (accessor #'id))
  (let ((keys (hash-keys hashtable)))
    (loop
      :with max-key := (first keys)
      :with max-val := (gethash max-key hashtable)
      :for key :in (rest keys)
      :for val := (gethash key hashtable)
      :when (funcall test
                     (funcall accessor val)
                     (funcall accessor max-val))
      :do (setf max-key key max-val val)
      :finally (return (values max-key max-val)))))

(defun max-index (vector)
  (loop
    :with max-ind := 0
    :with max-val := (aref vector 0)
    :for index :from 1 :below (length vector)
    :for value := (aref vector index)
    :when (> value max-val) :do (setf max-ind index max-val value)
    :finally (return (values max-ind max-val))))

;;; day 4
(defun day4-parse-time (time)
  ;;ignore the year
  (list :month (parse-integer (subseq time 5 7))
        :day   (parse-integer (subseq time 8 10))
        :hour  (parse-integer (subseq time 11 13))
        :min   (parse-integer (subseq time 14 16))))

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
    (append (day4-parse-time (subseq line 1 index))
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

;;; day 3
(defun day3-parse-line (line)
  ;; example "#1 @ 896,863: 29x19"
  (let ((index1 (position #\@ line))
        (index2 (position #\, line))
        (index3 (position #\: line))
        (index4 (position #\x line)))
    (list :id     (parse-integer (subseq line 1 (- index1 1)))
          :xpos   (parse-integer (subseq line (+ index1 2) index2))
          :ypos   (parse-integer (subseq line (+ index2 1) index3))
          :width  (parse-integer (subseq line (+ index3 2) index4))
          :height (parse-integer (subseq line (+ index4 1))))))

(defun make-claim (claim board)
  (loop
    :for x :from (getf claim :xpos) :below (+ (getf claim :xpos)
                                              (getf claim :width))
    :do (loop
          :for y :from (getf claim :ypos) :below (+ (getf claim :ypos)
                                                    (getf claim :height))
          :do (push (getf claim :id) (aref board x y)))))

(defun day3 ()
  (let ((board (make-array '(1000 1000) :initial-element nil))
        (ids (make-hash-table :test 'equal)))
    (loop-line-by-line (puzzlepath "input3.txt")
      :for claim := (day3-parse-line line)
      :do (make-claim claim board)
      :do (setf (gethash (getf claim :id) ids) t))
    (loop
      :with overlaps := 0
      :for index :below (reduce #'* (array-dimensions board))
      :for entries := (row-major-aref board index)
      :when (>= (length entries) 2)
      :do (incf overlaps) :and
      :do (dolist (id entries) (remhash id ids))
      :finally
      (format t "There are ~a square inches of fabric within two or more claims.~%Id ~a has no overlaps at all." overlaps (first (hash-keys ids))))))


;;; day 2
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

;;; day 1
(defun day1 ()
  (let* ((changes (mapcar #'parse-integer (read-puzzlefile "input1.txt")))
         (total (reduce #'+ changes))
         (seen (make-hash-table :size 1024)) ;use hashtable as a set
         (first-repetition
           (loop
             ;loop continuously over the changes
             :for change :in (setf (cdr (last changes)) changes)
             :sum change :into freq
             :until (gethash freq seen) ;check the set
             :do (setf (gethash freq seen) t)
             :finally (return freq))))
    (format t "The resulting frequency is ~a.~%The first frequency reached twice is ~a.~%"
            total first-repetition)))
