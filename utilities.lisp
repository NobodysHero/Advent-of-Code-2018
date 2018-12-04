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
