;;;; day20.lisp

(in-package :advent-of-code-2018)

(defun day20-parse-file (&optional (file (puzzlefile 20)))
  (with-open-file (in file)
    (labels ((parse-group ()
               (loop
                 :with result := nil
                 :with option := nil
                 :with seq := nil
                 :for char := (read-char in)
                 :do (case char
                       ((#\N #\S #\W #\E) (push char seq))
                       (#\(
                        (unless (null seq)
                          (push (coerce (nreverse seq) 'string) option)
                          (setf seq nil))
                        (push (parse-group) option))
                       ((#\) #\$)
                        (unless (null seq)
                          (push (coerce (nreverse seq) 'string) option))
                        (when (null option)
                          (push "" option))
                        (push (nreverse option) result)
                        (return (nreverse result)))
                       (#\|
                        (unless (null seq)
                          (push (coerce (nreverse seq) 'string) option)
                          (setf seq nil))
                        (when (null option)
                          (push "" option))
                        (push (nreverse option) result)
                        (setf option nil))))))
      (read-char in)
      (first (parse-group)))))

(defun day20-build-graph (node)
  (let ((edges (make-hash-table)))
    (labels ((move (current char)
               (let* ((shift
                        (case char
                          (#\N #C( 0  1))
                          (#\E #C( 1  0))
                          (#\S #C( 0 -1))
                          (#\W #C(-1  0))
                          (t 0)))
                      (next (mapcar (lambda (pos) (+ pos shift)) current)))
                 (loop :for room1 :in current
                       :for room2 :in next
                       :do (pushnew room2 (gethash room1 edges nil))
                       :do (pushnew room1 (gethash room2 edges nil)))
                 next))
             (follow (current path)
               (loop :for c :across path
                     :for new := (move current c) :then (move new c)
                     :finally (return new)))
             (explore-seq (current node)
               (loop :for path :in node
                     :when (stringp path) :do (setf current (follow current path))
                     :else :do (explore-options current path)))
             (explore-options (current options)
               (loop :for option :in options
                     :when (stringp option) :nconc (follow current option)
                     :else :nconc (explore-seq current option))))
      (explore-seq (list #C(0 0)) node))
    edges))

(defun day20 ()
  (let ((edges (day20-build-graph (day20-parse-file))))
    (loop :with open := (make-instance 'cl-heap:fibonacci-heap :key #'car)
          :with closed := (make-hash-table)
          :for (depth . node) := (cons 0 #C(0 0)) :then (cl-heap:pop-heap open)
          :count (>= depth 1000) :into far-away
          :do (setf (gethash node closed) t)
          :do (dolist (new (gethash node edges))
                (unless (gethash new closed nil)
                  (cl-heap:add-to-heap open (cons (1+ depth) new))))
          :while (> (cl-heap:heap-size open) 0)
          :finally (format t "The furthest room is ~a doors away and there a total of ~a rooms 1000 doors or more away.~%" depth far-away))))
