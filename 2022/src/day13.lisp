(defun convert-to-list (line)
  (let ((rst))
    (setf rst (substitute #\Space #\, line))
    (setf rst (substitute #\( #\[ rst))
    (setf rst (substitute #\) #\] rst))
    (read-from-string rst)))

(defparameter *input-lists*
  (with-open-file (in "../data/day13.txt")
    (loop for line = (read-line in nil)
          while line
          when (> (length line) 0)
            collect (convert-to-list line))))

(defparameter *input-pairs*
  (loop for el on *input-lists*
      if (and (>= (length el) 2)
              (= (mod (length el) 2) 0))
        collect (subseq el 0 2)))

(defun rvalid (left right)
  (cond
    ((and (numberp left) (numberp right) (< left right)) 1)
    ((and (numberp left) (numberp right) (> left right)) 0)
    ((and (numberp left) (numberp right) (= left right)) -1)
    ((and (numberp left) (listp right)) (rvalid (list left) right))
    ((and (listp left) (numberp right)) (rvalid left (list right)))
    ((and (null left) (listp right)) 1)
    ((and (listp left) (null right)) 0)
    ((and (null left) (null right)) -1)
    (t (let ((rst (rvalid (first left) (first right))))
         (if (= rst -1)
             (rvalid (rest left) (rest right))
             rst)))))

(defun valid-pair (l1 l2)
  (= (rvalid l1 l2) 1))

(defun validate-pairs ()
  (loop for pair in *input-pairs*
        for i from 1
        if (valid-pair (first pair) (second pair))
          summing i into validated
        end
        finally (return validated)))

(defun sorted-divider-packet-indices ()
  (let* ((packets (append *input-lists* '(((2)) ((6)))))
         (sorted (sort packets #'valid-pair)))
    (* (1+ (position '((2)) sorted :test 'equal))
       (1+ (position '((6)) sorted :test 'equal)))))

(format t "P1: ~a; P2: ~a~%" (validate-pairs) (sorted-divider-packet-indices))
