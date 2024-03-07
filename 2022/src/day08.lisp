(defparameter *matrix*
  (with-open-file (in "../data/day08.txt")
    (loop for line = (read-line in nil)
          while line
          when (> (length line) 0)
            collect (map 'list #'digit-char-p line))))

(defmacro matrix-get (x y matrix)
  `(nth ,x (nth ,y ,matrix)))

(defmacro foreach-up (x y &body body)
  `(loop for i from (1- ,y) downto 0 do
    (let ((other (matrix-get ,x i *matrix*)))
      ,@body)))

(defmacro foreach-right (x y &body body)
  `(loop for i from (1+ ,x) to (1- (length (first *matrix*))) do
    (let ((other (matrix-get i ,y *matrix*)))
      ,@body)))

(defmacro foreach-down (x y &body body)
  `(loop for i from (1+ ,y) to (1- (length *matrix*)) do
    (let ((other (matrix-get ,x i *matrix*)))
      ,@body)))

(defmacro foreach-left (x y &body body)
  `(loop for i from (1- ,x) downto 0 do
    (let ((other (matrix-get i ,y *matrix*)))
      ,@body)))

(defun top-visiblep (x y item)
  (foreach-up x y (when (>= other item)
                    (return-from top-visiblep nil)))
  t)

(defun right-visiblep (x y item)
  (foreach-right x y (when (>= other item)
                       (return-from right-visiblep nil)))
  t)

(defun bottom-visiblep (x y item)
  (foreach-down x y (when (>= other item)
                      (return-from bottom-visiblep nil)))
  t)

(defun left-visiblep (x y item)
  (foreach-left x y (when (>= other item)
                      (return-from left-visiblep nil)))
  t)

(defun visiblep (x y)
  (let ((item (matrix-get x y *matrix*)))
    (cond
      ((= x 0) t)
      ((= x (1- (length (first *matrix*)))) t)
      ((= y 0) t)
      ((= y (1- (length *matrix*))) t)
      (t  (or (top-visiblep x y item)
              (right-visiblep x y item)
              (bottom-visiblep x y item)
              (left-visiblep x y item))))))

(defun trees-visible ()
  (let ((sum 0))
    (loop for y from 0
          for row in *matrix* do
            (loop for x from 0
                  for col in row
                  when (visiblep x y) do
                    (incf sum)))
    sum))

(defun top-scenic-score (x y item)
  (let ((score 0))
    (foreach-up x y (if (< other item)
                        (incf score)
                        (return (incf score))))
    score))

(defun right-scenic-score (x y item)
  (let ((score 0))
    (foreach-right x y (if (< other item)
                           (incf score)
                           (return (incf score))))
    score))

(defun bottom-scenic-score (x y item)
  (let ((score 0))
    (foreach-down x y (if (< other item)
                          (incf score)
                          (return (incf score))))
    score))

(defun left-scenic-score (x y item)
  (let ((score 0))
    (foreach-left x y (if (< other item)
                          (incf score)
                          (return (incf score))))
    score))

(defun scenic-score (x y)
  (let ((item (matrix-get x y *matrix*)))
    (* (top-scenic-score x y item)
       (right-scenic-score x y item)
       (bottom-scenic-score x y item)
       (left-scenic-score x y item))))

(defun highest-scenic-score ()
  (let ((max-score 0))
    (loop for y from 0
          for row in *matrix* do
            (loop for x from 0
                  for col in row do
                    (let ((score (scenic-score x y)))
                      (when (> score max-score) (setf max-score score)))))
    max-score))

(format t "P1: ~a; P2: ~a~%" (trees-visible) (highest-scenic-score))
