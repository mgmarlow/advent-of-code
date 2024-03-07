(ql:quickload "cl-utilities")

(defun flatten (l)
  "Utility function for flattening a list of lists."
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (loop for a in l appending (flatten a)))))

(defun sign (x)
  (cond ((> x 0) 1)
        ((< x 0) -1)
        (t 0)))

(defclass point ()
  ((x
    :initarg :x
    :initform 0
    :accessor x)
   (y
    :initarg :y
    :initform 0
    :accessor y)))

(defun new-point (x y)
  (make-instance 'point :x x :y y))

(defmethod point-zerop ((p point))
  (and (= (x p) 0) (= (y p) 0)))

(defmethod subtract ((a point) (b point))
  (make-instance 'point
                 :x (- (x a) (x b))
                 :y (- (y a) (y b))))

(defmethod move ((p point) (dir point))
  (setf (x p) (+ (x p) (x dir)))
  (setf (y p) (+ (y p) (y dir))))

(defun create-points (dir n)
  (loop for i from 0 to (1- n) collect (new-point (x dir) (y dir))))

(defun expand-command (line)
  (let* ((parsed (cl-utilities:split-sequence #\SPACE line))
         (dir (first parsed))
         (n (parse-integer (second parsed))))
    (cond
      ((string= dir "R") (create-points (new-point 1 0) n))
      ((string= dir "L") (create-points (new-point -1 0) n))
      ((string= dir "U") (create-points (new-point 0 -1) n))
      ((string= dir "D") (create-points (new-point 0 1) n))
      (t (error "invalid input when parsing command")))))

(defparameter *commands*
  (with-open-file (in "../data/day09.txt")
    (flatten
     (loop for line = (read-line in nil)
           while line
           when (> (length line) 0)
             collect (expand-command line)))))

(defun diagonalp (delta)
  (and (> (abs (x delta)) 0) (> (abs (y delta)) 0)))

(defun find-next-tail-move (rhead rtail)
  (let ((delta (subtract rhead rtail)))
    (cond
      ((and (diagonalp delta)
            (or (> (abs (x delta)) 1) (> (abs (y delta)) 1)))
       (new-point (sign (x delta)) (sign (y delta))))
      ((> (abs (x delta)) 1) (new-point (sign (x delta)) 0))
      ((> (abs (y delta)) 1) (new-point 0 (sign (y delta))))
      (t (new-point 0 0)))))

(defun count-keys (hsh)
  (loop for i from 0
        for k being the hash-keys in hsh
        counting i into total
        finally (return total)))

(defun find-visited-tail-positions (snake-length)
  (let* ((positions (make-hash-table :test 'equal))
         (snake (loop for i from 0 to (1- snake-length) collect (new-point 0 0)))
         (tail (nth (1- snake-length) snake)))
    (loop for command in *commands* do
      (move (first snake) command)
      (loop for i from 1 to (1- (length snake)) do
        (let* ((head (nth (1- i) snake))
               (tail (nth i snake))
               (tail-move (find-next-tail-move head tail)))
          (move tail tail-move)))
      (setf (gethash (format nil "~a,~a" (x tail) (y tail)) positions) 1))
    (count-keys positions)))

(format t "P1: ~a; P2: ~a~%" (find-visited-tail-positions 2) (find-visited-tail-positions 10))
