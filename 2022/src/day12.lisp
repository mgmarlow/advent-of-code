(defconstant +start+ #\S)
(defconstant +end+ #\E)

(defun elevation (char)
  (cond
    ((char= char #\S) 0)
    ((char= char #\E) 25)
    (t (position char "abcdefghijklmnopqrstuvwxyz"))))

(defclass point ()
  ((x :initform 0 :initarg :x :accessor x)
   (y :initform 0 :initarg :y :accessor y)))

(defmethod pointfmt ((p point))
  (format nil "~a,~a" (x p) (y p)))

(defmethod point-add ((source point) (other point))
  (make-instance 'point
                 :x (+ (x source) (x other))
                 :y (+ (y source) (y other))))

(defclass heightmap ()
  ((contents
    :initarg :contents
    :accessor contents)
   (width
    :initarg :width
    :accessor width)
   (height
    :initarg :height
    :accessor height)
   (startpoint
    :initarg :startpoint
    :accessor startpoint)
   (endpoint
    :initarg :endpoint
    :accessor endpoint)))

(defmethod at ((m heightmap) p)
  (nth (x p) (nth (y p) (contents m))))

;; 0,0 = top-left
(defun find-point (map-contents item)
  (loop for y from 0
        for row in map-contents do
          (loop for x from 0
                for col in row do
                  (when (char= col item)
                    (return-from find-point
                      (make-instance 'point :x x :y y))))))

(defun expand-string (str)
  (loop for i across str collect i))

(defun new-map ()
  (let ((contents
          (with-open-file (in "../data/day12.txt")
            (loop for line = (read-line in nil)
                  while line
                  when (> (length line) 0)
                    collect (expand-string line)))))
    (make-instance 'heightmap
                   :contents contents
                   :height (length contents)
                   ;; Assuming every row is the same length
                   :width (length (first contents))
                   :startpoint (find-point contents +start+)
                   :endpoint (find-point contents +end+))))

(defparameter *heightmap* (new-map))

(defmethod outside-map ((m heightmap) dest)
  (or
   (< (x dest) 0)
   (< (y dest) 0)
   (>= (x dest) (width m))
   (>= (y dest) (height m))))

(defmethod valid-move ((m heightmap) source dest)
  (cond
    ((outside-map m dest) nil)
    ((<= (- (elevation (at m dest))
            (elevation (at m source)))
         1)
     t)))

(defmethod moves-available-at-point ((m heightmap) source)
  (flet ((valid-destination (dest) (valid-move m source dest)))
    (remove-if-not #'valid-destination
                   (list
                    (point-add source (make-instance 'point :x 1 :y 0))
                    (point-add source (make-instance 'point :x -1 :y 0))
                    (point-add source (make-instance 'point :x 0 :y 1))
                    (point-add source (make-instance 'point :x 0 :y -1))))))

;; https://3e8.org/pub/scheme/doc/lisp-pointers/v4i4/p2-norvig.pdf
(defun make-queue () (list nil))
(defun dequeue (q) (pop (car q)))
(defun enqueue (q item)
  (setf (car q) (nconc (car q) (list item))))

(defun lastcar (lst) (first (last lst)))

(defun bfs (graph start end)
  (let ((queue (make-queue))
        (visited (make-hash-table :test 'equal)))
    (enqueue queue (list start))
    (setf (gethash (pointfmt start) visited) t)
    (loop while (> (length queue) 0) do
      (let* ((path (dequeue queue)) (current (lastcar path)))
        (when (null current)
          (return-from bfs nil))
        (when (equal (at graph current) end)
          (return-from bfs path))
        (loop for edge in (moves-available-at-point graph current) do
          (unless (gethash (pointfmt edge) visited)
            (setf (gethash (pointfmt edge) visited) t)
            (enqueue queue (append path (list edge)))))))))

(defmethod shortest-path-from-start ((m heightmap))
  (1- (length (bfs m (startpoint m) #\E))))

(defmethod possible-starts ((m heightmap))
  (let ((starting-positions))
    (loop for y from 0
          for row in (contents m) do
            (loop for x from 0
                  for col in row do
                    (when (= (elevation col) 0)
                      (push (make-instance 'point :x x :y y) starting-positions))))
    (nreverse starting-positions)))

(defmethod shortest-path-from-any-start ((m heightmap))
  (let ((starting-positions (possible-starts m)))
    (loop for start in starting-positions
          for path = (bfs m start #\E)
          when (not (null path))
          minimizing (length path) into min
          finally (return (1- min)))))

(format t "P1:~a; P2:~a~%" (shortest-path-from-start *heightmap*) (shortest-path-from-any-start *heightmap*))
