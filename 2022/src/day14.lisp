(ql:quickload "cl-utilities")

(defun lastcar (lst) (first (last lst)))

(defun x (pt) (first pt))
(defun y (pt) (cdr pt))
(defun subt (pt other) (cons (- (x other) (x pt)) (- (y other) (y pt))))
(defun point (lst) (cons (first lst) (lastcar lst)))

(defconstant +air+ 0)
(defconstant +rock+ 1)
(defconstant +sand+ 2)

(defun parse-line-points (line)
  (mapcar #'(lambda (str)
              (point (mapcar #'parse-integer (cl-utilities:split-sequence #\, str))))
          (remove-if #'(lambda (str) (equal str "->"))
                     (cl-utilities:split-sequence #\SPACE line))))

(defparameter *line-points*
  (with-open-file (in "../data/day14.txt")
    (loop for line = (read-line in nil)
          while line
          when (> (length line) 0)
            collect (parse-line-points line))))

(defparameter *map* (make-array '(200 600)))

(defun expanded (source dest)
  (let* ((delta (subt source dest)) (x-delta (x delta)) (y-delta (y delta)))
    (cond ((> x-delta 0) (loop for i from 0 to x-delta
                               collect (cons (+ (x source) i) (y source))))
          ((< x-delta 0) (loop for i from 0 downto x-delta
                               collect (cons (+ (x source) i) (y source))))
          ((> y-delta 0) (loop for j from 0 to y-delta
                               collect (cons (x source) (+ (y source) j))))
          ((< y-delta 0) (loop for j from 0 downto y-delta
                               collect (cons (x source) (+ (y source) j)))))))

(defun expand-points (points)
  (loop for el on points
        if (> (length el) 1)
        collect (apply #'expanded (subseq el 0 2))))
