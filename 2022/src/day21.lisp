(ql:quickload "cl-utilities")

(defun lastcar (lst) (first (last lst)))

(defun parse-line (line)
  (let ((parsed (cl-utilities:split-sequence #\: line)))
    (cons (first parsed) (subseq (lastcar parsed) 1))))

(defparameter *input*
  (with-open-file (in "../data/day21.txt")
    (loop for line = (read-line in nil)
          while line
          collect (parse-line line))))

(defparameter *commands-hash*
  (let ((hsh (make-hash-table :test 'equal)))
    (loop for pair in *input* do
      (let ((key (car pair)) (value (cdr pair)))
        (setf (gethash key hsh) value)))
    hsh))

(defun evaluate (expression &optional humn-value)
  (handler-case (parse-integer expression)
    (parse-error ()
      (let ((split-expression (cl-utilities:split-sequence #\Space expression)))
        (cond ((and (equal expression "humn") (not (null humn-value)))
               humn-value)
              ((= (length split-expression) 1)
               (evaluate (gethash expression *commands-hash*) humn-value))
              ((equal "+" (second split-expression))
               (+ (evaluate (first split-expression) humn-value)
                  (evaluate (lastcar split-expression) humn-value)))
              ((equal "-" (second split-expression))
               (- (evaluate (first split-expression) humn-value)
                  (evaluate (lastcar split-expression) humn-value)))
              ((equal "*" (second split-expression))
               (* (evaluate (first split-expression) humn-value)
                  (evaluate (lastcar split-expression) humn-value)))
              ((equal "/" (second split-expression))
               (/ (evaluate (first split-expression) humn-value)
                  (evaluate (lastcar split-expression) humn-value))))))))

;; Take root values, binary search until equal.
(defun binary-search-humn ()
  (let* ((r (evaluate "vlzj")) (min 0) (max most-positive-fixnum))
    (loop for i = (floor (/ (+ max min) 2))
          for l = (evaluate "rnsd" i)
          until (= l r)
          do (if (> l r) (setf min i) (setf max i))
          finally (return i))))

(format t "P1: ~a; P2: ~a~%" (evaluate "root") (binary-search-humn))
