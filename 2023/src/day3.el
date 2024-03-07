;;; day3.el --- Gear Ratios -*- lexical-binding: t; -*-

;;; Code:

(defun read-lines (file)
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

(defvar data (read-lines "../data/day3.txt"))

(defvar data-array (make-vector (length data) 0))
(dotimes (i (length (car data)))
  (setf (aref data-array i) (make-vector (length (car data)) 0)))

;; Read raw input into data-array
(dotimes (rowi (length data))
  (let ((row (elt data rowi)))
    (dotimes (coli (length row))
      (let ((col (char-to-string (aref row coli))))
        (setf (aref (aref data-array rowi) coli) col)))))

;; From s.el
(defun s-numeric? (s)
  "Is S a number?"
  (declare (pure t) (side-effect-free t))
  (not (null (string-match-p "^[0-9]+$" s))))

(defun s-symbolic? (s)
  "Is S a symbol?"
  (declare (pure t) (side-effect-free t))
  (not (null (string-match-p "^[\$\+\#\*\/\@\=\%\&\-]+$" s))))

(defun within-bounds? (i j vec)
  (and (>= i 0)
       (>= j 0)
       (< i (length (aref vec 0)))
       (< j (length vec))))

(defun coords-to-check (i j)
  `((,(- i 1) . ,(- j 1))
    (,(- i 1) . ,j)
    (,(- i 1) . ,(+ j 1))
    (,i . ,(- j 1))
    (,i . ,(+ j 1))
    (,(+ i 1) . ,(- j 1))
    (,(+ i 1) . ,j)
    (,(+ i 1) . ,(+ j 1))))

(defun el-get (x y)
  (aref (aref data-array y) x))

(defun range-borders-symbol? (startx endx y)
  (let (any-border-p)
    (dotimes (xi (+ (- endx startx) 1))
      (dolist (coord (coords-to-check (+ startx xi) y))
        (let ((coord-x (car coord))
              (coord-y (cdr coord)))
          (when (and (within-bounds? coord-x coord-y data-array)
                     (s-symbolic? (el-get coord-x coord-y)))
            (setq any-border-p t)))))
    any-border-p))

;; Part 1
(defun part-numbers ()
  (let (rst)
    (dotimes (rowi (length data))
      (let ((row (elt data rowi)) (i 0))
        (while (string-match "[0-9]+" row i)
          (let ((starti (match-beginning 0))
                (endi (- (match-end 0) 1)))
            (when (range-borders-symbol? starti endi rowi)
              (push (string-to-number (match-string 0 row)) rst)))
          (setq i (match-end 0)))))
    rst))

(defun sum (lst) (apply '+ lst))

(sum (part-numbers))

;;; day3.el ends here
