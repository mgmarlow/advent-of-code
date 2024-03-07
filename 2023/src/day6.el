;;; day6.el --- Wait For It -*- lexical-binding: t; -*-

;;; Code:

(defun distance (held max)
  (* (- max held) held))

(defun expand (n)
  (let (rst)
    (dotimes (i (+ n 1))
      (push i rst))
    rst))

(defun distances (time)
  (mapcar (lambda (i) (distance i time)) (expand time)))

(defun distance-prs (time record)
  (seq-filter (lambda (d) (> d record)) (distances time)))

(defun num-prs (time record)
  (length (distance-prs time record)))

;; Part 1
(* (num-prs 42 284)
   (num-prs 68 1005)
   (num-prs 69 1122)
   (num-prs 85 1341))

;; Part 2
(num-prs 42686985 284100511221341)

;;; day6.el ends here
