;;; day9.el --- Mirage Maintenance -*- lexical-binding: t; -*-

;;; Code:

(defun read-lines (file)
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

(defun parse-line (line)
  (mapcar #'string-to-number (split-string line " ")))

(defvar data (mapcar #'parse-line (read-lines "../data/day9.txt")))

(defun lst-reduce (fn lst)
  (if (null (cadr lst))
      nil
    (cons (funcall fn (cadr lst) (car lst))
          (lst-reduce fn (cdr lst)))))

(defun analyze (lst)
  (if (seq-every-p #'zerop lst)
      (cons lst nil)
    (cons lst (analyze (lst-reduce #'- lst)))))

(defun lastcar (lst) (car (last lst)))

(defun sum (lst) (apply #'+ lst))

(defun extrapolate (lsts)
  (sum (mapcar #'lastcar lsts)))

(defun solve (lst)
  (extrapolate (analyze lst)))

;; Part 1
(sum (mapcar #'solve data))

;; Part 2
(sum (mapcar #'solve (mapcar #'reverse data)))

;;; day9.el ends here
