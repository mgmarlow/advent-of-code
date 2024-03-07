;;; aoc.el --- Advent of code 2023 helpers           -*- lexical-binding: t; -*-

;;; Commentary:

;; Helpers for working with 2D arrays and other common tasks in Advent
;; of Code problems.

;;; Code:

(defun aoc-file->list (file)
  "Read FILE into a list of strings."
  (with-temp-buffer
    (insert-file-contents file)
    (string-split (buffer-string) "\n" t)))

(defun aoc-file->vec (file)
  "Read FILE into a 2D vector."
  (let* ((raw (aoc-file->list file))
         (rst (make-vector (length raw) nil)))
    (dotimes (ridx (length raw))
      (let* ((row (elt raw ridx))
             (vec (make-vector (length row) nil)))
        (message row)
        (dotimes (cidx (length row))
          (aset vec cidx (char-to-string (aref row cidx))))
        (aset rst ridx vec)))
    rst))

(defun aoc-elt2d (vec x y)
  "Return the value from a 2D VEC at point X, Y."
  (aref (aref vec y) x))

(provide 'aoc)
;;; aoc.el ends here
