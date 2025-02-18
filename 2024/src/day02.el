;; -*- lexical-binding: t; -*-

(defun read-lines (file)
  "Return the contents of FILE as a list of strings"
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

(defvar data
  (mapcar
   (lambda (line) (mapcar #'string-to-number (split-string line " ")))
   (read-lines "../data/day02.txt")))

(defun differences (report)
  (cl-loop for i from 1 to (- (length report) 1)
           for prev = (- i 1)
           collect (- (nth prev report) (nth i report))))

(defun differing-signs (report)
  (and (seq-some (lambda (val) (< 0 val)) report)
       (seq-some (lambda (val) (> 0 val)) report)))

(defun within-range (report)
  (not (seq-some (lambda (val) (or (< (abs val) 1) (> (abs val) 3))) report)))

;; Part one
(defun safe (report)
  (let ((diffs (differences report)))
   (and (within-range diffs) (not (differing-signs diffs)))))

(length (seq-filter #'safe data))

;; Part two
(defun sans (lst index)
  "Remove element at INDEX from LST."
  (if (or (null lst) (< index 0))
      lst
    (if (= index 0)
        (cdr lst)
      (cons (car lst) (sans (cdr lst) (- index 1))))))

(defun range (n)
  (cl-loop for i from 0 to n
           collect i))

(defun safe-with-allowance (report)
  (or (safe report)
      (seq-some (lambda (i) (safe (sans report i))) (range (- (length report) 1)))))

(length (seq-filter #'safe-with-allowance data))
