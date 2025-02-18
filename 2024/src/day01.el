;; -*- lexical-binding: t; -*-

(defun read-lines (file)
  "Return the contents of FILE as a list of strings"
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

(defvar data (read-lines "../data/day01.txt"))

(defvar list1
  (mapcar (lambda (line) (string-to-number (car (split-string line "   "))))
          data))

(defvar list2
  (mapcar (lambda (line) (string-to-number (cadr (split-string line "   "))))
          data))

(defun distance (a b)
  (abs (- a b)))

;; Part one
(seq-reduce
 (lambda (acc cur)
   (+ acc (distance (car cur) (cadr cur))))
 (cl-mapcar #'list (sort list1) (sort list2))
 0)

;; Part two
(defvar frequency-alist
  (seq-reduce
   (lambda (acc cur)
     (push (cons cur (+ 1 (alist-get cur acc 0))) acc))
   list2
   ()))

(defun similarity (a)
  (* a (alist-get a frequency-alist 0)))

(seq-reduce
 (lambda (acc cur)
   (+ acc (similarity cur)))
 list1
 0)
