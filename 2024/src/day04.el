;; -*- lexical-binding: t; -*-

(defun read-lines (file)
  "Return the contents of FILE as a list of strings"
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

(defvar data
  (mapcar
   (lambda (line) (mapcar #'char-to-string (string-to-list line)))
   (read-lines "../data/day04.txt")))

(defun make-grid ()
  (let* ((rows (length data))
         (grid (make-vector rows nil)))
    (dotimes (i rows)
      (aset grid i (make-vector (length (nth i data)) nil)))
    (dotimes (i rows)
      (dotimes (c (length (nth i data)))
        (aset (aref grid i) c (nth c (nth i data)))))
    grid))

(defvar grid (make-grid))

(defun grid-get (row col)
  (aref (aref grid row) col))

(defvar rays '((-1 -1) (0 -1) (1 -1) (-1 0) (1 0) (-1 1) (0 1) (1 1)))

(defun within-bounds? (row col)
  (and (>= row 0)
       (>= col 0)
       (< row (length grid))
       (< col (length (aref grid 0)))))

(defun peek (letter row col)
  (and
   (within-bounds? row col)
   (string= letter (grid-get row col))))

;; Part one
(defun xmas? (row col dir)
  (and (peek "X" row col)
       (peek "M" (+ row (car dir)) (+ col (cadr dir)))
       (peek "A" (+ row (car dir) (car dir)) (+ col (cadr dir) (cadr dir)))
       (peek "S" (+ row (car dir) (car dir) (car dir)) (+ col (cadr dir) (cadr dir) (cadr dir)))))

(defun search ()
  (let ((sum 0))
    (dotimes (row (length grid))
      (dotimes (col (length (aref grid row)))
        (dolist (ray rays)
          (when (xmas? row col ray)
            (setq sum (+ sum 1))))))
    sum))

(search)

;; Part two
(defun x-mas? (row col)
  (and (peek "A" row col)
       (or
        (and (peek "M" (+ row -1) (+ col -1))
             (peek "S" (+ row 1) (+ col -1))
             (peek "M" (+ row -1) (+ col 1))
             (peek "S" (+ row 1) (+ col 1)))
        (and (peek "M" (+ row -1) (+ col -1))
             (peek "M" (+ row 1) (+ col -1))
             (peek "S" (+ row -1) (+ col 1))
             (peek "S" (+ row 1) (+ col 1)))
        (and (peek "S" (+ row -1) (+ col -1))
             (peek "S" (+ row 1) (+ col -1))
             (peek "M" (+ row -1) (+ col 1))
             (peek "M" (+ row 1) (+ col 1)))
        (and (peek "S" (+ row -1) (+ col -1))
             (peek "M" (+ row 1) (+ col -1))
             (peek "S" (+ row -1) (+ col 1))
             (peek "M" (+ row 1) (+ col 1))))))

(defun search-x ()
  (let ((sum 0))
    (dotimes (row (length grid))
      (dotimes (col (length (aref grid row)))
        (when (x-mas? row col)
          (setq sum (+ sum 1)))))
    sum))

(search-x)

