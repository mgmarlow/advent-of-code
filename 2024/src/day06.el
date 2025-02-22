;; -*- lexical-binding: t; -*-

(defun read-lines (file)
  "Return the contents of FILE as a list of strings"
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

(defun expand-file-contents (file)
  "Return the contents of FILE as a list of lists of string characters"
  (mapcar
   (lambda (line)
     (mapcar #'char-to-string (string-to-list line)))
   (read-lines file)))

(defun read-file-into-grid (file)
  "Read the contents of FILE into a vector of vectors."
  (let* ((file-contents (expand-file-contents file))
         (rows (length file-contents))
         (grid (make-vector rows nil)))
    (dotimes (i rows)
      (aset grid i (make-vector (length (nth i file-contents)) nil)))
    (dotimes (i rows)
      (dotimes (c (length (nth i file-contents)))
        (aset (aref grid i) c (nth c (nth i file-contents)))))
    grid))

(defvar grid (read-file-into-grid "../data/day06.txt"))

;; TODO it'd be easier if these grid functions took a dotted pair
;; since that's how navigate works.
(defun grid-get (grid row col)
  "Return element of 2D GRID at place ROW, COL."
  (aref (aref grid row) col))

(defun within-bounds? (grid row col)
  (and
   (>= row 0)
   (>= col 0)
   (< row (length grid))
   (< col (length (aref grid 0)))))

(defun empty? (grid row col)
  (not (string= "#" (grid-get grid row col))))

(defvar guard-direction-alist '(("^" . (-1 . 0)) (">" . (0 . 1)) ("v" . (1 . 0)) ("<" . (0 . -1))))

(defvar turn-alist '(("^" . ">") (">" . "v") ("v" . "<") ("<" . "^")))

(defun starting-position (grid)
  (let ((pos))
    (dotimes (row (length grid))
      (dotimes (col (length (aref grid row)))
        (when (seq-contains '("^" ">" "v" "<") (grid-get grid row col))
          (setq pos (cons row col)))))
    (unless pos
      (error "Couldn't find starting position."))
    pos))

(defun add-pos (pos1 pos2)
  (cons
   (+ (car pos1) (car pos2))
   (+ (cdr pos1) (cdr pos2))))

;; Part one
(defun navigate (grid)
  (let* ((positions ())
         (pos (starting-position grid))
         (dir (grid-get grid (car pos) (cdr pos))))
    (while (within-bounds? grid (car pos) (cdr pos))
      (push pos positions)
      (let ((next-pos (add-pos pos (alist-get dir guard-direction-alist nil nil #'string=))))
        (if (and (within-bounds? grid (car next-pos) (cdr next-pos))
                 (not (empty? grid (car next-pos) (cdr next-pos))))
            (setq dir (alist-get dir turn-alist nil nil #'string=))
          (setq pos next-pos))))
    (seq-uniq positions)))

(length (navigate grid))

;; Skipped part two because it felt kind of tedious
