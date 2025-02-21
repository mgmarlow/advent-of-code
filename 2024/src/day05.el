;; -*- lexical-binding: t; -*-

(defun read-lines (file)
  "Return the contents of FILE as a list of strings"
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

(defvar data (read-lines "../data/day05.txt"))

(defvar rules (seq-filter (lambda (line) (string-match-p "|" line)) data))

(defvar pages
  (mapcar
   (lambda (line) (mapcar #'string-to-number (string-split line ",")))
   (seq-filter (lambda (line) (string-match-p "," line)) data)))

(defvar lookahead-alist
  (seq-reduce
   (lambda (acc cur)
     (let ((left (string-to-number (car (string-split cur "|"))))
           (right (string-to-number (cadr (string-split cur "|")))))
       (push (cons left (append (list right) (alist-get left acc ()))) acc)
       acc))
   rules
   ()))

(defun middle-element (lst)
  (nth (/ (length lst) 2) lst))

(defun correctly-ordered? (page)
  (if (or (null page) (eq (length page) 1))
      t
    (if (seq-some
         (lambda (el)
           (seq-contains (alist-get el lookahead-alist) (car page)))
         (cdr page))
        nil
      (correctly-ordered? (cdr page)))))

;; Part one
(defun sum-correctly-ordered ()
  (cl-loop
   for page in pages
   when (correctly-ordered? page)
   sum (middle-element page)))

(sum-correctly-ordered)

;; Part two
(defun sorted (page)
  (seq-sort (lambda (a b)
              (seq-contains (alist-get a lookahead-alist) b))
            page))

(defun sum-sorted ()
  (cl-loop
   for page in pages
   unless (correctly-ordered? page)
   sum (middle-element (sorted page))))

(sum-sorted)
