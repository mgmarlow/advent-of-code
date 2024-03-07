;;; day4.el --- Scratchcards -*- lexical-binding: t; -*-

;;; Code:

(defun read-lines (file)
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

(defvar data (read-lines "../data/day4.txt"))

(defun retrieve-numbers (str)
  (let (rst (i 0))
    (while (string-match "[[:digit:]]+" str i)
      (push (string-to-number (match-string 0 str)) rst)
      (setq i (match-end 0)))
    rst))

(defun parse-card (line)
  (string-match "Card[[:space:]]+\\([[:digit:]]*\\): " line)
  (cons
   (string-to-number (match-string 1 line))
   (mapcar #'retrieve-numbers (string-split (substring line (match-end 0)) "|"))))

(defvar cards (mapcar #'parse-card data))

(defun sum (lst) (apply #'+ lst))

(defun winning-numbers (card)
  (seq-intersection (car (cdr card)) (cadr (cdr card))))

(defun point-value (winning-nums)
  (if (= (length winning-nums) 0)
      0
    (expt 2 (- (length winning-nums) 1))))

;; Part 1
(sum (mapcar #'point-value
             (mapcar #'winning-numbers cards)))

;; Part 2
(defun range (start n upper-bound)
  (let (rst)
    (dotimes (i n)
      (when (<= (+ start 1 i) upper-bound)
        (push (+ start 1 i) rst)))
    rst))

(defun next-cards (card)
  (let ((winners (winning-numbers card)))
    (range (car card) (length winners) 220)))

(defun score (lst)
  (if (null lst)
      1
    (+ (score (next-cards (assoc (car lst) cards)))
       (score (cdr lst)))))

(- (score (mapcar #'car cards)) 1)

;;; day4.el ends here
