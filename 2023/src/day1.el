;;; day1.el --- Trebuchet?! -*- lexical-binding: t; -*-

;;; Code:

(defun read-lines (file)
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

(defvar data (read-lines "../data/day1.txt"))

(defvar first-digit-regexp "\\([[:digit:]]\\)")

(defvar english-alist '(("one" . "1")
                        ("two" . "2")
                        ("three" . "3")
                        ("four" . "4")
                        ("five" . "5")
                        ("six" . "6")
                        ("seven" . "7")
                        ("eight" . "8")
                        ("nine" . "9")))

(defun maybe-backwards-english->digit (english)
  (or (alist-get english english-alist nil nil #'string=)
      (alist-get (reverse english) english-alist nil nil #'string=)))

(defun english->digit (str)
  (if (= (length str) 1)
      str
    (maybe-backwards-english->digit str)))

(defun first-digit (line)
  (string-match first-digit-regexp line)
  (english->digit (match-string 1 line)))

(defun calibration-number (line)
  (string-to-number (concat (first-digit line) (first-digit (reverse line)))))

(defun calibration-sum (lines)
  (if (null lines)
      0
    (+ (calibration-number (car lines)) (calibration-sum (cdr lines)))))

;; Part 1
;; Too much recursion for default Emacs Lisp eval depth :p
(let ((max-lisp-eval-depth 5000)
      (first-digit-regexp "\\([[:digit:]]\\)"))
  (calibration-sum data))

(defvar english-digits '("one" "two" "three" "four" "five" "six" "seven" "eight" "nine"))

;; Part 2
(let ((max-lisp-eval-depth 5000)
      (first-digit-regexp (concat "\\(\\([[:digit:]]\\)\\|"
                                  (regexp-opt
                                   (append english-digits (mapcar #'reverse english-digits))
                                   t)
                                  "\\)")))
  (calibration-sum data))

;;; day1.el ends here
