;; -*- lexical-binding: t; -*-

(defun read-file-as-string (file)
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defvar data (read-file-as-string "../data/day03.txt"))

(defvar exp-regexp "\\(mul(\\([0-9]+\\),\\([0-9]+\\))\\|do()\\|don't()\\)")

;; Parts one and two, comment out do/dont handling for part one.
(defun parse (str)
  (save-match-data
    (let ((sum 0)
          (do-inc t))
      (while (string-match exp-regexp str (match-end 0))
        (cond
         ((string= "do()" (match-string 1 str))
          (setq do-inc t))
         ((string= "don't()" (match-string 1 str))
          (setq do-inc nil))
         ((and do-inc (string-prefix-p "mul" (match-string 1 str)))
          (setq sum (+ sum (* (string-to-number (match-string 2 str))
                              (string-to-number (match-string 3 str))))))))
    sum)))

(parse data)
