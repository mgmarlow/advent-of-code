;; -*- lexical-binding: t; -*-

(defun read-lines (file)
  "Return the contents of FILE as a list of strings"
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

(defvar data
  (mapcar
   (lambda (line)
     (cons
      (string-to-number (car (string-split line ": ")))
      (mapcar #'string-to-number (string-split (cadr (string-split line ": ")) " "))))
   (read-lines "../data/day07.txt")))

(defvar operators '(* +))

(defvar op-perm-alist ())

;; Little bit of memoization to help performance
(defun generate-op-permutations (n)
  (if (alist-get n op-perm-alist)
      (alist-get n op-perm-alist)
    (if (<= n 0)
        '(())
      (let (result)
        (dolist (op operators)
          (dolist (perm (generate-op-permutations (1- n)))
            (push (cons op perm) result)))
        (reverse result)
        (push (cons n result) op-perm-alist)
        result))))

(defun weave (operands operators)
  (if (null operators)
      (car operands)
    (list (car operators) (car operands) (weave (cdr operands) (cdr operators)))))

(defun possibilities (operands)
  (let (result
        (all-ops (generate-op-permutations (1- (length operands)))))
    (dolist (ops all-ops)
      ;; Reverse because of left-right precedence
      (push (weave (reverse operands) ops) result))
    result))

(defun solvable? (answer operands)
  (seq-some
   (lambda (possible-answer) (= possible-answer answer))
   (mapcar #'eval (possibilities operands))))

(defun solvable-sum ()
  (cl-loop
   for equation in data
   when (solvable? (car equation) (cdr equation))
   sum (car equation)))

;; Part one
(solvable-sum)

;; Part two
(defun my-concat (a b)
  (string-to-number
   (concat (number-to-string a) (number-to-string b))))

;; Tried swapping to a mathematical calculation because performance is
;; bad, as well adding in some memoization for operator permutations.
;; Neither worked. I suspect the real performance problem is the
;; (mapcar #'eval ...) that all of the generated permutations are
;; being piped into. Need to revisit this one. Approach is valid, but
;; performance isn't good enough.

;; (defun my-concat (a b)
;;   (+ (* a (expt 10 (1+ (floor (log b 10))))) b))

(setq op-perm-alist ())
(setq operators '(* + my-concat))

(solvable-sum)
