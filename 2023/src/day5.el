;;; day5.el --- If You Give A Seed A Fertilizer -*- lexical-binding: t; -*-

;;; Code:

(defun read-lines (file)
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

(defvar data (read-lines "../data/day5.txt"))

(defun read-nums (line)
  (let (rst (i 0))
    (while (string-match "[0-9]+" line i)
      (push (string-to-number (match-string 0 line)) rst)
      (setq i (match-end 0)))
    (reverse rst)))

(defvar seeds (read-nums (car data)))

(defun read-maps (lines)
  (let (rst name cur)
    (dolist (line lines)
      (cond
       ((string-match-p "[0-9]" line 0) (push (read-nums line) cur))
       (t (progn
            (when name
              (push (cons name (reverse cur)) rst)
              (setq cur nil)
              (setq name nil))
            (setq name (car (split-string line " ")))))))
    (when name
      (push (cons name (reverse cur)) rst))
    (reverse rst)))

(defvar maps (read-maps (cdr data)))

(defun compute-destination (x map-range)
  (+ (elt map-range 0) (- x (elt map-range 1))))

(defun within-range? (x range length)
  (and (< x (+ range length))
       (>= x range)))

(defun destination (x map)
  (if-let ((matching-range (seq-find
                            (lambda (range)
                              (apply #'within-range? `(,x ,@(cdr range))))
                            (cdr map))))
      (compute-destination x matching-range)
    x))

(defun traverse-maps-dest (seed maps)
  (seq-reduce (lambda (acc map) (destination acc map)) maps seed))

;; Part 1
(apply #'min (mapcar (lambda (seed) (traverse-maps-dest seed maps)) seeds))

;; Part 2
(defun compute-source (x map-range)
  (+ (elt map-range 1) (- x (elt map-range 0))))

(defun source (x map)
  (if-let ((matching-range (seq-find
                            (lambda (range)
                              (apply #'within-range? `(,x ,(car range) ,(caddr range))))
                            (cdr map))))
      (compute-source x matching-range)
    x))

(defun traverse-maps-source (loc maps)
  (seq-reduce (lambda (acc map) (source acc map)) maps loc))

(defun group-by-twos (lst)
  (if (null lst)
      nil
    (cons `(,(car lst) ,(cadr lst)) (group-by-twos (cddr lst)))))

(defvar seeds-in-twos (group-by-twos seeds))

(defun seeds-contains-p (n)
  (seq-find (lambda (seed-range)
              (within-range? n (car seed-range) (cadr seed-range)))
            seeds-in-twos))

(defvar seed-maps (reverse maps))

(let (smallest-loc (i 0))
  (while (null smallest-loc)
    (let ((maybe-seed (traverse-maps-source i seed-maps)))
      (when (seeds-contains-p maybe-seed)
        (setq smallest-loc i)))
    (setq i (+ i 1)))
  smallest-loc)

;;; day5.el ends here
