(defun cons-line (str)
  (cons (char str 0) (char str 2)))

(defun strategy-guide ()
  (with-open-file (in "../data/day02.txt")
    (loop for line = (read-line in nil)
          while line
          collect (cons-line line))))

(defun hand-value (hand)
  (cond
    ((char= hand #\X) 1)
    ((char= hand #\Y) 2)
    ((char= hand #\Z) 3)))

(defun match-outcome (cell)
  (let ((opponent (car cell)) (you (cdr cell)))
    (cond
      ;; rock
      ((char= you #\X) (cond
                         ((char= opponent #\A) 3)
                         ((char= opponent #\B) 0)
                         ((char= opponent #\C) 6)))
      ;; paper
      ((char= you #\Y) (cond
                         ((char= opponent #\A) 6)
                         ((char= opponent #\B) 3)
                         ((char= opponent #\C) 0)))
      ;; scissors
      ((char= you #\Z) (cond
                         ((char= opponent #\A) 0)
                         ((char= opponent #\B) 6)
                         ((char= opponent #\C) 3))))))

(defun score (cell)
  (let ((score 0) (opponent (car cell)) (you (cdr cell)))
    (incf score (hand-value you))
    (incf score (match-outcome cell))
    score))

(defun p1-total-score ()
  (loop for cell in (strategy-guide)
        summing (score cell) into total
        finally (return total)))

(defun strategy (cell)
  (let ((opponent (car cell)) (outcome (cdr cell)))
    (cons opponent (cond
                     ;; lose
                     ((char= outcome #\X) (cond
                                            ((char= opponent #\A) #\Z)
                                            ((char= opponent #\B) #\X)
                                            ((char= opponent #\C) #\Y)))
                     ;; draw
                     ((char= outcome #\Y) (cond
                                            ((char= opponent #\A) #\X)
                                            ((char= opponent #\B) #\Y)
                                            ((char= opponent #\C) #\Z)))
                     ;; win
                     ((char= outcome #\Z) (cond
                                            ((char= opponent #\A) #\Y)
                                            ((char= opponent #\B) #\Z)
                                            ((char= opponent #\C) #\X)))))))

(defun p2-total-score ()
  (loop for cell in (strategy-guide)
        summing (score (strategy cell)) into total
        finally (return total)))

(format t "P1: ~a; P2: ~a~%" (p1-total-score) (p2-total-score))
