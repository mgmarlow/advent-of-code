(defun build-pair (str)
  (let ((midpoint (position #\- str)))
    (cons (parse-integer (subseq str 0 midpoint))
          (parse-integer (subseq str (1+ midpoint) (length str))))))

(defun elf-pair (line)
  (let ((midpoint (position #\, line)))
    (list (build-pair (subseq line 0 midpoint))
          (build-pair (subseq line (1+ midpoint) (length line))))))

(defun read-elf-pairs ()
  (with-open-file (in "../data/day04.txt")
    (loop for line = (read-line in nil)
          while line
          collect (elf-pair line))))

(defvar *elf-pairs* (read-elf-pairs))

(defun fully-overlapp (elf-pair)
  (let ((pair1 (first elf-pair)) (pair2 (second elf-pair)))
    (or
     (and (<= (car pair1) (car pair2))
          (>= (cdr pair1) (cdr pair2)))
     (and (<= (car pair2) (car pair1))
          (>= (cdr pair2) (cdr pair1))))))

(defun total-complete-overlaps ()
  (loop for pair in *elf-pairs*
        counting (fully-overlapp pair) into total
        finally (return total)))

(defun overlapp (elf-pair)
  (let ((pair1 (first elf-pair)) (pair2 (second elf-pair)))
    (or
     (fully-overlapp elf-pair)
     (and (<= (car pair1) (car pair2))
          (>= (cdr pair1) (car pair2)))
     (and (<= (car pair1) (cdr pair2))
          (>= (cdr pair1) (cdr pair2))))))

(defun total-overlaps ()
  (loop for pair in *elf-pairs*
        counting (overlapp pair) into total
        finally (return total)))

(format t "P1: ~a; P2: ~a~%" (total-complete-overlaps) (total-overlaps))
