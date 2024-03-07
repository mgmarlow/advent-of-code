(defun read-into-list (file)
  (with-open-file (in file)
    (loop for line = (read-line in nil)
          while line
          collect line)))

(defun priority (item)
  (1+ (position item "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")))

(defvar *rucksacks* (read-into-list "../data/day03.txt"))

(defun divide-compartments (rucksack)
  (let ((midpoint (/ (length rucksack) 2)))
    (list
     (loop for c across (subseq rucksack 0 midpoint)
           collect c)
     (loop for c across (subseq rucksack midpoint (length rucksack))
           collect c))))

(defun common-items (rucksack)
  (apply #'intersection (divide-compartments rucksack)))

(defun p1-common-item-total ()
  (loop for rucksack in *rucksacks*
        ;; Assuming 1 common item
        summing (priority (car (common-items rucksack))) into total
        finally (return total)))

(defun group (rucksacks)
  (let ((cur ()) (acc ()))
    (loop for rucksack in rucksacks do
      (push rucksack cur)
      (if (= (length cur) 3)
          (progn
            (push cur acc)
            (setf cur ()))))
    acc))

(defun items (rucksack)
  (coerce rucksack 'list))

(defun badge (rucksack-group)
  (let ((charlists (map 'list #'items rucksack-group)))
    (intersection
     (first charlists)
     (apply #'intersection (rest charlists)))))

(defun p2-badge-total ()
  (loop for group in (group *rucksacks*)
        ;; Assuming 1 badge per group
        summing (priority (car (badge group))) into total
        finally (return total)))

(format t "P1: ~a; P2: ~a~%" (p1-common-item-total) (p2-badge-total))
