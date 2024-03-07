(defparameter *rawinput*
  (with-open-file (in "../data/day01.txt")
    (loop for line = (read-line in nil)
          while line
          collect line)))

(defparameter *calories-per-elf*
  (let ((elves ()) (current 0))
    (loop for calories in *rawinput* do
      (if (> (length calories) 0)
          (incf current (parse-integer calories))
          (progn
            (push current elves)
            (setf current 0))))
    ;; Don't forget the last one!
    (push current elves)
    elves))

(defun most-calories ()
  (apply #'max *calories-per-elf*))

(defun top-three-summed ()
  (let ((sorted (sort *calories-per-elf* #'>)))
    (+ (first sorted) (second sorted) (third sorted))))

(format t "P1: ~a; P2: ~a~%" (most-calories) (top-three-summed))
