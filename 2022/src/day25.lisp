(defparameter *rawinput*
  (with-open-file (in "../data/day25.txt")
    (loop for line = (read-line in nil)
          while line
          collect line)))

(defun decimal-digit-to-snafu-digit (d)
  (cond ((eq d -1) #\-)
        ((eq d -2 ) #\=)
        (t (digit-char d))))

(defun snafu-char-to-decimal-digit (c)
  (cond ((equal c #\-) -1)
        ((equal c #\=) -2)
        (t (digit-char-p c))))

;; http://turing.cs.trincoll.edu/~ram/cpsc110/inclass/conversions.html
(defun decimal-to-snafu (decimal)
  (coerce (nreverse (loop while (> decimal 0)
                          ;; Offset by 2 since the number range is -2 -> 2
                          collecting (let* ((r (- (rem (+ decimal 2) 5) 2))
                                            (d (floor (/ (- decimal r) 5))))
                                       (setf decimal d)
                                       (decimal-digit-to-snafu-digit r))))
          'string))

(defun snafu-to-decimal (snafu)
  (loop for c across snafu
        for i from (1- (length snafu)) downto 0
        summing (let ((digit (snafu-char-to-decimal-digit c)) (place (expt 5 i)))
                  (* digit place))
          into total
        finally (return total)))

(loop for snafu in *rawinput*
      summing (snafu-to-decimal snafu) into total
      finally (return (decimal-to-snafu total)))
