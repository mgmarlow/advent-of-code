(defun get-encrypted-input ()
  (with-open-file (in "../data/day20.txt")
    (loop for line = (read-line in nil)
          while line
          collect (parse-integer line))))

;; append two subseqs?
(defun move ())

(defun decrypt ()
  (let ((cindex 0) (offset 0) (rst (get-encrypted-input)))
    ))
