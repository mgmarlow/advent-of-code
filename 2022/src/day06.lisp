(defparameter *raw-signal*
  (with-open-file (in "../data/day06.txt")
    (read-line in nil)))

(defun unique-p (str)
  (= (length (remove-duplicates str :test #'char-equal))
     (length str)))

(defun find-packet-marker (start-of-packet-length)
  (loop for i from 0
        for j from start-of-packet-length
        for c across *raw-signal*
        while (< j (length *raw-signal*)) do
          (when (unique-p (subseq *raw-signal* i j)) (return j))))

(defun find-p1-marker ()
  (find-packet-marker 4))

(defun find-p2-marker ()
  (find-packet-marker 14))

(format t "P1: ~a; P2: ~a~%" (find-p1-marker) (find-p2-marker))
