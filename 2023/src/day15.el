;;; day15.el --- Lens Library -*- lexical-binding: t; -*-

;;; Code:

(defun read-file (file)
  (let (rst)
    (with-temp-buffer
      (insert-file-contents file)
      (while (re-search-forward "[^,^\n]+" nil t)
        (push (match-string 0) rst)))
    (reverse rst)))

(defvar data (read-file "../data/day15.txt"))

(defun hash (str)
  (let ((rst 0))
    (dotimes (i (length str))
      (setq rst (mod (* 17 (+ rst (aref str i))) 256)))
    rst))

(defun sum (lst) (apply #'+ lst))

;; Part 1
(sum (mapcar #'hash data))

;; Part 2
(defun lens (label)
  (string-match "[^=^-]+" label)
  (match-string 0 label))

(defun focal-length (label)
  (string-match "=\\(.*\\)" label)
  (string-to-number (match-string 1 label)))

(defun box-assignment (label)
  (hash (lens label)))

(defun assign-boxes (labels)
  (let ((boxes (make-vector 256 nil)))
    (dolist (label labels)
      (let ((box (box-assignment label)))
        (cond
         ((string-match-p "=" label)
          (if (seq-find (apply-partially #'s-starts-with? (lens label)) (aref boxes box))
              (aset boxes box (seq-map
                               (lambda (o)
                                 (if (s-starts-with? (lens label) o)
                                     label
                                   o))
                               (aref boxes box)))
            (push label (aref boxes box))))
         ((string-match-p "-" label)
          (aset boxes box (seq-remove
                           (apply-partially #'s-starts-with? (lens label))
                           (aref boxes box)))))))
    boxes))

(let ((rst 0) (boxes (assign-boxes data)))
  (dotimes (boxi (length boxes))
    (setq rst (+ rst (sum (seq-map-indexed
                           (lambda (elt idx)
                             (* (+ 1 boxi) (+ 1 idx) (focal-length elt)))
                           (reverse (aref boxes boxi)))))))
  rst)

;;; day15.el ends here
