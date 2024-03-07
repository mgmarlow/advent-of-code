(ql:quickload "cl-utilities")

;; [T]             [P]     [J]        
;; [F]     [S]     [T]     [R]     [B]
;; [V]     [M] [H] [S]     [F]     [R]
;; [Z]     [P] [Q] [B]     [S] [W] [P]
;; [C]     [Q] [R] [D] [Z] [N] [H] [Q]
;; [W] [B] [T] [F] [L] [T] [M] [F] [T]
;; [S] [R] [Z] [V] [G] [R] [Q] [N] [Z]
;; [Q] [Q] [B] [D] [J] [W] [H] [R] [J]
;;  1   2   3   4   5   6   7   8   9 

(defun create-stack (str)
  (loop for i across str collect i))

(defun defstacks (&rest forms)
  (let ((stacks (make-hash-table)))
    (loop for i from 1
          for f in forms do
            (setf (gethash i stacks) (create-stack f)))
    stacks))

(defun create-stacks ()
  (defstacks
    "TFVZCWSQ"
    "BRQ"
    "SMPQTZB"
    "HQRFVD"
    "PTSBDLGJ"
    "ZTRW"
    "JRFSNMQH"
    "WHFNR"
    "BRPQTZJ"))

(defun instructionp (line)
  (and (> (length line) 4)
       (string= (subseq line 0 4) "move")))

(defclass instruction ()
  ((moves
    :initarg :moves
    :accessor moves)
   (source
    :initarg :source
    :accessor source)
   (target
    :initarg :target
    :accessor target)))

(defun new-instruction (line)
  (let* ((parsed (cl-utilities:split-sequence #\SPACE line))
         (moves (parse-integer (nth 1 parsed)))
         (source (parse-integer (nth 3 parsed)))
         (target (parse-integer (nth 5 parsed))))
    (make-instance 'instruction
                   :moves moves
                   :source source
                   :target target)))

(defun read-instructions ()
  (with-open-file (in "../data/day05.txt")
    (loop for line = (read-line in nil)
          while line
          when (instructionp line)
            collect (new-instruction line))))

(defparameter *instructions* (read-instructions))

(defun topmost (stacks)
  (loop for v being the hash-values in stacks
        collect (first v)))

(defun p1-apply-instructions ()
  (let ((stacks (create-stacks)))
    (loop for instruction in *instructions* do
      (with-accessors ((moves moves)
                       (source source)
                       (target target))
          instruction
        (dotimes (i moves)
          (let ((item (pop (gethash source stacks))))
            (push item (gethash target stacks))))))
    (coerce (topmost stacks) 'string)))

(defun p2-apply-instructions ()
  (let ((stacks (create-stacks)))
    (loop for instruction in *instructions* do
      (with-accessors ((moves moves)
                       (source source)
                       (target target))
          instruction
        (let ((buffer '()))
          (dotimes (i moves)
            (let ((item (pop (gethash source stacks))))
              (push item buffer)))
          (loop for item in buffer do
            (push item (gethash target stacks))))))
    (coerce (topmost stacks) 'string)))

(format t "P1: ~a; P2: ~a~%" (p1-apply-instructions) (p2-apply-instructions))
