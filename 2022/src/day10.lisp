(ql:quickload "cl-utilities")

(defclass operation ()
  ((command
    :initarg :command
    :accessor command)
   (value
    :initarg :value
    :accessor value)))

(defmethod work ((op operation))
  (cond
    ((string= (command op) "noop") 1)
    ((string= (command op) "addx") 2)
    (t 0)))

(defun new-operation (line)
  (let* ((parsed (cl-utilities:split-sequence #\SPACE line))
         (command (first parsed))
         (value (second parsed)))
    (make-instance 'operation
                   :command command
                   :value (when value (parse-integer value)))))

(defun read-instructions ()
  (with-open-file (in "../data/day10.txt")
    (loop for line = (read-line in nil)
          while line
          when (> (length line) 0)
            collect (new-operation line))))

(defclass cpu ()
  ((counter
    :initform 0
    :accessor counter)
   (cycle
    :initform 1
    :accessor cycle)
   (regx
    :initform 1
    :accessor regx)))

(defmethod process-instruction ((m cpu) instruction)
  (cond
    ;; ignore noop
    ((string= "addx" (command instruction)) (incf (regx m) (value instruction)))))

(defmethod run ((m cpu) instructions cb)
  (let ((current-instruction))
    (loop while (> (length instructions) 0) do
      (funcall cb m)
      (unless current-instruction
        (setf current-instruction (pop instructions))
        (setf (counter m) (work current-instruction)))
      (decf (counter m))
      (when (= 0 (counter m))
        (process-instruction m current-instruction)
        (setf current-instruction nil))
      (incf (cycle m)))))

(defmethod signal-strength ((m cpu))
  (* (cycle m) (regx m)))

(defun p1-cycles ()
  (let ((machine (make-instance 'cpu)) (sum 0))
    (run machine (read-instructions) #'(lambda (m)
                                         (when (find (cycle m) '(20 60 100 140 180 220))
                                           (incf sum (signal-strength m)))))
    sum))

(defun draw-pixel (m crt-width)
  ;; Cycles start at 1, pixels start at 0
  (let ((current-pixel (rem (1- (cycle m)) crt-width)))
    (if (or
         (= current-pixel (regx m))
         (= current-pixel (1+ (regx m)))
         (= current-pixel (1- (regx m))))
        (format t "#")
        (format t "."))))

(defun draw-crt ()
  (let ((machine (make-instance 'cpu)) (crt-width 40))
    (run machine (read-instructions) #'(lambda (m)
                                         (draw-pixel m crt-width)
                                         (when (= (mod (cycle m) crt-width) 0)
                                           (format t "~%"))))))

(format t "P1: ~a; P2: ~a~%" (p1-cycles) (draw-crt))
