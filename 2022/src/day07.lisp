(ql:quickload "cl-utilities")

(defclass node ()
  ((name
    :initarg :name
    :accessor name)
   (kind
    :initarg :kind
    :initform "dir"
    :accessor kind)
   (children
    :initarg :children
    :initform '()
    :accessor children)
   (parent
    :initarg :parent
    :accessor parent)
   (size
    :initarg :size
    :initform 0
    :accessor size)))

(defun cd-commandp (parsed-line)
  (string= (second parsed-line) "cd"))

(defun commandp (line)
  (string= (char line 0) #\$))

(defun find-node (name lst)
  (find-if #'(lambda (node) (string= (name node) name)) lst))

(defparameter *root* (make-instance 'node :name "/" :kind "dir"))
(defparameter *cwd* *root*)
(defparameter *sum* 0)
(defparameter *smallest* nil)

(defun modify-tree (line)
  (let ((parsed (cl-utilities:split-sequence #\SPACE line)))
    (cond
      ((commandp line)
       ;; ls command is ignored
       (when (cd-commandp parsed)
         (cond ((string= (third parsed) "/") (setf *cwd* *root*))
               ((string= (third parsed) "..") (setf *cwd* (parent *cwd*)))
               (t (let ((next (find-node (third parsed) (children *cwd*))))
                    (setf *cwd* next))))))
      (t (let ((existing (find-node (second parsed) (children *cwd*))))
           (unless existing
             (push  (if (string= (first parsed) "dir")
                        (make-instance 'node
                                       :kind "dir"
                                       :name (second parsed)
                                       :parent *cwd*)
                        (make-instance 'node
                                       :kind "file"
                                       :name (second parsed)
                                       :size (parse-integer (first parsed))))
                    (children *cwd*))))))))

(defun rtally (dir)
  (loop for child in (children dir) do
    (if (string= (kind child) "dir")
        (incf (size dir) (rtally child))
        (incf (size dir) (size child))))
  (size dir))

(defun build-file-system ()
  (with-open-file (in "../data/day07.txt")
    (loop for line = (read-line in nil)
          while line
          when (> (length line) 0) do
            (modify-tree line)))
  (rtally *root*))

(defun rsum-beneath-100000 (dir)
  (loop for child in (children dir) do
    (when (string= (kind child) "dir")
      (when (<= (size child) 100000)
        (incf *sum* (size child)))
      (rsum-beneath-100000 child))))

(defun p1-total-sizes ()
  (let ((*root* (make-instance 'node :name "/" :kind "dir"))
        (*cwd* *root*)
        (*sum* 0))
    (build-file-system)
    (rsum-beneath-100000 *root*)
    *sum*))

(defun find-smallest-to-delete (desired-space dir)
  (loop for child in (children dir) do
    (when (string= (kind child) "dir")
      (when (and (< (size child) (size *smallest*))
                 (>= (size child) desired-space))
        (setf *smallest* child))
      (find-smallest-to-delete desired-space child))))

(defun p2-smallest-deletion ()
  (let ((*root* (make-instance 'node :name "/" :kind "dir"))
        (*cwd* *root*))
    (build-file-system)
    (let ((*smallest* *root*))
      (find-smallest-to-delete (- 30000000 (- 70000000 (size *root*))) *root*)
      (size *smallest*))))

(format t "P1: ~a; P2: ~a~%" (p1-total-sizes) (p2-smallest-deletion))
