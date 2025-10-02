(defun p70a (str)
  (do ((i (1- (length str)) (1- i))
       (stack NIL))
      ((< i 0) (pop stack))
      (case (char str i)
        (#\^ 
          (do* ((item (pop stack) (pop stack))
                (list (cons item NIL) (cons item list)))
               ((atom item) (push list stack))))
        (T
          (push (intern (make-string 1 :initial-element (char str i)))
                stack)))))

(defun p70b (tree)
  (when (tree-p tree)
    (format nil "~a~{~a~^~}^" (car tree) (mapcar #'p70b (cdr tree)))))

(defun tree-p (tree)
  (and tree
       (listp tree)
       (atom (car tree))
       (every #'tree-p (cdr tree))))

(defun p70c (tree)
  (when (tree-p tree)
    (1+ (reduce #'+ (mapcar #'p70c (cdr tree))))))

(defun p71 (tree)
  (when (tree-p tree)
    (reduce #'+ (mapcar (lambda (tr) (+ 2 (p71 tr))) (cdr tree)))))

(defun p72 (tree)
  (when tree
    (nconc (mapcan #'p72 (cdr tree)) (list (car tree)))))

(defun p73 (tree)
  (format nil "t(~a,[~{~a~^,~}])" (car tree) (mapcar #'p73 (cdr tree))))