(defun p31 (n)
  (do ((i 2 (1+ i)))
      ((> i (sqrt n)) T)
      (when (divides-p i n)
        (return-from NIL NIL))))

(defun divides-p (d n)
  (= (rem n d) 0))

(defun p32 (a b)
  (cond ((< b a) (p32 b a))
        ((= a 0) b)
        (T (p32 (rem b a) a))))

(defun p33 (a b)
  (= (p32 a b) 1))

(defun p34 (n)
  (do ((d 1 (1+ d))
       (count 0)
      ((> d n) count)
      (when (p33 d n)
        (incf count))))

(defun p35 (n)
  (do ((p 2 (1+ p)))
      ((> (* p p) n) (list n))
      (and (p31 p)
           (divides-p p n)
           (return-from NIL (cons p (p35 (/ n p)))))))

(defun p36 (n)
  (mapcar #'reverse (p10 (p35 n))))

(defun p37 (n)
  (reduce #'*
    (mapcar (lambda (pair &aux (p (car pair)) (m (cadr pair)))
              (* (1- p) (expt p (1- m))))
      (p36 n))))

(defun p38 (n)
  (format t "~%Evaluating phi(~D) using the solution for P34:" n)
  (time (p34 n))
  (format t "~%Evaluating phi(~D) using the solution for P37:" n)
  (time (p37 n))
  NIL)

(defun p39 (a b)
  (remove-if-not #'p31 (p22 a b)))

(defun p40 (n)
  (and (evenp n) (> n 2)
    (dolist (p (p39 2 n))
      (when (p31 (- n p))
        (return-from NIL (list p (- n p)))))))

(defun p41 (a b &optional (threshold 0))
  (mapc (lambda (n &aux (res (p40 n)))
          (when (> (car res) threshold)
            (format t "~%~D = ~D + ~D" n (car res) (cadr res))))
    (p22 a b)))
