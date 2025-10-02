(defun p22 (i k)
  (cond ((= i k) (list i))
        ((< k i)
         (reverse (p22 k i)))
        (T
         (cons i (p22 (1+ i) k)))))

(defun p23 (list k)
  (when (> k 0)
    (let ((i (random (length list))))
      (cons (p3 list i)
            (p23 (p20 list i)
                        (1- k))))))

(defun p24 (n m)
  (p23 (p22 1 m)
              n))

(defun p25 (list)
  (p23 list (length list)))

(defun p26 (k list)
  (cond ((null list) NIL)
        ((= k 0) (list NIL))
        (T (append (p26 k (cdr list))
                   (mapcar (lambda (c) (cons (car list) c))
                           (p26 (1- k) (cdr list)))))))

(defun p27a (list)
  (p27 list '(2 3 4)))

(defun p27b (list sizes)
  (mapcar (lambda (siz) (partition list siz)) (permutations sizes)))

(defun partition (list sizes)
  (if (= (length sizes) 1)
      (list list)
      (let ((res (p17 list (car sizes))))
        (list (car res)
              (partition res (cdr sizes))))))

(defun permutations (list)
  (if list
      (mapcar (lambda (item)
                (mapcar (lambda (perm) (cons item perm))
                        (permutations (remove-item item list))))
              (remove-duplicates list))
      (list NIL)))

(defun remove-item (item list)
  (when list
    (if (eq item (car list))
        (remove-item (cdr list))
        (cons (car list)
              (remove-item (cdr list))))))

(defun remove-duplicates (list)
  (when list
    (cons (car list)
          (remove-duplicates
            (remove-item (car list) (cdr list))))))

(defun p28a (list)
  (sort list (lambda (s1 s2) (< (length s1) (length s2)))))

(defun p28b (list)
  (let ((freqs (mapcar #'reverse (p10 (mapcar #'length list)))))
    (sort list (lambda (s1 s2) (< (cadr (assoc (length s1) freqs))
                                  (cadr (assoc (length s2) freqs)))))))

(defun sort (list pred)
  (when list
    (let ((partition (partition (cdr list)
                                (lambda (item) (funcall pred item (car list))))))
      (append (sort (car partition) pred) (cons (car list) (sort (cdr partition) pred))))))

(defun partition (list pred)
  (if list
      (let ((res (partition (cdr list) pred)))
        (if (funcall pred (car list))
            (cons (cons (car list) (car res)) (cdr res))
            (list (car res) (cons (car list) (cadr res)))))
      (list NIL NIL)))
