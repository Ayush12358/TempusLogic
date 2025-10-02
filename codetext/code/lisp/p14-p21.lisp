(defun p14 (list)
  (when list
    (cons (car list)
          (cons (car list)
                (p14 (cdr list))))))

(defun p15 (list n)
  (when list
    (add-item (car list)
              (p15 (cdr list) n)
              n)))

(defun add-item (item list n)
  (if (= n 0)
      list
      (add-item item (cons item list) (1- n))))

(defun p16 (list n &optional (k n))
  (when list
    (if (= k 1)
        (p16 (cdr list) n)
        (cons (car list)
              (p16 (cdr list) n (1- k))))))

(defun p17 (list n)
  (when list
    (if (= n 1)
        (list (list (car list))
              (cdr list))
        (let ((res (p17 (cdr list) (1- n))))
          (cons (cons (car list) (car res))
                (cdr res))))))

(defun p18 (list i k)
  (when list
    (if (= i 1)
        (if (= k 1)
            (list (car list))
            (cons (car list)
                  (p18 (cdr list) 1 (1- k))))
        (p18 (cdr list) (1- i) (1- k)))))

(defun p19 (list n)
  (when (< n 0)
    (incf n (length list)))
  (when list
    (let ((partition (p17 list n)))
      (append (cadr partition) (car partition)))))

(defun p20 (list k)
  (if (= k 1)
      (cdr list)
      (cons (car list)
            (p20 (cdr list) (1- k)))))

(defun p21 (item list k)
  (if (= k 1)
      (cons item list)
      (cons (car list)
            (p21 item (cdr list) (1- k)))))
