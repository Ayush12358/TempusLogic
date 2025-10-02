(in-package #:99-lisp-problems)

(defun p1 (list)
  (if (null (cdr list))
      (car list)
      (p1 (cdr list))))

(defun p2 (list)
  (p1 (reverse (cdr (reverse list)))))

(defun p3 (list n)
  (if (= n 1)
      (car list)
      (p3 (cdr list) (- n 1))))

(defun p4 (list)
  (if (null list)
      0
      (+ 1 (p4 (cdr list)))))

(defun p5 (list)
  (if (null list)
      nil
      (append (p5 (cdr list)) (list (car list)))))

(defun p6 (list)
  (equal list (p5 list)))

(defun p7 (list)
  (if (null list)
      nil
      (if (listp (car list))
          (append (p7 (car list)) (p7 (cdr list)))
          (cons (car list) (p7 (cdr list))))))
