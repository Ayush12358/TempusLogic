(defmacro p46 (preds expr &optional prefs)
  (if (null preds)
      `(format t "~%~{~a~^ ~} ~a" ,(reverse prefs) (bool-symbol ,expr))
      (list 'progn
        (p46 (cdr preds) (subst T   (car preds) expr) (cons (bool-symbol T)   prefs))
        (p46 (cdr preds) (subst NIL (car preds) expr) (cons (bool-symbol NIL) prefs)))))

(defun bool-symbol (bool)
  (if bool 'true 'fail))

(defun nand (a b) (not (and a b)))

(defun nor (a b) (not (or a b)))

(defun xor (a b) (if a (not b) b))

(defun impl (a b) (if a b T))

(defun equ (a b) (if a b (not b)))

(let (dict)
  (defun p49 (n)
    (cond ((= n 1)
           (list (list 0) (list 1)))
          (t
           (unless (assoc n dict)
             (push (list n
                     (append (mapcar (lambda (code) (cons 0 code)) (p49 (1- n)))
                             (mapcar (lambda (code) (cons 1 code)) (reverse (p49 (1- n))))))
                   dict))
           (cadr (assoc n dict))))))

(defun p50 (fs)
  (setf fs (copy-tree fs))
  (do () ((= (length fs) 1))
      (sort fs #'< :key #'cdr)
      (setf fs (cons (cons (cons (caar fs) (caadr fs))
                           (+ (cdar fs) (cdadr fs)))
                     (cddr fs))))
  (generate-table (caar fs)))

(defun generate-table (tree)
  (if (consp tree)
      (append (mapcar (lambda (pair) (cons (car pair) (cons 0 (cdr pair))))
                (generate-table (car tree)))
              (mapcar (lambda (pair) (cons (car pair) (cons 1 (cdr pair))))
                (generate-table (cdr tree))))
      (list (cons tree NIL))))
