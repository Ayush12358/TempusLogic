(defun p93 (numbers)
  (when numbers
    (remove-duplicates
      (mapcar #'prefix-to-infix
        (mapcan (lambda (i &aux (sides (p17 numbers i)))
                  (append (make-equations (car sides) (cadr sides))
                          (make-equations (cons (- (caar sides)) (cdar sides))
                                          (cons (- (caadr sides)) (cdadr sides)))
                          ))
          (p22 1 (1- (length numbers)))))
      :test #'equal)))

(defun make-equations (lhs rhs)
  (if (< (length lhs) (length rhs))
      (mapcan (lambda (lhs-expr)
                (mapcar (lambda (rhs-expr)
                          (list '= lhs-expr rhs-expr))
                        (remove (eval lhs-expr) (make-exprs rhs)
                          :test-not #'eql
                          :key #'eval)))
              (make-exprs lhs))
      (mapcan (lambda (rhs-expr)
                (mapcar (lambda (lhs-expr)
                          (list '= lhs-expr rhs-expr))
                        (remove (eval rhs-expr) (make-exprs lhs)
                          :test-not #'eql
                          :key #'eval)))
              (make-exprs rhs))))

(defun make-exprs (numbers)
  (if (= (length numbers) 1)
      numbers
      (loop for i from 1 to (1- (length numbers))
            for partition = (p17 numbers i) append
          (loop for arg1 in (make-exprs (car partition)) append
              (loop for arg2 in (make-exprs (cadr partition)) append
                (apply-binary-ops arg1 arg2))))))

(defun apply-unary-ops (arg1)
  (list arg1 (list '- arg1)))

(defun apply-binary-ops (arg1 arg2)
  (loop for op in '(+ - * /)
    if (apply-op op arg1 arg2)
    collect it))

(defun apply-op (op arg1 arg2)
  (unless (and (eq op '/) (zerop (eval arg2)))
    (list op arg1 arg2)))

(defparameter *unary-ops*  '(-) "Unary operators.")
(defparameter *binary-ops* '(= + - * /) "Binary operators, sorted by precedence.")
(defparameter *op-complements* '((-  +) (/  *)) "Match binary operators to the operators they are a complement of.")

(defun prefix-to-infix (expr)
  (cond ((numberp expr) (list expr))
        ((unary-p expr) expr)
        ((binary-p expr)
         (append (infix-arg (first expr) (second expr) T)
                 (cons (first expr)
                   (infix-arg (first expr) (third expr) NIL))))))

(defun infix-arg (op expr lhs &aux (res (prefix-to-infix expr)))
  (if (or (not (binary-p expr))
          (precedes-p op (first expr) lhs))
      res
      (cons '[ (append res (list '])))))

(defun precedes-p (op1 op2 lhs)
  (or (< (position op1 *binary-ops*)
         (position op2 *binary-ops*))
      (if (assoc op1 *op-complements*)
          (and lhs
               (member op2 (assoc op1 *op-complements*)))
          (eq op1 op2))))

(defun unary-p (expr)
  (and (listp expr)
       (= (length expr) 2)
       (member (first expr) *unary-ops*)))

(defun binary-p (expr)
  (and (listp expr)
       (= (length expr) 3)
       (member (first expr) *binary-ops*)))

(defun infix-to-prefix (expr &aux output operators)
  (do* ((expr expr (cdr expr))
        (item (car expr) (car expr)))
       ((null expr))
       (cond ((numberp item) (push item output))
             ((member item *binary-ops*)
              (do ()
                  ((or (null operators)
                       (not (member (car operators) *binary-ops*))
                       (precedes-p (car operators) item T)))
                  (let* ((arg2 (pop output)) (arg1 (pop output)))
                    (push (list (pop operators) arg1 arg2) output)))
              (push item operators))
             ((eq item '])
              (push item operators))
             ((eq item '[)
              (do ()
                  ((eq (car operators) ']))
                  (let* ((arg2 (pop output)) (arg1 (pop output)))
                    (push (list (pop operators) arg1 arg2) output)))
              (pop operators))))
  (dolist (op operators (car output))
    (let* ((arg2 (pop output)) (arg1 (pop output)))
        (push (list op arg1 arg2) output))))
