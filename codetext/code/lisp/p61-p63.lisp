(defun p61 (tree)
  (and tree (p54 tree)
    (if (or (second tree) (third tree))
        (append (p61 (second tree))
                (p61 (third tree)))
        (list (first tree)))))

(defun count-leaves (tree)
  (length (p61 tree)))

(defun p62 (tree)
  (and tree (p54 tree)
       (or (second tree) (third tree))
       (cons (first tree)
         (append (p62 (second tree))
                 (p62 (third tree))))))

(defun p62b (level tree)
  (if (= level 1)
      (list tree)
      (append (p62b (1- level) (second tree))
              (p62b (1- level) (third tree)))))

(defun level-order (tree)
  (mapcan (lambda (level) (p62b level tree))
    (p22 1 (height tree))))

(defun level-order-direct (tree)
  (do ((queue (list tree) (cdr queue))
       (acc NIL (cons (caar queue) acc)))
      ((null queue) acc)
      (nconc queue (remove-if #'null (cdar queue)))))

(defun p63 (n &optional (address 1) &aux (sym 'X))
  (when (<= address n)
      (list sym
            (p63 n (* 2 address))
            (p63 n (1+ (* 2 address))))))

(defun complete-binary-tree-p (tree)
  (and (p54 tree)
       (every (lambda (level) (= (length (p62b level tree)) (expt 2 (1- level))))
         (p22 1 (1- (height tree))))))
