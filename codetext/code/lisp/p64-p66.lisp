(defun p64 (tree &optional (level 1) (left-cousins 0))
  (when tree
    (list (list (first tree) (cons (+ 1 left-cousins (length (nodes (second tree)))) level))
          (p64 (second tree) (1+ level) left-cousins)
          (p64 (third tree) (1+ level) (+ 1 left-cousins (length (nodes (second tree))))))))

(defun p65 (tree &optional (level 1) (distance (expt 2 (- (height tree) 2))) (start-x (start-x tree)))
  (when tree
    (list (list (first tree) (cons start-x level))
          (p65 (second tree) (1+ level) (/ distance 2) (- start-x distance))
          (p65 (third  tree) (1+ level) (/ distance 2) (+ start-x distance)))))

(defun start-x (tree &aux (distance (expt 2 (- (height tree) 2))))
  (if (second tree)
      (+ distance (start-x (second tree)))
      0))

(defun p66 (tree &optional (level 1))
  (when tree
    (let* ((left (p66 (second tree) (1+ level)))
           (right (p66 (third tree) (1+ level)))
           (distance (min-distance left right)))
      (list (list (first tree) (cons (+ distance (x-coord left)) level))
            left
            (shift-right (- (+ (* 2 distance) (x-coord left)) (x-coord right) right)))))

(defun shift-right (dx tree)
  (when tree
    (list (list (first (first tree)) (cons (+ dx (x-coord tree)) (y-coord tree)))
          (shift-right dx (second tree))
          (shift-right dx (third tree)))))

(defun min-distance (left right)
  (do ((distance (- (x-coord left) (x-coord (second left))) (1+ distance)))
      ((not (intersect-p left right (* 2 distance))) distance)))

(defun intersect-p (left right dx &aux (rdx (- (+ (x-coord left) dx)) (x-coord right)))
  (some (lambda (level)
          (intersection (mapcar #'x-coord (p62b level left))
                        (mapcar (lambda (node) (+ rdx (x-coord node)))
                          (p62b level right))))
        (p22 1 (1- (min (height left) (height right))))))

(defun rightmost (tree)
  (first (sort (mapcar #'x-coord (inorder tree)) #'>)))

(defun x-coord (tree)
  (and tree (p54 tree)
    (car (second (first tree)))))

(defun y-coord (tree)
  (and tree (p54 tree)
    (cdr (second (first tree)))))
