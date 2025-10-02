(defun p99 (word-list puzzle &aux (graph (puzzle-graph puzzle)))
  (when graph
    (assign-words word-list graph)
    (when graph
      (graph-puzzle graph))))

(defun assign-words (word-list graph)
  (when (<= (length (graph-nodes graph))
            (length word-list))
    (do* ((stack (list NIL) (cdr stack))
          found)
         ((or found (null stack))
          (when found
            (loop for (site word) in found
              do (setf (fourth site) word)
              finally (return T))))
         (if (= (length (car stack))
                (length (graph-nodes graph)))
             (setf found (car stack))
             (setf stack
               (append 
                 (loop with site = (car (set-difference (graph-nodes graph)
                                                        (mapcar #'car (car stack))
                                                        :test #'equal))
                       for word in (set-difference word-list
                                                   (mapcar #'cadr (car stack))
                                                   :test #'equal)
                       if (and (= (third site) (length word))
                               (every (lambda (edge &aux (other (other-node edge site)))
                                        (or (null (assoc other (car stack) :test #'equal))
                                            (char= (char word (site-index edge site))
                                                   (char (second (assoc other (car stack) :test #'equal))
                                                         (site-index edge other)))))
                                      (p80 site graph)))
                       collect (cons (list site word) (car stack)))
                 stack))))))

(defun site-index (edge site &aux (start (first site)) (dir (second site)) (meet (p80 edge)))
  (+ (* (car dir) (- (car meet) (car start)))
     (* (cdr dir) (- (cdr meet) (cdr start)))))

(defun puzzle-graph (puzzle &aux matrix (graph (make-graph)))
  (with-input-from-string (s puzzle)
    (do* ((lst NIL (cons l lst))
          (row 0 (1+ row))
          (col 0 (max col (length l)))
          (l (read-line s NIL) (read-line s NIL)))
         ((null l)
          (setf matrix (make-array (list row col)
                         :initial-contents 
                           (mapcar (lambda (row-list)
                                     (append row-list
                                       (make-list (- col (length row-list))
                                         :initial-element #\Space)))
                                   lst))))))
  (add-sites matrix graph (cons 0 1))
  (add-sites matrix graph (cons 1 0))
  (destructuring-bind
    (horizontal vertical)
    (reduce (lambda (node acc)
              (if (= (car (second node)) 0)
                  (push node (first acc))
                  (push node (second acc))))
            (graph-nodes graph)
            :from-end T
            :initial-element (list NIL NIL))
    (dolist (a horizontal)
      (dolist (b vertical)
        (and 
          (<= 0 
              (- (cdr (first b))
                 (cdr (first a)))
              (1- (third a)))
          (<= 0 
              (- (car (first a))
                 (car (first b)))
              (1- (third b)))
          (push (list a b (cons (car (first a))
                                (cdr (first b))))
                (graph-edges graph))))))
  graph)

(defun add-sites (matrix graph direction)
  (dotimes (row (array-dimension matrix (car direction)))
    (do* ((col 0 (1+ col))
          (cell (if (= (car direction) 0)
                    (aref matrix row col)
                    (aref matrix col row)))
          site)
         ((= col (array-dimension matrix (cdr direction))))
         (if (char= cell #\Space)
             (and site (> (second site) 1)
               (push (list (first site)
                           direction
                           (second site)
                           (coerce (third site) 'string)
                     (graph-nodes graph))
               (setf site NIL)))
             (cond (site
                    (incf (second site))
                    (if (char= cell #\.)
                        (setf (third site) NIL)
                        (vector-push cell (third site))))
                   (T
                    (setf site
                      (list (cons row col)
                            1
                            (unless (char= cell #\.)
                              (make-array (- (array-dimension matrix 0) row)
                                          :initial-element cell
                                          :fill-pointer 1))))))))))

(defun graph-puzzle (graph &aux matrix)
  (destructuring-bind
    (horizontal vertical)
    (reduce (lambda (site acc)
              (if (= (car (second site)) 0)
                  (push site (first acc))
                  (push site (second acc))))
            (graph-nodes graph)
            :from-end T
            :initial-element (list NIL NIL))
    (setf matrix
      (make-array
        (list (reduce #'max 
                (mapcar (lambda (site)
                          (+ (car (first site)) (1- (third site))))
                        horizontal))
              (reduce #'max 
                (mapcar (lambda (site)
                          (+ (cdr (first site)) (1- (third site))))
                        vertical)))
        :initial-element #\Space)))
  (dolist (site (graph-nodes graph))
    (type-site site matrix))
  (format nil "~{~A~^~%~}"
    (loop for row to (1- (array-dimension matrix 0))
      collect 
        (coerce (make-array (array-dimension matrix 1)
                  :displaced-to matrix
                  :displaced-index-offset
                    (* row (array-dimension matrix 1)))
                'string))))
