(defstruct graph 
  nodes edges directed)

(defun make-ud-graph (nodes edges)
  (make-graph :nodes nodes :edges edges :directed NIL))
  
(defun make-d-graph (nodes edges)
  (make-graph :nodes nodes :edges edges :directed T))

(defun p80a (graph)
  (mapcar (lambda (node)
            (cons node 
                  (mapcar #'cdr 
                    (remove-if-not (lambda (edge) (edge-of edge node graph)) 
                                   (graph-edges graph)))))
          (graph-nodes graph)))

(defun p80b (graph)
  (append (graph-edges graph)
          (lone-nodes graph)))

(defun lone-nodes (graph)
  (set-difference (graph-nodes graph)
    (mapcan #'nodes (graph-edges graph))))

(defun p80c (adj-list &aux (nodes (mapcar #'car adj-list)))
  (make-ud-graph
    nodes
    (let (edges)
      (mapc (lambda (n)
        (mapc (lambda (x &aux (e (cons n x)))
          (unless (member e edges :test (lambda (l1 l2) (null (set-exclusive-or l1 l2)))
                                  :key #'nodes)
            (push e edges)))
          (cdr (assoc n adj-list))))
        nodes)
      edges)))

(defun p80d (adj-list &aux (nodes (mapcar #'car adj-list)))
  (make-d-graph
    nodes
    (let (edges)
      (mapc (lambda (n)
        (mapc (lambda (x &aux (e (cons n x)))
                (push e edges))
          (cdr (assoc n adj-list))))
        nodes)
      edges)))

(defun p80e (list &aux (lone-nodes (remove-if-not #'atom list)) (edges (remove-if-not #'edge-p list)))
  (make-ud-graph (append lone-nodes
                         (mapcan #'nodes edges))
                 edges))

(defun p80f (list &aux (lone-nodes (remove-if-not #'atom list)) (edges (remove-if-not #'edge-p list)))
  (make-d-graph (append lone-nodes
                        (mapcan #'nodes edges))
                edges))

(defun edge-of (edge node graph)
  (or (eq node (car edge))
      (if (not (graph-directed graph))
          (eq node (end-node edge)))))

(defun nodes (edge)
  (list (car edge)
        (end-node edge)))

(defun edge-p (edge)
  (and (consp edge)
       (or (atom (cdr edge))
           (atom (cadr edge))
           (null (cdddr edge)))))

(defun edge (graph node1 node2)
  (let ((edges (assoc node1 (p80a graph))))
    (if edges
      (let ((edge-end (find node2 (cdr edges) 
                        :test (lambda (n e)
                                (or (eq n e)
                                    (if (consp e)
                                      (eq n (car e))))))))
        (or (atom edge-end)
            (cadr edge-end))))))

(defun edges (node graph)
  (mapcar (lambda (x) (cons node x)) (cdr (assoc node (p80a graph)))))

(defun neighbors (node graph)
  (mapcar (lambda (end)
            (if (atom end)
                end
                (car end)))
          (cdr (assoc node (p80a graph)))))

(defun start-node (edge)
  (car edge))

(defun end-node (edge)
  (if (consp (cdr edge))
      (cadr edge)
      (cdr edge)))

(defun other-node (edge node)
  (cond ((eq node (start-node edge)) (end-node edge))
        ((eq node (end-node edge)) (start-node edge))))

(defun edge-weight (edge)
  (and (edge-p edge)
       (consp (cdr edge))
       (caddr edge)))

(defun empty-p (graph)
  (null (graph-nodes graph)))

(defun subgraph-p (graph1 graph2)
  (and (graph-p graph1)
       (graph-p graph2)
       (subsetp (graph-nodes graph1) (graph-nodes graph2))
       (subsetp (graph-edges graph1) (graph-edges graph2))))

(defun graph-equal-p (graph1 graph2)
  (null (set-exclusive-or (p80b graph1) (p80b graph2)
          :test (lambda (a b)
                  (or (and (not (graph-directed graph1))
                           (not (graph-directed graph2))
                           (edge-p a)
                           (edge-p b)
                           (null (set-exclusive-or (nodes a) (nodes b)))
                           (eql (edge-weight a) (edge-weight b)))
                      (eql a b))))))
