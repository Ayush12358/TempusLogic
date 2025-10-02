(defun p95 (number &optional (base 10))
  (format nil "~{~a~^-~}" (mapcar #'digit-word (digits number base))))

(defun digits (number &optional (base 10) acc)
  (if (< number base)
      (cons number acc)
      (multiple-value-bind (div rem) (truncate number base)
        (digits div base (cons rem acc)))))

(defun digit-word (digit)
  (cdr (assoc number *digit-words*)))

(defparameter *digit-words*
  (mapcar (lambda (d)
            (cons d (format nil "~r" d)))
    (p22 0 9))
  "An alist mapping each digit to their word representation.")

(defun number-to-word (number &aux (base 10))
  (if (and (< number *hundreds-limit*) (/= (rem number (expt base 3)) 0))
      (multiple-value-bind (div rem) (truncate number (expt base 2))
        (if (= div 0)
            (tens-to-word rem)
            (format nil "~a hundred~@[ ~a~]"
              (tens-to-word div)
              (when (> rem 0)
                (tens-to-word rem)))))
      (do ((n number)
           (c 0 (1+ c))
           res)
          ((= n 0)
           (format nil "~{~a~^ ~}" res))
          (multiple-value-bind (div rem) (truncate n (expt base 3))
            (when (/= rem 0)
              (when (> c 0)
                (push (nth (1- c) *thousands*) res))
              (push (number-to-word rem) res))
            (if (< c (1- (length *thousands*)))
                (setf n div)
                (progn 
                  (push (car (last *thousands*)) res)
                  (push (number-to-word div) res)
                  (setf n 0)))))))

(defun tens-to-word (number &aux (base 10))
  (multiple-value-bind (div rem) (truncate number base)
    (format nil "~[~a~;~*~a~:;~2*~a-~1@*~a~]"
      div
      (second (assoc rem *digits*))
      (third  (assoc rem *digits*))
      (fourth (assoc div *digits*)))))

(defparameter *hundreds-limit* 2000
  "Print all numbers less than *HUNDREDS-LIMIT*, except for multiples of 1000, using hundreds instead of thousands.
Example: \"eleven hundred\" for 1100, instead of \"one thousand one hundred\".")

(defparameter *thousands*
  '("thousand" "million" "billion" "trillion" "quadrillion" "quintillion" "sextillion" "septillion" "octillion" "nonillion")
  "List of powers of 1000 up to 1000^30.")

(defparameter *digits*
  (mapcar (lambda (d)
            (list d
                  (format nil "~r" d)
                  (format nil "~r" (+ 10 d))
                  (format nil "~r" (* 10 d))))
    (p22 0 9))
  "Alist mapping every digit D to the word representation of D, 1D and D0.")

(defun p96 (string &aux (list (coerce string 'list)))
  (labels ((rest-p (list) 
             (or (null list)
                 (and (alphanumericp (car list))
                      (rest-p (cdr list)))
                 (and (char= (car list) #\_)
                      (cdr list)
                      (alphanumericp (cadr list))
                      (rest-p (cddr list))))))
    (and list
         (alpha-char-p (car list))
         (rest-p (cdr list)))))

(defun alt-identifier-p (string)
  (funcall (recognizer-predicate
             (make-recognizer
               `((start end ,(predicate-recognizer #'alpha-char-p))
                 (end b ,(at-most-one (lambda (x) (eq x #\_))))
                 (b end ,(predicate-recognizer #'alphanumericp)))))
           string))

(defun make-recognizer (syntax-list &aux (graph (d-readable-graph syntax-list))
                                         (start (start-state graph))
                                         (end (end-state graph)))
  (and start end
    (lambda (string)
      (do ((node start)
           (string string)
           queue)
          ((null queue)
           (if (eq node end)
               string))
          (let ((added (loop for e in (edges node graph)
                             for res = (funcall (edge-weight e) string)
                         if res
                         collect (cons (end-node e) res))))
            (setf queue   (append queue added)))
          (let ((pair (pop queue)))
            (setf node (car pair)
                  string (cdr pair)))))))

(defun start-state (syntax-graph)
  (let ((start (remove 'start (graph-nodes syntax-graph) :test-not #'eq)))
    (if start
        (car start))))

(defun end-state (syntax-graph)
  (let ((end (remove 'end (graph-nodes syntax-graph) :test-not #'eq)))
    (if end
        (car end))))

(defun predicate-recognizer (pred)
  (lambda (string)
    (when (funcall pred (char string 0))
          (subseq string 1))))

(defun recognizer-predicate (recognizer)
  (lambda (string &aux (res (funcall recognizer string)))
    (and res (zerop (length res)))))

(defun at-most-one (pred)
  (lambda (string)
    (if (funcall pred (char string 0))
        (subseq string 1)
        string)))

(defun kleene-star (pred)
  (lambda (string)
    (if (funcall pred (char string 0))
        (funcall (kleene-star pred) (subseq string 1))
        string)))

(defconstant id-recognizer #'identity "A recognizer predicate that does nothing.")

(defun recognizer-union (arg1 arg2)
  (lambda (string &aux (res (funcall arg1 string)))
    (if res
        res
        (funcall arg2 string))))

(defun recognizer-compose (arg1 arg2)
  (lambda (string &aux (res (funcall arg1 string)))
    (when res
      (funcall arg2 res))))

(defun literal-recognizer (literal)
  (if (characterp literal)
      (setf literal (make-string 1 :initial-element literal)))
  (lambda (string)
    (and (<= (length literal) (length string))
         (string= literal (subseq string 0 (length literal)))
         (subseq string (length literal)))))

(defun parentheses (string)
  (funcall (make-recognizer `((start end ,#'identity)
                              (start a   ,(literal-recognizer "("))
                              (a     b   ,#'parentheses)
                              (b     end ,(literal-recognizer ")"))))))
