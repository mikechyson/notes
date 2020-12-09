(defun new-union (lst1 lst2 fn)
  (let ((lst-union (append lst1 lst2)))
    (funcall fn lst-union)))


;; This is very slow.
;; Because lisp is sequential and I take the last element every recursion.
;; But I don't take the efficiency into account at the first.
(defun set-unique (lst)
  (if (null lst)
      lst
    (let ((front-seq (subseq lst 0 (- (length lst) 1))))
      (if (member (car (last lst)) front-seq)
          (set-unique front-seq)
        (append (set-unique front-seq) (last lst))))))
    

;; iteration version
(defun set-unique-iteration (lst)
  (let ((full-queue nil))
    (dolist (obj lst)
      (if (not (member obj full-queue))
          (setf full-queue (append full-queue (list obj)))))
    full-queue))
      
