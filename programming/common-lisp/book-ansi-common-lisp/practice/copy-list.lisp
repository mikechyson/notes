(defun our-copy-list (lst)
  (if (atom lst)
      lst
      (cons (car lst) (our-copy-list (cdr lst)))))
;; what is the difference between copy-list and copy-tree?
;; why?
