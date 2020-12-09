;; A proper list is either nil, or a cons whose cdr is a proper list.
(defun proper-list? (x)
  (or (null x)
      (and (consp x)
           (proper-list? (cdr x)))))
