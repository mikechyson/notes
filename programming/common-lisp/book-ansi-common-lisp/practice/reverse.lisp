;; iterative version of reverse
(defun our-reverse (lst)
  (let ((acc nil))
    (dolist (elt lst)                   ;element list
      (push elt acc))
    acc))
