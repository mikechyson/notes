;; (push obj lst)
(defun my-push (obj lst)
  (let ((new-lst (cons obj lst)))
  new-lst))
;; This is wrong.
;; I don't know why.
