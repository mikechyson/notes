;; recursion
(defun reverse-pos+ (lst)
  (if (null lst)
      lst
      (cons (+ (length (cdr lst))
               (car lst))
            (reverse-pos+ (cdr lst)))))

(defun pos+recursion (lst)
  (reverse (reverse-pos+ (reverse lst))))



;; iteration
(defun pos+iteration (lst)
  (let ((index 0)
        (reverse-list nil))
    (dolist (obj lst)
      (push (+ index obj) reverse-list)
      (setf index (+ index 1)))
    (reverse reverse-list)))



;; mapcar
(defun pos+seq (lst)
  (if (null lst)
      lst
      (cons (length (cdr lst)) (pos+seq (cdr lst)))))

(defun pos+mapcar (lst)
  (mapcar #'(lambda (x y)
              (+ x y))
          lst
          (reverse (pos+seq lst))))
