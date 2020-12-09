(defun nthmost (n lst)
  (nth (- n 1)
       (sort (copy-list lst) #'>)))
