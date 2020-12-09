(defun occurrences (lst)
  (if (null lst)
      lst
      (sort (my-reduce (my-map lst)) 
            #'(lambda (pair1 pair2)
                (> (cdr pair1) (cdr pair2))))))


;; mapper
(defun my-map (lst)
  (if (null lst)
      lst
      (mapcar #'(lambda (obj)
                  (cons obj 1))
              lst)))

;; reducer
(defun my-reduce (alist)
  (if (null alist)
      alist
      (let ((first-char (car (car alist)))
            (first-elt (car alist)))
        (let ((match-elt (assoc first-char (cdr alist))))
          (if (null match-elt)
              (cons first-elt (my-reduce (cdr alist)))
              (my-reduce (my-combine alist)))))))


;; combiner
(defun my-combine (alist)
  (let ((index (indexof (car alist) (cdr alist)))
        (first-char (car (car alist))))
    (append (subseq (cdr alist) 0 index)
            (list (cons first-char (+ (cdr (car alist)) (cdr (assoc first-char (cdr alist))))))
            (subseq (cdr alist) (+ 1 index)))))
          


;; This is my helper function
(defun indexof (pair alist)
  (if (null alist)
      alist
      (if (equal (car pair)
                 (car (car alist)))
          0
          (if (null (indexof pair (cdr alist)))
              nil
              (+ 1 (indexof pair (cdr alist)))))))


                               
    


