;; A palindrome is a sequence that reads the same in either direction
;; If a palindrome has an even number of elements, the the second half will be a mirror of the first.
(defun mirror? (s)
  (let ((len (length s)))
    (and (evenp len)
         (let ((mid (/ len 2)))
           (equal (subseq s 0 mid)
                  (reverse (subseq s mid)))))))
