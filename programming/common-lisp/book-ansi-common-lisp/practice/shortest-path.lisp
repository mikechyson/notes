;; The function takes a start node, a destination node,
;; and a newwork, and returns the shortest path, if there is one

;; nodes are represented as symbols,
;; and networks are represented as assoc-lists
;; with elements of the form
;; (node . neighbors)

(defun shortest-path (start end net)
  (bfs end (list (list start)) net))    ;breadth-first search


;; * (append '(a b c) '(d e f))
;; (A B C D E F)

;; * (cons '(a b c) '(d e f))
;; ((A B C) D E F)

(defun bfs (end queue net)              ;breadth-first search
  (if (null queue)
      nil
      (let ((path (car queue)))
        (let ((node (car path)))
          (if (eql node end)
              (reverse path)
              (bfs end
                   (append (cdr queue)
                           (new-paths path node net))
                   net))))))



;; * (mapcar #'list 
;;         '(a b c)
;;         '(1 2 3 4))
;; ((A 1) (B 2) (C 3))

;; * (setf trans '((+ . "add") (- . "substract")))
;; ((+ . "add") (- . "substract"))

;; * (assoc '+ trans)
;; (+ . "add")

;; * (assoc '* trans)
;; NIL

(defun new-paths (path node net)
  (mapcar #'(lambda (n)
              (cons n path))
          (cdr (assoc node net))))


