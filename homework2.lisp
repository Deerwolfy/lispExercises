;;; 4
(defun my-append (lst1 lst2)
  (if (null lst1) 
      lst2
      (cons (car lst1) (my-append (cdr lst1) lst2))))

(defun flatten (lst)
  (cond
    ((null lst) nil)
    ((listp (car lst)) (my-append (flatten (car lst)) (flatten (cdr lst))))
    (t (cons (car lst) (flatten (cdr lst))))))

;;; 5
(defun revl (lst)
  (if (null (cdr lst)) 
      (cons (car lst) nil)
      (cons (revl (cdr lst)) (cons (car lst) nil))))
      
;;; 6
(defun devlev1 (lst)
  (if (null (cdr lst)) 
      lst 
      (cons (car lst) (cons (devlev1 (cdr lst)) nil))))

(flatten (list 1 (list 2 3) 4 6 (list 7 (list 8 0 (list 1))) 6 7))
(revl (list 3 4 5 1 2))
(devlev1 (list 3 4 5 1 2))
