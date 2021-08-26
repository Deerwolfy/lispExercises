;;; 1
(defun f (x y)
  (/ (+ (* x x) y) (+ 1 (* x x))))

(defun f1 (lst func)
  (cond
    ((null lst) nil)
    (t (cons 
         (funcall func (caar lst) (cadar lst)) 
         (f1 (cdr lst) func)))))

(f1 '((1 2) (2 3) (5 6) (1 2)) 'f)

;;; 2

(defun findMinHelper (lst localMin)
  (cond
    ((null lst) localMin)
    ((listp (car lst)) (findMinHelper (cdr lst) (findMinHelper (car lst) localMin)))
    ((null localMin) (findMinHelper (cdr lst) (car lst)))
    (t (if (< (car lst) localMin) (findMinHelper (cdr lst) (car lst)) (findMinHelper (cdr lst) localMin)))))

(defun findMin (lst)
  (findMinHelper lst nil))

(findMin '(((2 -30) -10) 3 2 0 ((-40) -1) 1 2))
(findMin '())

;;; 3

(defun firstCharInBoth (lst1 lst2)
  (cond
    ((or (null lst1) (null lst2)) nil)
    ((contains lst2 (car lst1)) (car lst1))
    (t (firstCharInBoth lst2 lst1))))

(defun contains (lst val)
  (cond
    ((null lst) nil)
    ((= (car lst) val) T)
    (t (contains (cdr lst) val))))

(firstCharInBoth '(5 1 2) '(1 2 3))

;;; 4

(defmacro printf (&rest lists)
    (cond
         ((null lists) nil)
         ((null (car lists)) (progn
                              (if (not (null (cdr lists)))
                                  (progn
                                    (terpri t)
                                    (princ "_")))
                              `(printf ,@(cdr lists)))) 
         (t `(progn
                  (princ ,(caar lists))
                  (if (not (null (cdar ',lists))) (princ "_"))
                  (if (not (null (car ',lists))) (printf (,@(cdar lists)) ,@(cdr lists)))))))


(printf (1 2 3 2 3 3) (3 2 1 2 2 3))
