;;; 1
((lambda(x)(log (/ (*(cos x) (cos x)) (+ (* (tan x) (tan x) (tan x)) 1)) 2.71828)) 1.5)

((lambda(x)(* (expt (+ (expt x 3) (* 3 x x) (* 4 x) 5) 2) (+ (* x x) (* 7 x) 1))) 2)

;;; 2
(defun f (x y)
  (+ (- (* 4 (expt x 10)) (* 9 (expt y 20))) 2))

(f 3 2)

;;; 3
(defun f1 (n)
  (cond
    ((= n 1) 2)
    ((= n 2) 4)
    (t (+ (* 5 (expt (f1 (- n 1)) 2)) (* 4 (f1 (- n 2)))))))

(defun f2 (v1 v2 n)
  (if (= n 2) v2
    (f2 v2 (+ (* 5 (expt v2 2)) (* 4 v1)) (- n 1))))

(time (f1 10))
(time (f2 2 4 10))

;;; 4
(defun e4-helper (lst n)
  (cond
    ((null lst) nil)
    ((= (rem n 3) 0) (if (atom lst) nil (e4-helper (cdr lst) (+ n 1))))
    ((= (rem n 5) 0) (if (atom lst) (* lst 2) (cons (* (car lst) 2) (e4-helper (cdr lst) (+ n 1)))))
    (t (cons (car lst) (e4-helper (cdr lst) (+ n 1))))))

(defun e4 (lst)
  (e4-helper lst 1))

(e4 (list 1 2 3 4 5 6 7 8 9 10))

;;; 5
(defun e5-helper (lst elem n)
  (cond
    ((null lst) n)
    ((atom lst) (if (= elem lst) (+ n 1) n))
    ((= (car lst) elem) (e5-helper (cdr lst) elem (+ n 1)))
    (t (e5-helper (cdr lst) elem  n))))

(defun e5 (lst elem)
  (e5-helper lst elem 0))

(e5 (list 1 2 1 3 4 1 5 6 7 8 1 19) 1)
