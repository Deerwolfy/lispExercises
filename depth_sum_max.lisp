(defun maxOf (num1 num2)
  (if (> num1 num2) num1 num2))

(defun depth (lst)
  (labels ((findDepth (lst cdepth)
             (if (atom lst) cdepth 
                 (maxOf (findDepth (car lst) (+ cdepth 1)) (findDepth (cdr lst) cdepth))))) 
    (findDepth lst 0)))

(defun sum (lst)
  (if (null lst) 0
      (if (atom lst) lst
          (+ (sum (car lst)) (sum (cdr lst))))))

(defun maxVal (lst)
  (labels ((maxOf (num1 num2) (if (null num1) num2 (if (null num2) num1 (if (> num1 num2) num1 num2)))))
    (if (atom lst) lst (maxOf (maxVal (car lst)) (maxVal (cdr lst))))))

(depth (list 1 (list (list 2 nil) 1 2)))
(depth (list 1 (list (list 2) 3) (list 4) 5 (list 6 100 (list (list 7) (list (list 8)) 9) 10)))
(depth (list (list 2 3) (list 1 2)))

(sum (list 1 (list (list 2 nil) 1 2)))
(sum (list 1 (list (list 2) 3) (list 4) 5 (list 6 100 (list (list 7) (list (list 8)) 9) 10)))
(sum (list (list 2 3) (list 1 2)))

(maxVal (list 1 (list (list 2 nil) 1 2)))
(maxVal (list 1 (list (list 2) 3) (list 4) 5 (list 6 100 (list (list 7) (list (list 8)) 9) 10)))
(maxVal (list (list 2 3) (list 1 2)))
