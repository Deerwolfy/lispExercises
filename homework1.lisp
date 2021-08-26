;;; 1
(defun substitude (new old lst)
  (if (null lst) nil
      (cons (if (= (car lst) old) new (car lst))
            (substitude new old (cdr lst)))))

;;; 2
;;; found - a boolean value, true if first element already found on that depth, otherwise false
(defun first-atom-helper (lst found)
  (if (null lst) nil
      (if (listp (car lst))
          (cons (first-atom-helper (car lst) NIL) (first-atom-helper (cdr lst) found))
          (if (not found)
              (cons (car lst) (first-atom-helper (cdr lst) T))
              (first-atom-helper (cdr lst) found)))))

(defun first-atom (lst)
  (first-atom-helper lst NIL))

;;; 3
(defun one-bubble (lst)
  "One bubble sort pass"
  (cond
    ((atom lst) lst)
    ((null (cdr lst)) lst)
    ((< (cadr lst) (car lst)) (cons (cadr lst) (one-bubble (cons (car lst) (cddr lst)))))
    (t (cons (car lst) (one-bubble (cdr lst))))))

;;; Essentially a bubble sort
(defun collect-helper (lst bubbled)
  (if (= bubbled (length lst)) lst
      (collect-helper (one-bubble lst) (+ bubbled 1))))

(defun collect (lst)
  (collect-helper lst 0))

(substitude 1 2 (list 7 7 7 2 3 2 1 7 2))
(first-atom (list 1 (list 3 4) 2 (list 1 (list 5 6 (list 7)))))
(first-atom (list (list 1 2) 1 6 7 2 1))
(collect (list 2 1 3 1 1 1 4 3 2 5))
(collect (list 2 3 1 7 8 2 0 0 4 3))
