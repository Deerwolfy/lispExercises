; 2
(defun flatten (lst)
  (cond
    ((null lst) nil)
    ((listp (car lst)) (append (flatten (car lst)) (flatten (cdr lst))))
    (t (cons (car lst) (flatten (cdr lst))))))

; 3
(defun remsec-helper (lst is-sec)
  (if (null lst) lst
      (if is-sec (remsec-helper (cdr lst) (not is-sec))
          (cons (car lst) (remsec-helper (cdr lst) (not is-sec))))))

(defun remsec (lst)
  (remsec-helper lst NIL))

; 4
(defmacro myAllNumbers (l)
  (cond
    ((null l) T)
    (`(and (numberp (car ',l)) (myAllNumbers (,@(cdr l)))))))

(defmacro alnum (l)
  (if (null (member nil (mapcar 'numberp l))) t nil))

(flatten '(((3) 2) 1))
(remsec '(1 2 3 4 5 6 7 8 9 10))
(myAllNumbers (1 "_" 3))
(alnum (1 "_" 3))
(myAllNumbers (1 2 3))
(alnum (1 2 3))
(myAllNumbers (1 (list 1) 3))
(alnum (1 (list 1) 3))
