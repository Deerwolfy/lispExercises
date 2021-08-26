;;; 7
(defun devlev2-helper (lst acc)
  (if (null (cdr lst)) (cons acc (cons (car lst) nil))
      (if (null acc) (devlev2-helper (cdr lst) (cons (car lst) nil))
          (devlev2-helper (cdr lst) (cons acc (cons (car lst) nil))))))

(defun devlev2 (lst)
  (devlev2-helper lst '()))

;;; 8
(defun destlev1 (lst)
  (if (null lst) lst
      (cons (car lst) (destlev1 (cadr lst)))))

;;; 9
(defun remsec-helper (lst is-sec)
  (if (null lst) lst
      (if is-sec (remsec-helper (cdr lst) (not is-sec))
          (cons (car lst) (remsec-helper (cdr lst) (not is-sec))))))

(defun remsec (lst)
  (remsec-helper lst NIL))

(devlev2 (list 1 2 3 4 5))
(destlev1 (list 1 (list 2 (list 3 (list 4 (list 5))))))
(remsec (list 1 2 3 4 5 6 7 8 9 10))
