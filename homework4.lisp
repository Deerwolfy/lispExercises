;;; 10
(defun devpair (lst)
  (if (null lst) lst
      (if (atom lst) (cons lst nil)
          (cons (cons (car lst) (cons (cadr lst) nil)) (devpair (cddr lst))))))

;;; 11 in another file

;;; 12
(defun forall (func lst)
  (if (null lst) T
      (and (funcall func (car lst)) (forall func (cdr lst)))))

;;; 13
(defun forsome (func lst)
  (if (null lst) nil
      (or (funcall func (car lst)) (forsome func (cdr lst)))))

(devpair (list 1 2 3 4 5 6 7))
(forall #'(lambda (x) (= x 3)) (list 3 3 3 2))
(forsome #'(lambda (x) (= x 3)) (list 1 3 1))
