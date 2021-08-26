(defun myymap (func &rest args)
  (labels ((rapply (rfunc lst r)
             (cond
               ((atom lst) (funcall rfunc lst))
               ((null (cdr lst)) (rapply rfunc (car lst) r))
               (t (prog (cons r (rapply rfunc (car lst) r)) (cons r (rapply rfunc (cdr lst) r)))))))
    (rapply func args nil)))

(defun mymap (func lst)
  (cond
    ((null lst) nil)
    (t (cons (funcall func (car lst)) (mymap func (cdr lst))))))

(defun checkNil (lsts)
  (if (null lsts) t 
      (and (not (null (car lsts))) (checkNil (cdr lsts)))))

(defun mymultimap (func &rest lsts)
  (labels ((realmap (f ls)
             (if (not (checkNil ls)) nil
                 (cons (myymap f (mymap #'car ls))
                       (realmap f (mymap #'cdr ls))))))
    (realmap func lsts)))
             
(myymap #'(lambda (n) (* 4 n)) (list 1 2 (list 2 3) 4 5))
(mymap #'(lambda (n) (+ 1 n)) (list 6 7 8))
(mymultimap #'(lambda (a b c) (+ a b c)) (list 1 2 3) (list 4 5 6) (list 7 8 9))
(checkNil '(NIL NIL NIL))
