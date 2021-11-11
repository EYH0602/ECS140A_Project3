(defun match (pattern match)
  ; This is just to show that it works when there's no quotations
  (if (string= (first pattern) (first match))
      t
      NIL))

(defun dropwhile (f xs)
  (if (not (funcall f (car xs)))
    xs
    (dropwhile f (cdr xs))))

(defun drop (f xs)
  (case xs
    (nil nil)
    (otherwise
     (if (not (funcall f (car xs)))
      (dropwhile (lambda (x) (not (funcall f x))) (cdr xs))
      (drop f (cdr xs))))))

(defun any (f xs)
  (cond
    ((null xs) nil)
    (t (or (funcall f (car xs)) (any f (cdr xs))))))

(defun match-base (s f xs ys)
  (cond
    ((null ys) t)
    ((null xs) nil)
    ((eql (cons s nil) xs) t)
    ((eql s (car xs))
      (and
        (any (lambda (x) (funcall f (nth 1 xs) x)) ys) 
        (match-base s f (cdr (cdr xs)) (drop (lambda (x) (not (funcall f (nth 1 xs) x))) ys))))
    ((funcall f (car xs) (car ys)) (match-base s f (cdr xs) (cdr ys)))
    (t nil)))

(defun matchStr (xs ys)
  (match-base '* #'eql xs ys))

(defun match (xs ys)
  (match-base '(!) #'matchStr xs ys))

; You may run the code and the output will be true.
; (print (any (lambda (x) (eq x 's)) '(t h i s)))
(print (matchStr '(*) '(t h i s)))
