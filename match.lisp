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


(defun match-base (s f xs ys))

; You may run the code and the output will be true.
(print (drop (lambda (x) (/= x 4)) '(1 2 3 4 4 4 4 4 5 6 7)))
