; helper functions

; dropwhile : (a -> Bool) -> [a] -> [a]
(defun dropwhile (f xs)
  (if (or (null xs) (not (funcall f (car xs))))
      xs
      (dropwhile f (cdr xs))))

; drop : (a -> Bool) -> [a] -> [a]
; drop while f is true and then until f is not true
; drop (!= 1) [2, 3, 1, 1, 1, 4, 5] == [4, 5]
(defun drop (f xs)
  (case xs
        (nil nil)
        (otherwise
         (if (not (funcall f (car xs)))
             (dropwhile (lambda (x) (not (funcall f x))) (cdr xs))
             (drop f (cdr xs))))))

; any : (a -> Bool) -> [a] -> Bool
(defun any (f xs)
  (cond
   ((null xs) nil)
   (t (or (funcall f (car xs)) (any f (cdr xs))))))

; match-base : (Eq a) => a -> (a -> a -> Bool) -> [a] -> [a] -> Bool
; param s: a special symbol of type a, for example * for gr*n, (!) for (green red)
; param f: binary compare operator
; param xs: pattern
; param ys: assertion
(defun match-base (s f xs ys)
  (cond
   ((null ys) t)
   ((null xs) nil)
   ((equal (cons s nil) xs) t)
   ((equal s (car xs))
    (and
     (any (lambda (x) (funcall f (nth 1 xs) x)) ys)
     (match-base s f (cdr (cdr xs)) (drop (lambda (x) (not (funcall f (nth 1 xs) x))) ys))))
   ((funcall f (car xs) (car ys)) (match-base s f (cdr xs) (cdr ys)))
   (t nil)))

(defun matchStr (xs ys)
  (match-base '* #'equal xs ys))

; ABC -> (A B C)
(defun to-char-symbol-list (xs)
  (mapcar #'intern (mapcar #'string (coerce (string-upcase (string xs)) 'list))))

(defun match (xs ys)
  (let ((xss (mapcar #'to-char-symbol-list xs))
        (yss (mapcar #'to-char-symbol-list ys)))
    (match-base '(!) #'matchStr xss yss)))


; test cases
(print (match '(color apple red) '(color apple red)))
(print (match '(color apple red) '(color apple green)))
(print (match '(! table !) '(this table supports a block)))
(print (match '(this table !) '(this table supports a block)))
(print (match '(! brown) '(green red brown yellow)))
(print (match '(! brown) '(green red brown brown)))
(print (match '(red green ! blue) '(red green blue)))
(print (match '(red gr*n blue) '(red green blue)))
(print (match '(t* table is *n) '(this table is blue)))
(print (match '(color apple *) '(color apple red)))
(print (match '(color * red) '(color apple red)))
(print (match '(color * red) '(color apple green)))
(print (match '(color*red) '(color apple red)))
(print (match '(color ! * red) '(color apple red)))
