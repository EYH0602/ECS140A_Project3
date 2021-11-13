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
    ((equal (cons s nil) xs) t)
    ((eql s (car xs))
      (and
        (any (lambda (x) (funcall f (nth 1 xs) x)) ys) 
        (match-base s f (cdr (cdr xs)) (drop (lambda (x) (not (funcall f (nth 1 xs) x))) ys))))
    ((funcall f (car xs) (car ys)) (match-base s f (cdr xs) (cdr ys)))
    (t nil)))

(defun matchStr (xs ys)
  (match-base '* #'equal xs ys))

(defun to-char-symbol-list (xs)
  (mapcar #'intern (mapcar #'string (coerce (string-upcase (string xs)) 'list))))

(defun match (xs ys)
  (match-base '(!) #'matchStr (mapcar #'to-char-symbol-list xs) (mapcar #'to-char-symbol-list ys)))

; You may run the code and the output will be true.
; (print (match '(color apple red) '(color apple red)))
; (print (match '(color apple red) '(color apple green)))
; (print (match '(! table !) '(this table supports a block)))
; (print (match '(this table !) '(this table supports a block)))
; (print (match '(! brown) '(green red brown yellow)))
; (print (match '(! brown) '(green red brown brown)))
; (print (match '(red green ! blue) '(red green blue)))
; (print (match '(red gr*n blue) '(red green blue)))
; (print (match '(t* table is *n) '(this table is blue)))
; (print (match '(color apple *) '(color apple red)))
; (print (match '(color * red) '(color apple red)))
; (print (match '(color * red) '(color apple green)))
; (print (match '(color*red) '(color apple red)))
; (print (match '(color ! * red) '(color apple red)))

(print (mapcar #'to-char-symbol-list '(! table !)))

; (let ((xs (to-char-symbol-list 'a*le)) (ys (to-char-symbol-list 'apple)))
;   (print (matchStr xs ys)))
