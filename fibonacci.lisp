(defun nth-fib (n)
  (case n
        (0 0)
        (1 1)
        (otherwise (+ (nth-fib (- n 1)) (nth-fib (- n 2))))))

; list generators
(defun list-from (n)
  (case n
        (0 '(0))
        (1 '(0))
        (otherwise (cons (- n 1) (list-from (- n 1))))))

;; would be [0..n-1] in Haskell
(defun list-to (n)
  (reverse (list-from n)))

; a different version getting nth fib
(defun nfib (n)
  (case n
        (0 0)
        (1 1)
        (otherwise
         (let* ((k (floor n 2))
                (f1 (nfib k))
                (f2 (nfib (- k 1))))
           (cond
            ((evenp n) (* f1 (+ f1 (* 2 f2))))
            ((= 1 (mod n 4)) (+ 2 (* (+ (* 2 f1) f2) (- (* 2 f1) f2))))
            (t (- (* (+ (* 2 f1) f2) (- (* 2 f1) f2)) 2)))))))

(defun fib (n)
  (mapcar #'nfib (list-to n)))

; helper function
(defun get-fibs (n xs bound)
  (let ((newFib (nth-fib n)))
    (if (>= newFib bound)
        xs
        (cons newFib (get-fibs (+ n 1) xs bound)))))

(defun fib-lt (n)
  (get-fibs 0 nil n))

(print (fib 9))
(print (fib-lt 100))
