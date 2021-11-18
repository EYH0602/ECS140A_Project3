(defun nth-fib (n)
  (case n
        (0 0)
        (1 1)
        (otherwise (+ (nth-fib (- n 1)) (nth-fib (- n 2))))))

; append a new fib number to a existing fib list
(defun append-fib (xs)
  (nconc xs (list (reduce '+ (last xs 2)))))

(defun fib-helper (n xs)
  (if (>= (length xs) n)
      xs
      (fib-helper n (append-fib xs))))

(defun fib (n)
  (fib-helper n '(0 1)))

; helper function
(defun get-fibs (n xs bound)
  (let ((newFib (nth-fib n)))
    (if (>= newFib bound)
        xs
        (cons newFib (get-fibs (+ n 1) xs bound)))))

(defun fib-lt (n)
  (get-fibs 0 nil n))

(print (nth-fib 9))
(print (fib 9))
(print (fib-lt 100))
