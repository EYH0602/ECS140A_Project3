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

;; would be [0..n] in Haskell
(defun list-to (n)
  (reverse (list-from n)))

(defun fib (n)
  (mapcar #'nth-fib (list-to n)))

; helper function
(defun get-fibs (n xs bound)
  (let ((newFib (nth-fib n)))
    (if (< newFib bound)
      xs
      (cons newFib (get-fibs (+ n 1) xs bound)))))

(defun fib-lt (n)
  (get-fibs 0 nil n))

(print (fib-lt 100))
