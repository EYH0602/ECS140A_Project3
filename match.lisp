(defun match (pattern match)
	; This is just to show that it works when there's no quotations
	(if (string= (first pattern) (first match))
		t
		NIL
	)
)

; You may run the code and the output will be true.
(print (match '(apple2) '(apple2 banana)))
