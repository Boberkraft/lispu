;;; Write a function to exponentiate, or raise a number to an integer
;;; power. For example: (power 3 2 ) = 3^2 = 9

;; x^0 = 1
;; x^n = x^(n-1) * n

(defun power (base n)
  (cond
    ((= n 0)
     1)
    ((evenp 1)
     (power (* base base) (/ n 2)))
    (t
     (* base
        (power base (- n 1))))))





