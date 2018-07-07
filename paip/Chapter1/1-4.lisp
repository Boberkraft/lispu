;;;Write a function that counts the number of times an expression
;;;occurs anywhere within another expression. Example:
;;; ( count - anywhere ' a ' ( a ( ( a ) b) a ) ) -> 3.

(defun count-anywhere  (exp lst)
  (apply #'+ (mapcar (lambda (x)
                       (cond
                         ((equal x exp) 1)
                         ((atom x) 0)
                         (t (count-anywhere exp x))))
                     lst)))

(count-anywhere 'a '(a ((a) b) a))
