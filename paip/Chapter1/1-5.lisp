
;;;Write a function to compute the dot product of two sequences
;;;of numbers, represented as lists. The dot product is computed by multiplying
;;;corresponding elements and then adding up the resulting products. Example:


(defun dot-product (a b)
  (apply #'+ (mapcar #'* a b)))

(dot-product '(10 20) '(3 4))
