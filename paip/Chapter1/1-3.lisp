;;;Write a function that counts the number of atoms in an expression.
;;;For example: ( count - a toms ' ( a ( b ) c ) ) = 3. Notice that there is something of an
;;;ambiguity in this: should ( a n i 1 c ) count as three atoms, or as two, because it is
;;;equivalent to ( a ( 1 c ) ?


(defun count-atoms (lst)
  (apply #'+ (mapcar (lambda (x)
                       (if (atom x)
                           1
                           (count-atoms x)))
                     lst)))


(defun count-atoms2 (lst)
  (cond
    ((atom lst) 1)
    ((null (rest lst)) (count-atoms2 (first lst)))
    (t (+ (count-atoms2 (first lst))
          (count-atoms2 (rest lst))))))
