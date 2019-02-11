;; Find the number of elements of a list.


(defun my-length (list)
  (labels ((temp (lst n)
             (if (endp lst)
                 n
                 (temp (rest lst) (1+ n)))))
    (temp list 0)))
