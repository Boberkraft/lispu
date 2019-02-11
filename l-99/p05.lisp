;; Reverse a list.

(defun my-reverse (list)
  (labels ((rev (list aux)
             (if list
                 (rev (cdr list)
                      (cons (car list) aux))
                 aux)))
    (rev list '())))

