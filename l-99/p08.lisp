;; Eliminate consecutive duplicates of list elements.
;; If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.

;; Example:
;; * (compress '(a a a a b c c a a d e e e e))
;; (A B C A D E)


(defun compress (list)
  (let ((result nil)
        (last-el (gensym)))
    (reverse (dolist (el list result)
                (if (not (equal el last-el))
                    (progn
                      (push el result)
                      (setf last-el el)))))))

(defun compress2 (list)
  (cond ((atom list) list)
        ((eql (car list) (cadr list))
         (compress2 (cdr list)))
        (t
         (cons (car list) (compress2 (cdr list))))))
