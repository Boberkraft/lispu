
;; P01 (*) Find the last box of a list.
;; Example:
;; * (my-last '(a b c d))
;; (D)

(defun my-last (lst)
  (do ((lst1 lst (rest lst1))
       (lst2 lst))
      ((endp lst1) lst2)
    (setq lst2 lst1)))
