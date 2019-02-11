;; (*) Find the last but one box of a list.
;; Example:
;; * (my-but-last '(a b c d))
;; (C D)

(defun my-but-last (lst n)
  (do ((lst1 lst (rest lst1))
       (lst2 lst)
       (i 0 (1+ i)))
      ((endp lst1) lst2)
    (if (>= i n)
        (setq lst2 (rest lst2)))))
