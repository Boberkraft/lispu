;; Pack consecutive duplicates of list elements into sublists.
;; If a list contains repeated elements they should be placed in separate sublists.

;; Example:
;; * (pack '(a a a a b c c a a d e e e e))
;; ((A A A A) (B) (C C) (A A) (D) (E E E E))

(defun pack-aux (items new-group so-far)
  (let ((item (first items)))
    (cond
      ((null item) (cons new-group so-far))
      ((eql item (car new-group))
       (pack-aux (rest items)
                 (cons item new-group)
                 so-far))
      (t
       (pack-aux (rest items)
                 (list item)
                 (cons new-group so-far))))))
(defun pack (items)
  (cdr (reverse (pack-aux items nil nil))) )

