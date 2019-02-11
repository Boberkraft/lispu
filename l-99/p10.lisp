;; Run-length encoding of a list.
;; Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.

;; Example:
;; * (encode '(a a a a b c c a a d e e e e))
;; ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))


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
(defun encode (items)
  (cdr (let ((new nil))
         (dolist (item (pack-aux items nil nil)
                  new)
           (print item)
           (push (list (length item) (first item)) new)))))
