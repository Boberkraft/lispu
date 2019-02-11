;; Modified run-length encoding.
;; Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.

;; Example:
;; * (encode-modified '(a a a a b c c a a d e e e e))
;; ((4 A) B (2 C) (2 A) D (4 E))

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
