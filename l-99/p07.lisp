
;; Flatten a nested list structure.
;; Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).

;; Example:
;; * (my-flatten '(a (b (c d) e)))
;; (A B C D E)

;; Hint: Use the predefined functions list and append.

(defun my-flatten (list)
  (append (mapcan (lambda (el)
                    (if (atom el)
                        (list el)
                        (my-flatten el)))
                  list)))


(defun my-flatten2 (lst)
  (if (not (atom lst))
      (apply #'append (mapcar #'my-flatten2 lst))
      (list lst)))

(defun my-flatten3 (lst)
  (if (not (atom lst))
      (apply #'append (mapcar #'my-flatten2 lst))
      (list lst)))
