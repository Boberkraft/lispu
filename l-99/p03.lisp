;; (*) Find the K'th element of a list.
;; The first element in the list is number 1.
;; Example:
;; * (element-at '(a b c d e) 3)
;; C

(defun element-at (list position)
  (nth (1- position) list))

(defun element-at2 (list position)
  (do ((i 1 (1+ i))
       (lst list (rest list)))
      ((= i position) (car lst))))
