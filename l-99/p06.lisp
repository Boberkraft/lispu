;; Find out whether list is a palindrome.
;; A palindrome can be read forward or backward; e.g. (x a m a x).




(defun palindrome (list)
  (equal list (reverse list)))

