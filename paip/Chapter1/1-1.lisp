
;;;Define a version of 1 ast - name that handles " Rex Morgan MD, "
;;; "Morton Downey, Jr.," and whatever other cases you can think of.


(defparameter *last-titles*
  '(Jr. MD))

(defun last-name (name)
  (let ((last-word (first (last name))))
    (if (member last-word
                *last-titles*)
        (last-name (without-last name))
        last-word)))

(defun without-last (lst)
  (if (null (rest lst))
      nil
      (cons (first lst)
            (without-last (rest lst)))))
