;;;Exercise 2.1 [m] Write a version of generate that uses cond but avoids calling
;;;rewrites twice.


(defun generate2 (phrese)
  (let ((choice))
    (cond ((listp phrase)
           (mappend #'generate phrase))
          ((setf choice (rewrite phrase))
           (generate (random-elt choice)))
          (t (list phrase)))))
