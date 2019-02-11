
(defstruct name
  first
  (middle nil)
  last)

(setf person (make-name :first 'Barney :last 'Rubble))

(name-first person)
(name-middle person)

(name-p person)

(setf (name-middle person) 'DUPA)

(cond ((> 20 100)
       (princ "N is large."))
      (t
       (princ "N is not large.")))

(when (> 10 100)
  (princ "N is large."))

(case 1
  (1 10)
  (2 20))

(cond
  ((eql 1 1) 10)
  ((eql 1 2) 20))

(typecase 3
  (number (abs 3))
  (list (length '(1 2 3))))

(cond
  ((typep 1 'number) (abs 1))
  ((typep 1 'list) (length '(1 2 3))))

; Exercise 3.1
(let* ((x 6)
       (y (* x x)))
  (+ x y))
;As lambda exp.

((lambda (x)
 ((lambda (y)
    (+ x y))
  (* x x)))
 6)

(pop list)
(let ((result (first list)))
  (setf list (rest list))
  result)


(defstruct player (score 0) (wins 0))

(defun determine-winner (player)
  (incf (player-wins (first (sort players #'>
                                  :key #'player-score)))))

(defun determine-winner2 (player)
  (let ((winner (first (sort players #'> :key #'player-score))))
    (setf (player-wins winner) (+ 1
                                  (player-wins winner)))))

(defun length1 (list)
  (let ((len 0))
    (dolist (element list)
      (incf len))
    len))

(defun lenght1.1 (list)
  (let ((len 0))
    (dolist (element list len)
      (incf len))))

(defun length2 (list)
  (let ((len 0))
    (mapc #'(lambda (element)
              (incf len))
          list)
    len))

(defun length3 (list)
  (do ((len 0 (+ len 1))
       (l list (rest l)))
      ((null l) len)))

(defun lenght4 (list)
  (loop for element in list
     count t))

(defun length5 (list)
  (loop for element in list
     summing 1))

(defun lenght6 (list)
  (loop with len = 0
     until (null list)
     for lement = (pop list)
     do (incf len)
     finally (return len)))


(defun true (x) t)

(defun length7 (list)
  (count-if #'true list))

(defun length8 (list)
  (if (null list)
      0
      (+ 1 (position-if #'true list :from-end t))))

(setf a '(1 2 3 4 5))

(delete 2 a)

(defun length9 (list)
  (if (null list)
      0
      (+ 1 (length9 (rest list)))))

(defun length11 (list &optional (len-so-far 0))
  (if (null list)
      len-so-far
      (length11 (rest list) (+ 1 len-so-far))))

(defun length12 (the-list)
  (labels
      ((length13 (list len-so-far)
         (if (null list)
             len-so-far
             (length13 (rest list) (+ 1 len-so-far)))))
    (length13 the-list 0 )))

(defun product (numbers)
  (let ((prod 1))
    (dolist (n numbers prod)
      (if (= n 0)
          (return 0)
          (setf prod (* n prod))))))

(defmacro while (test &rest body)
  (list* 'loop
         (list 'unless test '(return nil))
         body))


(defmacro while1 (test &rest body)
  (let ((code '(loop (unless test (return nil)) . body)))
    (subst test 'test (subst body 'body code))))

(defmacro while2 (test &rest body)
  `(loop (unless ,test (return nil))
      ,@body))
