(make-array 3)
(defparameter x (make-array 3))
(aref x 1)

(setf (aref x 1) 'foo)
(aref x 1)

(setf foo (make-array 4))

(setf (aref foo 2) '(x y z))

(setf (car (aref foo 2)) (make-hash-table))
(setf (gethash 'zoink (car (aref foo 2))) 5)

(defparameter x (make-hash-table))

(gethash 'yup x)

(setf (gethash 'yup x) 25)

(gethash 'yup x)



(defparameter *drink-order* (make-hash-table))

(setf (gethash 'bill *drink-order*) 'double-espresso)
(setf (gethash 'lisa *drink-order*) 'small-drip-coffee)
(setf (gethash 'john *drink-order*) 'medium-late)

(gethash 'lisa *drink-order*)

(round 2.4)

(defun foo ()
  (values 3 7))
(foo)

(multiple-value-bind (a b) (foo)
  (* a b))

(defstruct person
  name
  age
  waist-size
  favorite-color)

(defparameter *bob* (make-person :name "Bob"
                                 :age 35
                                 :waist-size 32
                                 :favorite-color "blue"))

(length '(a b c))
(length "Blub")
(length (make-array 5))

(find-if #'numberp '(a b 5 d))
(count #\s "mississippi")

(position #\4 "2kewkl4skewl")

(some #'numberp '(a b 5 d))
(every #'numberp '(a b 5 d))


(reduce #'+ '(3 4 6 5 2))

(reduce (lambda (best item)
          (if (and (evenp item)
                   (> item best))
              item
              best))
        '(7 4 6 5 2)
        :initial-value 0)

(defun sum (lst)
  (reduce #'+ lst))

(sum (make-array 5 :initial-contents '(1 2 3 4 5)))

(map 'string
     (lambda (x)
       (if (eq x #\s)
           #\S
           x))
     "this this is a string")

(subseq "america" 2 6)
(sort '(5 8 2 4 9 3 6) #'<)

(defun add (a b)
  (cond ((and (numberp a)
              (numberp b))
         (+ a b))
        ((and (listp a)
              (listp b))
         (append a b))))

(defmethod add-my ((a number)
                   (b number))
  (+ a b))

(defmethod add-my ((a list)
                   (b list))
  (append a b))


(add-my 3 6)

(add-my '(a b) '(c d))
