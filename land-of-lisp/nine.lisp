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


