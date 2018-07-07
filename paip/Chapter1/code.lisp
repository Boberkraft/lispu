(defparameter *titles*
  '(Mr Mrs Miss Ms Sir Madam Dr Admiral major General))

(defun last-name (name)
  (first (last name)))

(defun first-name (name)
  (first name))



(defparameter names '((Johny Q Public)
              (Malcolm X)
              (Madam Major Admiral Grzegorz Biedronka)
              (Burek)
              (Arystoteles)
              (Sir Lary Olivier)))


(defun first-name (name)
  (if (member (first name) *titles*)
      (first-name (rest name))
      (first name)))

(defun mappend (fn the-list)
  (apply #'append (mapcar fn the-list)))



(defun self-and-double (x) (list x (+ x x)))



(defun number-and-negation (x)
  (if (numberp x)
      (list x (- x))
      nil))


(defun number-and-negations (input)
  (mappend #'number-and-negation input))

(defun mappend (fn the-list)
  (if (null the-list)
      nil
      (append (funcall fn (first the-list))
              (mappend fn (rest the-list)))))

