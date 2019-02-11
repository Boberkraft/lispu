(defun ask-number ()
	(format t "Please enter a number. ")
	(let ((val (read)))
		(if (numberp val)
			val
			(ask-number))))

(ask-number)

(defun show-squeres (start end)
	(do ((i start (+ i 1)))
		 ((> i end) `done)
	  (Format T "~A ~A ~%" I (* I  I I))))

(show-squeres 2 3)

(Defun Hello-World ()
  (Format t "Helll World!"))

(defun our-member (obj lst)
  (if (null lst)
      nil
      (if (equal (first lst) obj)
          lst
          (our-member obj (rest lst)))))

(our-member 'b '(a b c))

(defun ask-number ()
  (format t "Please enter a number: ")
  (let ((val (read)))
    (if (numberp val)
        val
        (ask-number))))

(setf a b
      c d
      e f)
 
(defun show-squeres (start end)
  (do ((i start (+ i 1)))
      ((> i end) `done)
    (format t "~a ~a ~%" i (* i i))))

(defun show-squares (i end)
  (if (> i end)
      `done
      (progn
        (format t "~a ~a ~%" i (* i i))
        (show-squares (+ i 1) end))))

(show-squeres 2 5)
(show-squares 2 5)

(defun our-length (lst)
  (let ((len 0 ))
    (dolist (obj lst)
      (setf len (+ len 1)))
    len))

(defun our-lenght (lst)
  (if (null lst)
      0
      (+ (our-lenght (rest list)) 1)))

(print (our-length '(2 4 1 2 5)))

(apply #'+ '(1 2 3))
(apply #'+ 1 2 '(1 2 3))

(funcall #'+ 1 2 3)

('exercises)

;; 1.
(+ (- 5 1) (+ 3 7)) ; 14
(list 1 (+ 2 3)) ; (1 5)
(if (listp 1) (+ 1 2) (+ 3 4)) ;7
(list (and   uujnnb(listp 3) t ) (+ 1 2)) ;nil 3


;; 2
(cons 'a '(b c))
(cons 'a (cons 'b '(c)))
(cons 'a (cons 'b (cons 'c nil)))

;;3
(defun czwarty (lst)
  (car (cdr (def (cdr  lst)))))

(czwarty '(1 2 3 4 5 6 7))

;;4
(defun my-max (a b)
  (if (> a b)
      a
      b))

(my-max 5 7)
(my-max 8 7)

;;5

;a zwara t jezeli zawiera nil
(defun enigma (x)
  (and (not (null x))
       (or (null (car x))
           (enigma (cdr x)))))
(enigma `(7 6 1))

(defun mystery (x y)
  (if (null y)
      nil
      (if (eq (car y) x)
          0
          (let ((z (mystery x (cdr y))))
            (and z (+ z 1)))))) ; why?

;; shows on what position is x in y
(mystery 2 '(2 3 4 4 ))

;;6
;; a
(car (car (cdr '(a (b c) d ))))
;car
;; b
                                        ;or
;; c
; apply
(apply #'list 1 nil)

;; 7
(defun contains-list (lst)
  (if (listp (car lst))
      t
      (contains-list (cdr lst))))


 (contains-list '(a b c d (a b c)))

;;8
(defun dots-rec (lst)
  (if (null lst)
      nil
      (dots-rec (cdr lst))))

(defun dots-iter (lst)
  (dolist (_ lst)
    (format t ".")))

(format t "~%")
(dots-iter '(1 2 3 4 5 6))

(defun how-many-rec (a lst)
  (if (null lst)
      0
      (+ (if (eql (car lst) a)
               1
               0)
         (how-many-rec a (cdr lst)))))

(defun how-many-iter (a lst)
  (let ((x 0))
    (dolist (el lst)
      (if (eql el a)
          (setf x (+ x 1))))
    x))

(how-many-rec 2 '(1 2 3 4 5 6 2 2 2 2 45 154))
(how-many-iter 2 '(1 2 3 4 5 6 2 2 2 2 45 154))

;;9
;;a remove operates on a copy, now original list

(defun summit (lst)
  (apply #'+ (remove nil lst)))

(summit '(1 2 45 6))

;; whats the exit condition?
;; it overflows!
(defun summit (lst)
  (if lst ; if its not nil => empty
      (let ((x (car lst)))
        (if (null x)
            (summit (cdr lst))
            (+ x (summit (cdr lst)))))
      0))

(summit '(2 5 6 4 3))
(car nil)

abcdefaa
