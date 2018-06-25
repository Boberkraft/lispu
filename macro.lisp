(defun add (a b)
  (let ((x (+ a b)))
    (format t "The sum is ~a" x)
    x))

'(add 2 3)

(defmacro let1 (var val &body body)
  `(let ((,var ,val))
     ,@body))


(let ((foo (+ 2 4)))
  (* foo foo))

(let1 foo (+ 2 3)
  (* foo foo))

(let1 foo (+ 2 3)
  (princ "Lisp is awsome!")
  (* foo foo))

(defun add-my (a b)
  (let1 x (+ a b)
    (format t "The sum is ~a" x)
    x))

'(add-my 2 3)

'(macroexpand '(let1 foo (+ 2 3)
               (* foo foo)))

(defun my-length (lst)
  (labels ((f (lst acc)
             (if lst
                 (f (cdr lst) (1+ acc))
                 acc)))
    (f lst 0)))

(defmacro split (val yes no)
  `(if ,val
       (let ((head (car ,val))
             (tail (cdr ,val)))
         ,yes)
       ,no))

(defun my-length2 (lst)
  (labels ((f (lst acc)
             (split lst
                    (f tail (1+ acc))
                    acc)))
    (f lst 0)))


'(macroexpand '(split (progn (format t "Lisp rocks!~%")
                            '(2 3))
               (format t "This can be split into ~a and ~a~%" head tail)
               (format t "This cannot be split.~%")))

(defmacro split3 (val yes no)
  `(let1 x ,val
     (if x
         (let ((head (car x))
               (tail (cdr x)))
           ,yes)
         ,no)))

(split3 (progn (format t "Lisp rocks!~%")
               '(2 3))
        (format t "This can be split into ~a and ~a~%" head tail)
        (format t "This cannot be split.~%"))

(let1 x 100
  (split3 '(2 3)
          (+ x head)
          nil))

(macroexpand '(split3 '(2 3)
               (+ x head)
               nil))

(defmacro split (val yes no)
  (let1 g (gensym)
        `(let1 ,g ,val
               (if ,g
                   (let ((head (car ,g))
                         (tail (cdr ,g)))
                     ,yes)
                   ,no))))

(macroexpand '(split4 '(2 3)
               (+ x head)
               nil))
(defun my-length (lst)
  (labels ((f (lst acc)
             (split lst
                    (f tail (1+ acc))
                    acc)))
    (f lst 0)))

(my-length '(1 2 3))

(defun pairs (lst)
  (labels ((f (lst acc)
             (split lst
                    (if tail
                        (f (cdr tail) (cons (cons head
                                                  (car tail))
                                            acc))
                        (reverse acc))
                    (reverse acc))))
    (f lst nil)))
(pairs '(a b c d e f))

(defmacro recurse (vars &body body)
  (let1 p (pairs vars)
    `(labels ((self ,(mapcar #'car p)
                ,@body))
       (self ,@(mapcar #'cdr p)))))

(recurse (n 9)
  (fresh-line)
  (if (zerop n)
      (princ "Lift-off!")
      (progn (princ n)
             (self (1- n)))))

(defun my-length (lst)
  (recurse (lst lst ;
                acc 0
                )
    (split5 lst
            (self tail (1+ acc))
            acc)))

(defun my-length10 (lst)
  (reduce (lambda (x i)
            (1+ x))
          lst
          :initial-value 0))

(my-length10 '(1 2 3 4))



(defun print-tag (name alst closingp)
  (princ #\<)
  (when closingp
    (princ #\/))
  (princ (string-downcase name))
  (mapc (lambda (att)
          (format t " ~a=\"~a\"" (string-downcase (car att)) (cdr att)))
        alst)
  (princ #\>))

(print-tag 'mytag '((color . blue) (height . 9)) nil)
(print-tag 'mytag nil t)


(defmacro tag (name atts &body body)
  `(progn (print-tag ',name
                     (list ,@(mapcar (lambda (x)
                                       `(cons ',(car x) ,(cdr x)))
                                     (pairs atts)))
                     nil)
          ,@body
          (print-tag ',name nil t)))


(let1 data '(1 2 3 4 5 6 7 8 9 10)
  (mapcar (lambda (x)
            `(cons ',(car x) ,(cdr x)))
          (pairs data)))

(princ (macroexpand '(tag mytag
                      (color 'blue
                       height (+ 4 5)))))

(tag mytag (color 'blue size 'big)
  (tag first_inner_tag ())
  (tag second_inne_tag ()))

(print (macroexpand '(tag mytag (color 'blue size 'big)
                      (tag first_inner_tag ())
                      (tag second_inne_tag ()))))

(tag html ()
  (tag body ()
    (princ "Hello world!")))

(defmacro html (&body body)
  `(tag html ()
     ,@body))

(defmacro body (&body body)
  `(tag body ()
     ,@body))

'(html
  (body
    (princ "Hello world!")))

(defmacro svg (&body body)
  `(tag svg  (xmlns "http://www.w3.org/2000/svg"
              "xmlns:xlink" "http://www.w3.org/1999/xlink")
        ,@body))

(defun brightness (col amt)
  (mapcar (lambda (x)
            (min 255 (max 0 (+ x amt))))
          col))

'(brightness '(255 0 0) -100)

(defun svg-style (color)
  (format nil
          "~{fill:rgb(~a,~a,~a);stroke:rgb(~a,~a,~a)~}"
          (append color
                  (brightness color -100))))

'(svg-style '(255 255 255))

(defun circle (center radius color)
  (tag circle (cx (car center)
               cy (cdr center)
               r radius
               style (svg-style color))))

(svg (circle '(50 . 50)
             50
             '(255 0 0))
     (circle '(100 . 100)
             50
             '(0 0 255)))
(defun polygon (points color)
  (tag polygon (points (format nil
                      "~{~a,~a ~}"
                      (mapcan (lambda (tp)
                                (list (car tp) (cdr tp)))
                              points))
                       style (svg-style color))))

(defun random-walk (value length)
  (unless (zerop length)
    (cons value
          (random-walk (if (zerop (random 2))
                           (1- value)
                           (1+ value))
                       (1- length)))))

(random-walk 100 10)

(with-open-file (*standard-output* "random_walk.svg"
                                   :direction :output
                                   :if-exists :supersede)
  (svg (loop repeat 10
          do (polygon (append '((0 . 200))
                              (loop for x from 0
                                 for y in (random-walk 100 400)
                                 collect (cons x y))
                              '((400 . 200)))
                      (loop repeat 3
                           collect (random 256))))))
