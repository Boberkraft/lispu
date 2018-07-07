(defparameter *foo* (lambda ()
                      5))

(funcall *foo*)

(defparameter *foo* (let ((x 5))
                      (lambda ()
                        x)))

(funcall *foo*)

(let ((line-number 0))
  (defun my-print (x)
    (print line-number)
    (print x)
    (incf line-number)
    nil))

(my-print "This")
(my-print "is")

(defun xd (x y)
  (declare (type unsigned-byte
                 x
                 y)
           (optimize (safety 0) (space 3) (debug 0) (speed 0)))
  (if (> x y)
      (if (> x 0)
          x
          y)
      y))
(disassemble #'xd)

(progn
  (defun my-length (lst)
    (if lst
        (1+ (my-length (cdr lst)))
        0))
  (defparameter *biglist* (loop for i below 100000
                             collect 'x)))

(my-length '(fie foh fum))

(defun my-length2 (lst)
  (labels ((f (lst acc)
             (if lst
                 (f (cdr lst) (1+ acc))
                 acc)))
    (f lst 0)))

(my-length2'(fie foh fum))



