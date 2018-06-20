(defun add-widget (database widget)
  (cons widget
        database))

(defparameter *database* nil)

(defun main-loop ()
  (loop (princ "Please anter the name of a new widget:")
     (setf *database* (add-widget *database* (read)))
     (format t "The database contains the following: ~a~%" *database*)))

(defparameter *my-list* '(4 7 2 3))

;; oh god help
(loop for n below (length *my-list*)
   do (setf (nth n *my-list*)
            (+ (nth n *my-list*)
               2)))


;;def add_two(lst):
;;  if lst:
;;    return [lst[0] + 2] + add_two(lst[1:])
;;  return []
(defun add-two (lst)
  (when lst
    (cons (+ 2 (car lst))
          (add-two (cdr lst)))))
(add-two '( 2 1 5 32))

(mapcar (lambda (x)
          (+ x 2))
        '( 4 7 2 3))

