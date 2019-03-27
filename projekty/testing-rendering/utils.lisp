(defpackage #:utils
  (:use #:cl))

(defparameter *stepper* (make-stepper 0.3))

(defun make-stepper (time)
  "Useful helper. (Stepper-can-p) returns True only if internal timer reached 0.
   (stepper-reset) sets this timer to the default value of 'to-wait."
  (let ((old-time 0)
        (to-wait time))
    (lambda (&optional reset)
      '(print (- (now) old-time))
      (cond
        (reset (setf old-time (now))) ;reset timer
        (t
         (< to-wait (- (now) old-time))
         )))))

(defun stepper-reset ()
  (funcall *stepper* t))
(defun stepper-can-p ()
  (funcall *stepper*))


(defun get-color-v-for-block (sym)
  (case sym
    (tetris:A (v! 0.27 0.54 0.4 ))
    (tetris:B (v! 1.0  0.54 0.65))
    (tetris:C (v! 1.0  0.69 0.23))
    (tetris:D (v! 0.71 0.29 0.15))
    (tetris:E (v! 0.56 0.16 0.0 ))
    (tetris:F (v! 0.0  0.69 0.23))
    (tetris:G (v! 0.71 0.29 1   ))
    (tetris:H (v! 0.27  0.4 0.7 ))
    (tetris:X (v! 0    1    0   ))
    (tetris:* (v! 1    0    0   ))
    (tetris:- (v! 1.01 0.50 1.00))
    (grey     (v! 0.1  0.1  0.1))
    (+        (v! 0.05  0.05  0.05))
    (otherwise (error  (format nil "color for block: ~a not found!" sym)))))

(defun now ()
  (/ (float (get-internal-real-time))
     500))
