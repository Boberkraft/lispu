;;;; testing-rendering.lisp

(ql:quickload :testing-rendering)
(push #p"C:/Users/Bobi/Desktop/lispu/projekty/testing-rendering/" asdf:*central-registry*) 
(in-package #:testing-rendering)


(load "C:/Users/Bobi/Desktop/lispu/projekty/testing-rendering/tetris.lisp")

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

(defvar *buf-stream* nil)
(defvar *gpu-arr* nil)

(defparameter *rotate-latch* nil)
(defparameter *stepper* (make-stepper 0.3))


(defun get-color-v-for-block (sym)
  (case sym
    (A (v! 0.27 0.54 0.4 ))
    (B (v! 1.0  0.54 0.65))
    (C (v! 1.0  0.69 0.23))
    (D (v! 0.71 0.29 0.15))
    (E (v! 0.56 0.16 0.0 ))
    (F (v! 0.0  0.69 0.23))
    (G (v! 0.71 0.29 1   ))
    (H (v! 0.27  0.4 0.7 ))
    (X (v! 0    1    0   ))
    (* (v! 1    0    0   ))
    (- (v! 1.01 0.50 1.00 ))
    (otherwise (error  "color for block not found!"))))

(defun-g some-vert-stage-z ((vert g-pnt)
                            &uniform (now :float)
                            (perspective :mat4)
                            (cords :vec3)
                            (block-color :vec3))
  (let* ((pos (pos vert))
         (color (+ (pos vert)
                   (v! 0.5 0.5 0.5)))
         (pos (+ pos cords))
         (pos (+ pos (v! 0
                         0
                         (+ 0
                            -11)))))
    (values (* perspective (v! pos 10))
            (+ (/ 1
                  5)
               (* block-color
                  (+ 0 (z (pos vert))
                     (if (= 0 (z cords))
                         1.0
                         1.0
                         ))))
            )))

(defun now ()
  (/ (float (get-internal-real-time))
     500))

(defun-g some-frag-stage ((color :vec3))
  color)

(defpipeline-g some-pipeline ()
  (some-vert-stage-z g-pnt)
  (some-frag-stage :vec3))

(defun my-tr (x y z)
  (v! (v:+ (v! -4.5 10)
           (v! x
               (* -1 y)))
      (+ -10 z)))


(defun advanced-repl ()
  (when (keyboard-button (keyboard) key.a)
    (left)
    (stepper-reset))
  (when (keyboard-button (keyboard) key.s)
    (down)
    (stepper-reset))
  (when (keyboard-button (keyboard) key.d)
    (right)
    (stepper-reset))
  (when (and (keyboard-button (keyboard) key.r)
             (not *rotate-latch*))
    (rotate)
    (setf *rotate-latch* nil)
    (stepper-reset)))

(defun draw ()
  (step-host)
  (setf (resolution (current-viewport))
        (surface-resolution (current-surface (cepl-context))))
  (clear)
  (when (stepper-can-p)
    (advanced-repl))
  ;; draw current shape
  (loop for row below (length *curr-shape*)
     do (loop for column below (length (car *curr-shape*))
           for s = (symbol-at column
                              row
                              *curr-shape*)
           when (not (eql s '-))
           do (draw-box (+ *curr-column* column)
                        (+ *curr-row* row)
                        0
                        (get-color-v-for-block s))))
  ;; draw map
  (loop for row below +height+
     do (loop for column below +width+
           for s = (symbol-at column
                              row
                              *map*)
           if (eq s '-)
           do      (draw-box column row -1 (get-color-v-for-block s))
           else do (progn
                     '(draw-box column row -2 (get-color-v-for-block '-))
                     (draw-box column row 0 (get-color-v-for-block s)))))
  (swap))

(defun draw-box (column row depth color)
  (map-g #'some-pipeline *buf-stream*
         :now (now)
         :perspective (rtg-math.projection:perspective
                       (x (resolution (current-viewport)))
                       (y (resolution (current-viewport)))
                       0.1
                       30f0
                       60f0)
         :cords (my-tr column row depth)
         :block-color color
         ))


(defun init ()
  (unless *buf-stream*
    (destructuring-bind (vert index)
        (nineveh.mesh.data.primitives:box-gpu-arrays)
      (setf *buf-stream*
            (make-buffer-stream vert :index-array index)))))


(def-simple-main-loop play (:on-start #'init)
  (draw))



(defun main ()
  (create-computer)
  (loop while (not *game-over*)
     do (draw)))
