;;;; testing-rendering.lisp

(defpackage #:testing-rendering
  (:use #:cl
        #:cepl
        #:rtg-math
        #:nineveh
        #:varjo
        #:vari
        #:cepl.skitter.sdl2
        #:livesupport
        #:utils
        #:tetris-structures
        ))

(in-package :testing-rendering)

#+nil (progn
        (swank:set-default-directory "c:\\Users\\Bobi\\Desktop\\lispu\\projekty\\testing-rendering\\")
        (push #p"C:/Users/Bobi/Desktop/lispu/projekty/testing-rendering/" asdf:*central-registry*)
        (ql:quickload :tetris)
        (in-package #:tetris))

(defvar *buf-stream* nil)
(defvar *gpu-arr* nil)
(defparameter *render-state* nil "Instance of rendering-state")

(defclass render-state ()
  ((animation-timer
    :accessor animation-timer
    :initform 0)
   (time-before-draw
    :accessor time-before-draw
    :initform (now))
   (animation-color
    :accessor animation-color
    :initform (get-color-v-for-block '+))))

(setf *render-state* (make-instance 'render-state))

;;;;;;;;;;;;;;;;;;;;;;;; TETRIS BLOCKS

(defun-g some-vert-stage-z ((vert g-pnt)
                            &uniform (now :float)
                            (perspective :mat4)
                            (cords :vec3)
                            (block-color :vec3)
                            (width :int)
                            (height :int))
  (let* ((pos (pos vert))
         (pos (+ pos
                 (v! 0.5 -0.5 0.5)))
         (pos (* pos
                 (v! width height 0)))
         (pos (+ pos cords ))
         (pos (+ pos (v! 0
                         0
                         (+ 0
                            -11)))))
    (values (* perspective (v! pos 1))
            (+ (/ 1
                  5)
               (* block-color
                  (+ 0 (z (pos vert))
                     (if (= 0 (z cords))
                         1.0
                         1.0
                         ))))
            )))


(defun-g some-frag-stage ((color :vec3))
  color)

(defpipeline-g some-pipeline ()
  (some-vert-stage-z g-pnt)
  (some-frag-stage :vec3))

;;;;;;;;;;;;;;;;;;;; BACKGROUND

(defun-g wall-vert-stage ((vert g-pnt)
                          &uniform (now :float)
                          (perspective :mat4)
                          (cords :vec3)
                          (block-color :vec3)
                          (width :int)
                          (height :int))
  (let* ((pos (v:s~ (pos vert) :xyz))
         (org-pos (v:s~ pos :xy))
         (pos (+ pos
                 (v! 0.1 0 0.5)))
         (cords2 (v! pos 1))
         (pos (* pos
                 (v! 5 10 -42.0005))))
    (values (* perspective (v! pos 1))
            (/ (+ org-pos (v! 1 1))
               2))))

(defun-g wall-frag-stage ((pos :vec2)
                          &uniform
                          (multipler :float)
                          (color :vec3))
  ;;#+nil(v! (perlin-noise (* (v:swizzle color :xy) 1))
  ;;         0)
  (let* ((pos (v:s~ pos :yyy))
         (grey-scale (- (v! 1 1 1) pos))

         (colored-vector (* grey-scale
                            color))
         (grey-background (/ grey-scale 5)))

    (v! (+ (* grey-background (- 1 multipler))
           (* colored-vector multipler))
        )))

(defpipeline-g wall-pipeline ()
  (wall-vert-stage g-pnt)
  (wall-frag-stage :vec2))
;;;;; 
(defun my-tr (x y z)
  "Translation function that puts stuff at the back"
  (v! (v:+ (v! -4.5 10)
           (v! x
               (* -1 y)))
      (+ -10 z)))

;; ------    PLAYER INPUT


(defun advanced-repl ()
  "Handles user input"
  ;; LEFT
  (when (keyboard-button (keyboard) key.a)
    (tetris:left)
    (stepper-reset))
  ;; DOWN
  (when (keyboard-button (keyboard) key.s)
    (tetris:down)
    (stepper-reset))
  ;; RIGHT
  (when (keyboard-button (keyboard) key.d)
    (tetris:right)
    (stepper-reset)))

(defun rotate ()
  (format t "~%Rotating")
  (tetris:rotate))

(defun drop-down ()
  (tetris:drop-down))

(defun toggle-on-off ()
  (sounds:toggle-on-off))

;;;;;;;;; --------  DRAWNINI
(defun draw ()
  (step-host)
  (let ((now (now)))
    (setf (animation-timer *render-state*) (+ (animation-timer *render-state*)
                                              (- now (time-before-draw *render-state*)))
          (time-before-draw *render-state*) now))

  (setf (resolution (current-viewport))
        (surface-resolution (current-surface (cepl-context))))
  (clear)

  (when (stepper-can-p)
    (advanced-repl))

  (draw-wall tetris:+width+  tetris:+height+
             (animation-color *render-state*)
             (animation-timer *render-state*))

  ;; draw current shape
  (loop for row below (length (tetris:get-current-shape))
     do (loop for column below (length (car (tetris:get-current-shape)))
           for s = (tetris:symbol-at column
                                     row
                                     (tetris:get-current-colored-shape))
           when (not (eql s '-))
           do (draw-box (+ (curr-column tetris:*game-state*) column)
                        (+ (curr-row tetris:*game-state*) row)
                        0
                        (get-color-v-for-block s))))

  ;; draw 2 next shapes
  (let ((offset 1))
    (loop
       for num below 2
       for shape in (mapcar
                     (lambda (piece)
                       (tetris:get-colored-shape piece))
                     (tetris:get-next-pieces :limit 2))
       ;; TODO maybe it might now work?
       do (progn (loop for row below (length shape)
                    do (loop for column below (length (car shape))
                          for s = (tetris:symbol-at column
                                                    row
                                                    shape)
                          when (not (eql s '-))
                          do (draw-box (+ tetris:+width+ column 1)
                                       (+ row 1 offset )
                                       0
                                       (get-color-v-for-block s))))
                 (setf offset (+ 1 offset (length shape))))))

  ;; draw ghost shape
  (when (curr-piece tetris:*game-state*)
    (let* ((ghost-piece (tetris:get-current-ghost-piece))
           (ghost-shape (piece-shape ghost-piece))
           (ghost-col (piece-column ghost-piece))
           (ghost-row (piece-row ghost-piece)))
      ;;::TODO will it break?
      (loop for row below (length (tetris:get-current-shape))
         do (loop for column below (length (car (tetris:get-current-shape)))
               for s = (tetris:symbol-at column
                                         row
                                         ghost-shape)
               when (not (eql s '-))
               do (draw-box (+ ghost-col column)
                            (+ ghost-row row)
                            0
                            (v:* (get-color-v-for-block (tetris:get-current-color))
                                 0.1))))))
  ;; draw map
  (loop for row below tetris:+height+
     do (loop for column below tetris:+width+
           for s = (tetris:symbol-at column
                                     row
                                     (game-map tetris:*game-state*))
                                     ;; #TODO just create get-current-map etc.
           if (eq s '-)
           do      '(draw-box column row -1 (get-color-v-for-block s))
           else do (progn
                     '(draw-box column row -2 (get-color-v-for-block '-))
                     (draw-box column row 0 (get-color-v-for-block s)))))
  (swap))


(defun draw-wall (width height color time)

  (map-g #'wall-pipeline (get-quad-stream-v2)
         :now (now)
         :width (+ width 2)
         :height (+ height 2)
         :perspective (rtg-math.projection:perspective
                       (x (resolution (current-viewport)))
                       (y (resolution (current-viewport)))
                       0.1
                       30f0
                       60f0)
         :cords (my-tr -1 -1.5 -1)
         :multipler (calculate-multipler time)
         :color color
         ))

(defun calculate-multipler (time)
  (/ (- 2 (if (> time 2)
              2f0
              time))
     2))

(defun draw-box (column row depth color)
  (map-g #'some-pipeline *buf-stream*
         :now (now)
         :width 1
         :height 1
         :perspective (rtg-math.projection:perspective
                       (x (resolution (current-viewport)))
                       (y (resolution (current-viewport)))
                       0.1
                       30f0
                       60f0)
         :cords (my-tr column row depth)
         :block-color color
         ))

(defun set-background-animation-timer (color)
  (format t "Setting new animation: ~a~%"color)
  (format t "Playing sound~%")
  (sounds:play-hit-sound)
  (setf (animation-timer *render-state*) 0f0
        (animation-color *render-state*) (get-color-v-for-block color))
  )


(defun init ()
  (tetris:create-player-and-reinit)
  (setf (piece-touched tetris:*callbacks*) #'set-background-animation-timer)
  (sounds:init-sound-system)
  (sounds:play-background-music)

  ;; init input

  (on-key-down key.space 'drop-down)
  (on-key-down key.r 'rotate)
  (on-key-down key.m 'toggle-on-off)
  ;;
  (unless *buf-stream*
    (destructuring-bind (vert index)
        (nineveh.mesh.data.primitives:box-gpu-arrays)
      (setf *buf-stream*
            (make-buffer-stream vert :index-array index)))))


(def-simple-main-loop play (:on-start #'init)
  
  (draw))



(defun main ()
  (tetris:create-computer)
  (loop while (not (game-over tetris:*game-state*))
     do (draw)))




