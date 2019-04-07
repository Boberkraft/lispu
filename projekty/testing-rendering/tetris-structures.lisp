(defpackage #:tetris-structures
  (:use #:cl
        #:bt-semaphore)
  (:export  :piece ; piece
            :make-piece
            :copy-piece
            :piece-name
            :piece-color
            :piece-shape
            :piece-column
            :piece-row

            ;; game-state
            :game-state
            :game-map
            :curr-row
            :curr-column
            :curr-piece
            :game-over
            :events
            :difficulty
            :next-pieces
            :callbacks
            :misc

            ;; callbacks
            :piece-touched

            ;; misc
            :lock
            :hilighted-rows))

(in-package :tetris-structures)

(defstruct piece
  name
  color
  shape
  (column nil)
  (row nil))


(defclass game-state ()
  ((game-map
    :accessor game-map
    :initform nil
    :documentation "2d list of list of symols")
   (current-row
    :accessor curr-row
    :initform nil)
   (current-column
    :accessor curr-column
    :initform nil)
   (current-piece
    :accessor curr-piece
    :initform nil)
   (game-over
    :accessor game-over
    :initform nil)
   (events
    :accessor events
    :initform nil)
   (difficulty
    :accessor difficulty
    :initform 1)
   (next-pieces-queue
    :accessor next-pieces
    :initform nil)
   (all-of-the-available-callback
    :accessor callbacks
    :initform (make-instance 'callbacks)
    :documentation "Contains all callback that other can subsitue. See the class callbacks")
   (miscellaneous
    :accessor misc
    :initform (make-instance 'misc)
    :documentation "contains all variables like locks. See the class misc")))


(defclass callbacks ()
  ((piece-touched
    :accessor piece-touched
    :initform (lambda (piece-color)
                (declare (ignore piece-color))
                nil))))
(defclass misc ()
  ((lock
    :accessor lock
    :initform (bt:make-lock))
   (hilighted-rows
    :accessor hilighted-rows
    :initform nil)))
