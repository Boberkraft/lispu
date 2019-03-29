(defpackage #:sounds
  (:use :cl
        :harmony-simple)
  (:export :init-sound-system
           :play-background-music
           :stop
           :play-hit-sound
           :resume
           :pausedp
           :toggle-on-off))
(in-package :sounds)

(defvar *background-music* nil)
(defvar *hit-sound* nil)

(defparameter *stopped* nil)
(defparameter *is-background-playing* nil)
(defparameter *background-source* nil)

;;TODO: MAKE IT IN BUFFERS AAAAAA

(defun pausedp ()
  *stopped*)

(defun init-sound-system ()
  (harmony-simple:initialize :output-spec '(harmony-out123:out123-drain))
  )

(defun play-background-music ()
  (when (and (not *is-background-playing*)
             (not *stopped*))
    (setf *background-source* (harmony-simple:play #p"Chipzel To The Sky.mp3" :music))
    (setf *is-background-playing* t)))

(defun stop ()
  (setf *stopped* t)
  (when *background-source*
    (harmony-simple:pause *background-source*)))

(defun resume ()
  (setf *stopped* nil)
  (when *background-source*
    (harmony-simple:resume *background-source*)))


(defun toggle-on-off ()
  "Toggles between ON and OFF.
   Resumes when stopped, stopps when playinh"
  (if (pausedp)
      (resume)
      (stop)))

(defun play-hit-sound ()
  "The sound of block hitting"
  (harmony-simple:play #p"kick.mp3" :sfx))
