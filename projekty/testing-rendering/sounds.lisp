

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

(in-package #:sounds)



(defvar *background-music*
  (harmony-simple:decode #p"banks.mp3"))

(defvar *hit-sound*
  (harmony-simple:decode #p"kick.mp3"))

(defun pausedp ()
  (harmony-simple:paused-p))

(defun init-sound-system ()
  (harmony-simple:initialize :output-spec '(harmony-out123:out123-drain)))


(defun play-background-music ()
  (harmony-simple:play 'harmony:buffer-source :music :loop t :buffers *background-music*)
  (setf *was-background-playing* t))

(defun stop ()
  (harmony-simple:stop))

(defun resume ()
  (harmony-simple:resume))

(defun toggle-on-off ()
  "Toggles between ON and OFF.
   Resumes when stopped, stopps when playinh"
  (if (pausedp)
      (resume)
      (stop)))

(defun play-hit-sound ()
  "The sound of block hitting"
  (harmony-simple:play 'harmony:buffer-source :sfx :buffers *hit-sound*))
