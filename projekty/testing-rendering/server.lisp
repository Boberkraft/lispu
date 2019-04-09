

(ql:quickload :usocket)
(ql:quickload :bt-semaphore)
(ql:quickload :alexandria)

(defpackage serve-tetris
  (:use)
  (:export))



(defparameter *commands*
  (map 'list #'string '(left
                        right
                        down
                        rotate
                        drop-down)) "Contains all of the commands that the client can send. As Strings")



(defun process-command (command)
  "Calls resposible tetris funcion"
  (alexandria:switch (command :test #'equal)
    ("left" (tetris:left))
    ("right" (tetris:right))
    ("down" (tetris:down))
    ("rotate" (tetris:rotate))
    ("drop-down" (tetris:drop-down))))

(defun accept-tetris-command (id command)
  (format t "~%Message connection from ~a" id)
  (cond ((not (find command *commands*))
         ;; Is this a good command?
         (format t "~%Unrecognized command ~a" command))
        (t
         ;; Inits player
         (format t "~%Executing command ~a" command)
           (with-player (init-player id) ;; Changes all of the global variables.
             (process-command command)))))











