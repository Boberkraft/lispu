
(defpackage serve-tetris
  (:use #:cl)
  (:export :*commands*
           :process-command
           :accept-tetris-command))



(defparameter *commands*
  (map 'list #'string '(left
                        right
                        down
                        rotate
                        drop-down)) "Contains all of the commands that the client can send. As Strings")



(defun process-command (command)
  "Calls resposible tetris function"
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
           (tetris:with-player (player-functions:reinit-player id) ;; Changes all of the global variables.
             (process-command command)))))











