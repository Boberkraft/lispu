
(defpackage #:serve
  (:use #:cl)
  (:export :start
           :stop))

(in-package :serve)

(defparameter *commands*
  (map 'list #'string '(left
                        right
                        down
                        rotate
                        drop-down)) "Contains all of the commands that the client can send. As Strings")

(defun message-as-client (client message)
  "Makes a string with client-id at the Begining"
  (concatenate 'string
               (format nil "~a ~a" (link:client-id client) message)
               "~%"))

(defun accept-tetris-command (client command)
  (let ((id (link:client-id client)))
    (format t "~%Message from ~a" id)
    (cond ((not (find command *commands* :test #'equalp))
           ;; Is this a good command?
           (format t "~%Unrecognized command ~w" command))
          (t
           ;; Inits player
           (format t "~%Executing command ~w" command)
           (tetris:with-player (player-functions:init-player id) ;; Changes all of the global variables.
             (process-command-sended-to-server command)
             (inform-other-players client command))))))

(defun inform-other-players (who command)
  (link:send-data-to-all-clients
   (message-as-client who command)))

(defun process-command-sended-to-server (command)
  "Calls resposible tetris function"
  (alexandria:switch (command :test #'equalp)
    ("left" (tetris:left))
    ("right" (tetris:right))
    ("down" (tetris:down))
    ("rotate" (tetris:rotate))
    ("drop-down" (tetris:drop-down))))


(defun start ()
  (link:start-server 'accept-tetris-command))

(defun stop ()
  (link:stop-server))


#+ nil (start)
#+ nil (stop)
