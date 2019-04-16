;; This file is reposible for providing functionality
;; for connecting to a tetris server (defined in server.lisp).
;; It accepts commands send from server, and then executes then on
;; local game of tetris. It might be a console tetris (tetris.lisp)
;; or this colored graphical tetris (testing-rendering.lisp)

(defpackage #:client
  (:use #:cl
        )
  (:export :start
           :stop)
  )


(in-package :client)

(defparameter *server-stream* )

(defun start ()
  (link:start-client 'start-message))

(defun start-dummy-client ()
  (link:start-client (lambda ()
                       (loop (sleep 0.1)))))

(defun stop ()
  (link:stop-client))



(defun send-data-to-server (message)
  (format link:*server-stream* message)
  (force-output link:*server-stream*))

(defun read-data-from-server ()
  (read link:*server-stream*))

(defun start-message ()
  (send-data "left~%")
  (send-data "left~%")
  (send-data "left~%")
  (send-data "drop-down~%")
  )

(defun dummy-client ()
  (loop while link:*client-running*
       do (sleep 0.1)))

#+nil (start-dummy-client)
#+nil (start-message)
#+nil (stop)
