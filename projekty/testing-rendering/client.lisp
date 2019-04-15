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

(defun start ()
  (link:start-client))

(defun stop ()
  (link:stop-client))

(defun send-data (message)
  (link:send-data-to-server message))

(defun start-message ()
  (start)
  (send-data "left~%")
  (send-data "left~%")
  (send-data "left~%")
  (stop))


#+nil (start-message)
