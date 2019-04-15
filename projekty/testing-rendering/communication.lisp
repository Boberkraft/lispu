(defpackage #:communication
  (:use #:cl)

  (:export :start-server
           :stop-server

           :start-client
           :stop-client
           :send-data-to-server
           ))

(in-package :communication)


(defstruct client
  lock)
;;client
(defparameter *client-running* nil "True if the connection exists")
(defparameter *data-to-send* nil "List of data to send.")
(defparameter *data-lock* (bt:make-lock) "Used for removing and adding data.")

;;server
(defparameter *server-running* nil)

(defparameter *connection-id* 0) ;; unused for now.
;; TODO:
;; Maybe i should be giving an unique id to each connection,
;; so they can authenticate themself later?

;; Right now im authenticating connection based on their ip and port.
;; If its the same, then the player is the same ;


(defun make-id (&rest lst)
  ;;TODO: Maybe it should be replecable/changeble/schadowable?. Maybe it is?
  "Converts given list to string. Used as id"
  (format nil "~{~a~}" lst))


(defun start-simple-server (port callback)
  "Listen on port for messange, and call callback with recived input"
  (usocket:with-socket-listener (socket "127.0.0.1" port)
    (usocket:wait-for-input socket)
    (format t "~% - [Server]: connection accepted -")
    (usocket:with-connected-socket (connection (usocket:socket-accept socket))
      (let ((addr (usocket:get-peer-address connection))
            (port (usocket:get-peer-port connection)))
        ;; Strip/trim all of the Spaces and Newlines from end and beginning.
        (funcall callback (make-id addr port)
                 (string-trim '(#\Space #\Newline)
                              (read-line (usocket:socket-stream connection))))))))

(defun start-server (function)
  (setf *server-running* t)
  (format t "~% - [Server]: STARTING -")
  (bt:make-thread
   (lambda ()
     (unwind-protect
          (progn (loop while *server-running*
                    do (progn
                         (format t "~% - [Server]: waiting for connection -")
                         (start-simple-server 5515 function)
                         (format t "~% - [Server]: connection closed  -"))))
       (setf *server-running* nil))
     (format t "~% - [Server] STOPING - "))))

(defun stop-server ()
  (setf *server-running* nil))

;;; ----------- client


(defun start-client ()

  (if (not *client-running*)
      (progn
        (bt:make-thread (lambda ()
                          (start-simple-client 5515)))
        (setf *client-running* t))
      (format t "~% - [Client]: ALREADY RUNNING -")))

(defun stop-client ()
  (setf *client-running* nil))


(defun send-data-to-server (data)
  (bt:with-lock-held (*data-lock*)
    (push data *data-to-send*)))

(defun is-there-data-to-send-p ()
  *data-to-send*)

(defun pop-data-to-be-sended-to-server ()
  (bt:with-lock-held (*data-lock*)
    (prog1
        (first *data-to-send*)
      (setf *data-to-send* (rest *data-to-send*)))))

(defun start-simple-client (port)
  "Connect to a server and send a messange."
  (setf *client-running* t)
  (format t "~% - [Client]: STARTING - ")
  (unwind-protect
       (usocket:with-client-socket (socket stream "127.0.0.1" port)
         (format t "~% - [Client]: connected - ")
         (loop while *client-running*
            do (progn
                 (if (is-there-data-to-send-p)
                     (let ((data (pop-data-to-be-sended-to-server)))
                       (format t "~% - [Client]: sending ~w - " data)
                       (format stream data)
                       (force-output stream))
                     (format t "~% - [Client]: no data to send - "))
                 (sleep 3) ;; TODO see without it.
                 )))
    ;; something broke (prob. socket in use or something)
    (progn (format t "~% - [Client]: STOPPING -")
           (setf *client-running* nil))))

