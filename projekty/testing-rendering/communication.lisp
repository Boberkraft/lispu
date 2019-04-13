(defpackage #:communication
  (:use #:cl)

  (:export :start-simple-server
           :start-simple-client))

(defparameter *connection-id* 0) ;; unused for now.
;; TODO:
;; Maybe i should be giving an unique id to each connection,
;; so they can authenticate themself later?

;; Right now im authenticating connection based on their ip and port.
;; If its the same, then the player is the same ;


(defun make-id (&rest lst)
  ;;TODO: it should be replecable/changeble. Maybe it is?
  "Converts given list to string. Used as id"
  (format nil "~{~a~}" lst))


(defun start-simple-server (port callback)
  "Listen on port for messange, and call callback with recived input"
  (usocket:with-socket-listener (socket "127.0.0.1" port)
    (usocket:wait-for-input socket)
    (usocket:with-connected-socket (connection (usocket:socket-accept socket))
      (let ((addr (usocket:get-peer-address connection))
            (port (usocket:get-peer-port connection)))
        (funcall callback (make-id addr port) (string-trim '(#\Space #\Newline)
                                          (read-line (usocket:socket-stream connection))))))))


;; (bt:make-thread (lambda ()
;;                   (start-simple-server 5512 '(lambda (a b) (print (list a b))))
;;                    (print "ENDED")))


(defun start-simple-client (port)
  "Connect to a server and send a messange."
  (usocket:with-client-socket (socket stream "127.0.0.1" port)
    (format stream "Hello world!~%")
    (force-output stream)))
