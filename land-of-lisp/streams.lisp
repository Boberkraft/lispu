(output-stream-p *standard-output*)

(write-char #\x *standard-output*)

(input-stream-p *standard-input*)

(read-char *standard-input*)

(with-open-file (my-stream "d:/data.txt"
                           :direction :output)
  (print "my data" my-stream))

(with-open-file (my-stream "D:/data.txt"
                           :direction :input)
  (read my-stream))

(let ((animal-noises '((dog . woof)
                       (cat . meow))))
  (with-open-file (my-stream "D:/animal-noises.txt"
                             :direction :output)
    (print animal-noises my-stream)))

(with-open-file (my-stream "D:/animal-noises.txt"
                           :direction :input)
  (read my-stream))

(with-open-file (my-stream "D:/data.txt"
                           :direction :output
                           :if-exists :supersede))

(defparameter my-socket (socket-server 4321))
(defparameter my-stream (socket-accept my-socket))

(read my-stream)

(print "What up, Client!" my-stream)

(close my-stream)
(socket-server-close my-socket)

(defparameter foo (make-string-output-stream))
(princ "this will go into foo. " foo)
(princ "This will also go into foo. " foo)
(get-output-stream-string foo)

;;client
(socket-connect 54321 "127.0.0.1")

(with-output-to-string (*standard-output*)
  (princ "The sum of ")
  (princ 5)
  (princ " and ")
  (princ 2)
  (princ " is ")
  (princ (+ 2 5)))

GET /dupa HTTP/1.1
Host: localhost:8080
User-Agent: Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9.0.5)
Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
Accept-Language: en-us,en;q=0.5
Accept-Encoding: gzip,deflate
Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7
Keep-Alive: 300
Connection: keep-alive
