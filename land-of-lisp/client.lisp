

(defparameter data "POST /login.html HTTP/1.1
Host: localhost:8080
User-Agent: Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9.0.5)
Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
Accept-Language: en-us,en;q=0.5
Accept-Encoding: gzip,deflate
Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7
Keep-Alive: 300
Connection: keep-alive
Content-Length: 39
userid=foo&password=supersecretpassword")

(with-open-stream (stream (socket-connect (progn (princ "Podaj port: ")
                                                 (read))
                                          "127.0.0.1"))
  (princ data stream)
  (force-output stream))



