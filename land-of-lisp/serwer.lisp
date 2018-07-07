

(defun bad-function ()
  (error 'foo))

;;(handler-case (bad-function)
;;  (foo () "somebody signaled foo!")
;;  (bar () "somebody singaled bar!"))

;;(unwind-protected (/ 1 0)
;;(princ "I need to say 'flubydubu' no matter what"))

(defun http-char (c1 c2 &optional (default #\Space))
  (let ((code (parse-integer
               (coerce (list c1 c2) 'string)
               :radix 16
               :junk-allowed t)))
    (if code
        (code-char code)
        default)))

(defun decode-param (s)
  (labels ((f (lst)
             (when lst
               (case (car lst)
                 (#\% (cons (http-char (cadr lst) (caddr lst))
                            (f (cdddr lst))))
                 (#\+ (cons #\space
                            (f (cdr lst))))
                 (otherwise (cons (car lst)
                                  (f (cdr lst))))))))
    (coerce (f (coerce s 'list)) 'string)))

(decode-param "foo")
(decode-param "foo%3f")
(decode-param "foo+bar")


(defun my-split (string &key (delimiterp  #'delimiterp ))
  (loop
     for beg = (position-if-not delimiterp string)
     then (position-if-not delimiterp string :start (1+ end))
     for end = (and beg
                    (position-if delimiterp  string :start beg))
     when beg collect (subseq string beg end)
     while end))

(defun delimiterp  (c) (char= c #\Space))

(my-split "   moja mama robi dobre jajka")


(defun parse-params (s)
  (let* ((i1 (position #\= s))
         (i2 (position #\& s)))
    (cond (i1 (cons (cons (intern (string-upcase (subseq s 0 i1)))
                          (decode-param (subseq s (1+ i1) i2)))
                    (and i2
                         (parse-params (subseq s (1+ i2))))))
          ((equal s "")
           nil)
          (t s))))


(defun parse-url (s)
  (let* ((url (subseq s
                      (+ 2 (position #\space s))
                      (position #\space s :from-end t)))
         (x (position #\? url)))
    (if x
        (cons (subseq url 0 x)
              (parse-params (subseq url (1+ x))))
        (cons url
              '()))))

(parse-url "GET /lolcats.html HTTP/1.1")

(parse-url "GET /lolcats.html?extra-funny=yes&duap=ye HTTP/1.1")

(defun get-header (stream)
  (let* ((s (read-line stream))
         (h (let ((i (position #\: s)))
              (when i
                (cons (intern (string-upcase (subseq s 0 i)))
                      (subseq s (+ i 2)))))))
    (when h
      (cons h
            (get-header stream)))))


(defun get-content-params (stream header)
  (let ((length (cdr (assoc 'content-length header))))
    (when length
      (let ((content (make-string (parse-integer length))))
        (read-sequence content stream) ;; the f
        (parse-params content)))))

;;(get-header (make-string-input-stream "Host: www.mywebsite.com
;;User-Agent: Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9.0.5)
;;Content-Length: 39"))

(defun serve (request-handler)
  (let ((socket (socket-server (progn (princ "Jaki ma byc port twojego serwera?: ")
                                      (let ((port (read)))
                                        (princ "Port is ")
                                        (princ port))))))
    (format t "Starting server" )
    (sleep 2)
    (format t "...")
    (unwind-protect
         (loop (with-open-stream (stream (socket-accept socket))
                 (format t "Somebody connected!~%")
                 (let* ((url    (parse-url (print (read-line stream))))
                        (path   (car url))
                        (header (print (get-header stream)))
                        (params (append (cdr url)
                                        (get-content-params stream header)))
                        (*standard-output* stream)
                        )
                   (format stream "HTTP/1.x 200 OK~%Content-Type: text/html; charset=UTF-8~%~%")
                   (funcall request-handler path header params)
                   (force-output stream))))
      (socket-server-close socket))))

(defun hello-request-handler (path header params)
  (if (equal path "greeting")
      (let ((name (assoc 'name params)))
        (if (not name)
            (princ "<html><form>What is your name?<input name='name' /></form></html>")
            (format t "<html>Nice to meet you, ~a!</html>" (cdr name))))
      (princ "Sorry... I don't know that page.")))

;;(hello-request-handler "greeting" nil '((name . "Bob")))

;;(serve #'hello-request-handler)









