

(defun flam (data prev n)
  (if (not (equal data ""))
      (let ((el (char data 0)))
        (if (eql el prev)
            (flam (string-rest data) prev (+ n 1))
            (progn
              (show-flamaster prev n)
              (flam (string-rest data) el 1))))
      (show-flamaster prev n)))

(defun string-rest (data)
  (subseq data 1))

(defun show-flamaster (el times)
  (if (equal times 1)
      (format t "~a" el)
      (if (equal times 2)
          (format t "~a~a" el el)
          (format t "~a~a" el times))))

(defun flamaster (data)
  (flam (string-rest data) (char data 0) 1)
  (format t "~%"))

(defun flamaster-loop ()
  (dotimes (i (read))
    (flamaster (read-line))))

(flamaster-loop)
;(show-flamaster "A" 5)
;(show-flamaster "A" 2)
;(show-flamaster "A" 3)
;(string-rest "abdc")
;(flamaster "aaaaabbbbeeezxc")
(exit)
