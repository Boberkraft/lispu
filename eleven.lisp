(format t "Add onion rings for only ~$ dollars more!" 1.5)

(princ (reverse
        (format nil "Add onion rings for only ~$ dollars more!" 1.5)))

(format t "Im a printing ~s in the middle of this sentence." "Foo")
(format t "Im a printing ~a in the middle of this sentence." "Foo")

(format t "I am printin ~10a withing ten space of room." "foo")
(format t "I am printin ~10@a withing ten space of room." "foo")
(format t "I am printin ~10,3a withing ten space of room." "foo")

(format t "I am printin ~,,2a space of room." "foo")

(format t "The world ~,,4,'!@a feels very importent." "foo")

(format t "The number 1000 in haxadecimal is ~x" 1000)
(format t "the number 1000 in binary is ~b" 1000)

(format t "The number 1000 is decimal is ~d" 1000)

(format t "Numbers with comas in them are ~:d times better." )

(format t "Im pringing ~10d witdth ten space of room" 10000)

(format t "I am printing ~10,'xd withing ten spaces of room" 1000000)

(format t "Pi can be estimated as ~4f" 3.141593)

(format t "PI can be estimated as ~,4f" pi)

(format t "Percentages are ~,,2f percent better than fractions" 0.77)

(format t "I wish i hade ~$ dollars in my ban account." 1000.2)

(progn (princ 22)
       (terpri)
       (princ 33))

(progn (format t "This is on one line ~%")
       (format t "~%this is on another line"))
(progn (format t "This is on one line ~&")
       (format t "~&this is on another line"))

(format t "This will print ~5%on lines spread far apart")

(defun random-animal ()
  (nth (random 5) '("dog" "tick" "tiger" "walrus" "kangaroo")))

(loop repeat 10
   do (format t "~5t~a ~15t~a ~25t~a~%"
              (random-animal)
              (random-animal)
              (random-animal)))

(loop repeat 10
   do (format t "~10a ~10a ~10a~%"
              (random-animal)
              (random-animal)
              (random-animal)))


(loop repeat 10
   do (format t "~30<~a~;~a~;~a~>~%"
              (random-animal)
              (random-animal)
              (random-animal)))

(loop repeat 10 do (format t "~30:@<~a~>~%" (random-animal)))

(loop repeat 10
   do (format t "~30:@<~a~;~a~;~a~>~%"
              (random-animal)
              (random-animal)
              (random-animal)))

(loop repeat 10
   do (format t "~10:@<~a~>~10:@<~a~>~10:@<~a~>~%"
              (random-animal)
              (random-animal)
              (random-animal)))

(defparameter *animals* (loop repeat 10 collect (random-animal)))

(format t "~{I see a ~a! ~}"*animals*)

(format t "~{I see a ~a... or was it a ~a?~%~}"*animals*)


(format t "~{~<|~%|~,33:;~2d ~>~}" (loop for x below 100 collect x))

(format t "~{~< |~%| ~,30:; ~2d ~>~}" (loop for x below 100 collect x))
