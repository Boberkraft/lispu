(setf arr (make-array '(2 3) :initial-element nil))
arr

(aref arr 0 0)
(setf (aref arr 0 0) 'b)
arr
(aref arr 0 0)

(setf vec (make-array 4 :initial-element nil))

(vector "b" 'b 3)

(svref vec 0)

;;; specialized data structures

(defun bin-search (obj vec)
  (let ((len (length vec)))
    (and (not (zerop len))
         (finder obj vec 0 (- len 1)))))

(defun finder (obj vec start end)
  (let ((range (- end start)))
    (if (zerop range)
        (if (eql obj (aref vec start))
            obj
            nil)
        (let ((mid (+ start (round (/ range 2)))))
          (let ((obj2 (aref vec mid)))
            (if (< obj obj2)
                (finder obj vec start (- mid 1))
                (if (> obj obj2)
                    (finder obj vec (+ mid 1) end)
                    obj)))))))

(bin-search 3 #(0 1 2 3 4 5 6 7 8 9))


(sort "elbow" #'char<)
(aref "abc" 1)
(char "abc" 1)

(let ((str (copy-seq "Merlin")))
  (setf (char str 3) #\k)
  str)

(equal "fred" "fred")
(equal "fred" "Fred")
(string-equal "fred" "FRED")

(format nil "~A or ~A" "truth" "dare")
(concatenate 'string "not " "to worry")

(defun mirror? (s)
  (let ((len (length s)))
    (and (evenp len)
         (do ((forward 0 (+ forward 1))
              (back (- len 1) (- back 1)))
             ((or (> forward back)
                  (not (eql (elt s forward)
                            (elt s back))))
              (> forward back))))))

(mirror? (vector 1 2 2 1))

(position #\a "fantasia")
(position #\a "fantasia" :start 3 :end 5)

(position #\a "fantasia" :from-end t)

(position 'a '((c d) (a b)) :key #'car)

(position '(a b) '((a b) (c d)))
(position '(a b) '((a b) (c d)) :test #'equal)

(position 3 '(1 0 7 5) :test #'<)

(defun second-word (str)
  (let ((p1 (+ (position #\Space str) 1)))
    (subseq str p1 (position #\Space str :start p1))))

(second-word "Form follows function.")

(position-if #'oddp '(2 3 4 5))


(find #\a "cat")
(find-if #' characterp "ham")

(find-if #'(lambda (x)
             (eql (car x) 'complete))
         '((complete x)))

(find 'complete '((complete x)) :key #'car)

(remove 'a '(a b c d e))
(remove-duplicates "abracadabra")

(reduce #'intersection '((b r a d 's) (b a d) (c a t)))

(defun tokens (str test start)
  (let ((p1 (position-if test str :start start)))
    (if p1
        (let ((p2 (position-if #'(lambda (c)
                                   (not (funcall test c)))
                               str :start p1)))
          (cons (subseq str p1 p2)
                (if p2
                    (tokens str test p2)
                    nil)))
        nil)))

(defun constituent (c)
  (and (graphic-char-p c)
       (not (char= c #\Space))))

(tokens "ab12 3cde.f" #'alpha-char-p 0)
(tokens "ab12 3cde.f
                    gh" #'constituent 0)


(defun parse-data (str)
  (let ((toks (tokens str #'constituent 0)))
    (list (parse-integer (first toks))
          (parse-month   (second toks))
          (parse-integer (third toks)))))

(defconstant month-names
  #("jan" "feb" "mar" "apr" "may" "jun"
    "jul" "aug" "sep" "oct" "nov" "dec"))

(defun parse-month (str)
  (let ((p (position str month-names
                     :test #'string-equal)))
    (if p
        (+ p 1)
        nil)))

(parse-data "16 Aug 1980")

(defun my-read-integer (str)
  (if (every #'digit-char-p str)
      (let ((accum 0))
        (dotimes (pos (length str))
          (setf accum (+ (* accum 10)
                         (digit-char-p (char str pos)))))
        accum)
      nil))

(my-read-integer "203")

(defstruct point
  x
  y)

(setf p (make-point :x 0 :y 0))
(point-x p)
(setf (point-y p) 2)
p

(point-p p)
(typep p 'point)

(defstruct polemic
  (type (progn
          (format t "What kind of polemic was it?: ")
          (read)))
  (effect nil)
  (dupa nil))
(setf *xd* (make-polemic))

(defstruct (point (:conc-name p)
                  (:print-function print-point))
  (x 0)
  (y 0))

(defun print-point (p streab depth)
  (format stream "#<~a, ~a>" (px p) (py p)))

;;; binarry treee search
(defstruct (node (:print-function
                  (lambda (n s d)
                    (format s "#<~a>" (node-elt n)))))
  elt (l nil) (r nil))

(defun bst-insert (obj bst <)
  (if (null bst)
      (make-node :elt obj)
      (let ((elt (node-elt bst)))
        (if (eql obj elt)
            bst
            (if (funcall < obj elt)
                (make-node
                 :elt elt
                 :l  (bst-insert obj (node-l bst) <)
                 :r  (node-r bst))
                (make-node
                 :elt elt
                 :r  (bst-insert obj (node-r bst) <)
                 :l  (node-l bst)))))))

(defun bst-find (obj bst <)
  (if (null bst)
      nil
      (let ((elt (node-elt bst)))
        (if (eql obj elt)
            bst
            (if (funcall < obj elt)
                (bst-find obj (node-l bst) <)
                (bst-find obj (node-r bst) <))))))

(defun bst-min (bst)
  (and bst
       (or (bst-min (node-l bst))
           bst)))

(defun bst-max (bst)
  (and bst
       (or (bst-max (node-r bst))
           bst)))

(setf nums nil)

(dolist (x '(5 8 4 2 1 9 6 7 3))
  (setf nums (bst-insert x nums #'<)))

(bst-find 12 nums #'<)

(bst-find 4 nums #'<)

(bst-min nums)
(bst-max nums)

(defun bst-remove (obj bst <)
  (if (null bst)
      nil
      (let ((elt (node-elt bst)))
        (if (eql obj elt)
            (percolate bst)
            (if (funcall < obj elt)
                (make-node
                 :elt elt
                 :l (bst-remove obj (node-l bst) <)
                 :r (node-r bst))
                (make-node
                 :elt elt
                 :r (bst-remove obj (node-r bst) <)
                 :l (node-l bst)))))))

(defun percolate (bst)
  (cond ((null (node-l bst))
         (if (null (node-r bst))
             nil
             (rperc bst)))
        ((null (node-r bst)) (lperc bst))
        (t (if (zerop (random 2))
               (lperc bst)
               (rperc bst)))))

(defun rperc (bst)
  (make-node :elt (node-elt (node-r bst))
             :l (node-l bst)
             :r (percolate (node-r bst))))

(defun lperc (bst)
  (make-node :elt (node-elt (node-l bst))
             :l (percolate (node-l bst))
             :r (node-r bst)))


(defun bst-traverse (fn bst)
  (when bst
    (bst-traverse fn (node-l bst))
    (funcall fn (node-elt bst))
    (bst-traverse fn (node-r bst))))

(bst-traverse #'princ (bst-remove 2 nums #'<))


;;4.8

(setf ht (make-hash-table))
(gethash 'color ht)

(setf (gethash 'color ht) 'red)
(gethash 'color ht)

(setf bugs (make-hash-table))
(push "doesn't take keyword arguemnts."
      (gethash "benis" bugs))

(setf fruits (make-hash-table))
(setf (gethash 'apricot fruits) t)

(remhash 'color ht)

(setf (gethash 'shape ht) 'spherical
      (gethash 'size ht) 'giant)

(maphash #'(lambda (k v)
             (format t "~a = ~a~%" k v))
         ht)

(make-hash-table :size 5)

(setf writers (make-hash-table :test #'equal))
(setf (gethash `(ralph waldo emerson) writers) t)

;;1
(defun quarter-turn (arrs)
  (let* ((dim (car (array-dimensions arrs)))
         (new-arrs (make-array (list dim dim))))
    (dotimes (i dim)
      (dotimes (j dim)
        (setf (aref new-arrs i j)
              (aref arrs (- dim j 1) i))))
    new-arrs))


(quarter-turn-other #2A((a b c)
                        (d e f)
                        (g h i)))

(print (quarter-turn #2A((a b c)
                         (d e f)
                         (g h i))))


(let* ((x 5)
      (y (+ x 2)))
  y)


