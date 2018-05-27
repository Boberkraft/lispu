(defun our-lisp (x)
  (or (null x)
      (consp x)))

(defun our-atom (x)
  (not (consp x)))

(setf x (cons 'a nil))
(eql x x)

(equal x (cons 'a nil))

(defun our-equal (x y)
  (or (eql x y)
      (and (consp x)
           (consp y)
           (our-equal (car x) (car y))
           (our-equal (cdr x) (cdr y)))))

(our-equal '(1 2 3) '(1 2 4 3))
(our-equal 2 2)

(setf x '(a b c)
      y (copy-list x))

(defun our-copy-list (lst)
  (if (atom lst)
      lst
      (cons (car lst) (our-copy-list (cdr lst)))))

(our-copy-list '(a b c d))

(append '(a b) '(c d) '(e))

(defun compress (x)
  (if (consp x)
      (compr (car x) 1 (cdr x))
      x))

(defun compr (elt n lst)
  (if (null lst)
      (list (n-elts elt n))
      (let ((next (car lst)))
        (if (eql next elt)
            (compr elt (+ n 1) (cdr lst))
            (cons (n-elts elt n)
                  (compr next 1 (cdr lst)))))))

(defun n-elts (elt n)
  (if (> n 1)
      (list n elt)
      elt))

(compress '(1 1 1 0 1 0 0 0 0 1))
(n-elts "Siema" 3)
(list (n-elts "Siema" 3))

;;;;;;;;;;;

(defun uncompress (lst)
  (if (null lst)
      nil
      (let ((elt (car lst))
            (rest (uncompress (cdr lst))))
        (if (consp elt)
            (append (apply #'list-of elt)
                    rest)
            (cons elt rest)))))


(defun list-of (n elt)
  (if (zerop n)
      nil
      (cons elt (list-of (- n 1) elt))))

(uncompress '((3 1) 0 1 (4 0) 1))
(list-of 3 'ho)
(apply #list-of 2)

(nth 0 '(a b c))
(nthcdr 2 '(a b c))


(defun our-nthcdr (n lst)
  (if (zerop n)
      lst
      (our-nthcdr (- n 1) (cdr lst))))

(our-nthcdr 3 '(a b c 3))

(last '(a b c)) ; -> (c)


(mapcar #'(lambda (x) (+ x 100))
        '(1 2 3))

(mapcar #'list
        '(a b c)
        '(1 2 3 4))

(maplist #'(lambda (x) x)
         '(a b c))

(defun our-copy-tree (tr)
  (if (atom tr)
      tr
      (cons (our-copy-tree (car tr))
            (our-copy-tree (cdr tr)))))

(our-copy-tree '(a b c))

(subst 'y 'x '(and (integer x) (zerop (mod x 2))))

(defun our-subst (new old tree)
  (if (eql tree old)
      new
      (if (atom tree)
          tree
          (cons (our-subst new old (car tree))
                (our-subst new old (cdr tree))))))

(our-subst 'y 'x '(and (integer x) (zerop (mod x 2))))


(defun len (lst)
  (if (null lst)
      0
      (+ (len (cdr lst)) 1)))

(len '(a b c d))

(member 'b '(a b c))

(member '(a) '((a) (z)) :test #'equal)

(member 'c '((a b) (c d)) :key #'car)

(member-if #'oddp '(2 3 4))

(defun our-member-if (fn lst)
  (and (consp lst)
       (if (funcall fn (car lst))
           lst
           (our-member-if fn (cdr lst)))))

(our-member-if #'oddp '(2 3 4))

(adjoin 'b '(a b c))
(adjoin 'z '(a b c))
(union '(a b c) '(c b s))
(intersection '(a b c) '(b b c))
(set-difference '(a b c d e) '(b e))

(length '(a b c))

(subseq '(a b c d) 1 2) ; third is optional
(reverse '(a b c))

(defun mirror? (s)
  (let ((len (length s)))
    (and (evenp len)
         (let ((mid (/ len 2)))
           (equal (subseq s 0 mid)
                  (reverse (subseq s mid)))))))
(mirror? '(a b b a))
(mirror? '(a b b b a))

(sort '(0 2 1 3 8) #'>)

(defun nthmost (n lst)
  (nth (- n 1)
       (sort (copy-list lst) #'>)))

(nthmost 2 '(0 2 1 3 8))

(every #'oddp '(1 3 5))
(some #'evenp '(1 2 3))

(every #'> '(1 3 5) '(0 2 4))

(setf x '(b))
(push 'a x)

(list x)

(setf y x)
(pop x)

(list x)
(list y)


(defun our-reverse (lst)
  (let ((acc nil))
    (dolist (elt lst)
      (push elt acc))
    acc))

(our-reverse '(1 2 3 4 5 6 7))

(let ((x '(a b)))
  (pushnew 'c x)
  (pushnew 'a x)
  x)

(defun proper-list? (x)
  (or (null x)
      (and (consp x)
           (proper-list? (cdr x)))))

(proper-list? '(a b c d e))

(setf pair (cons 'a 'b))

'(a . (b . (c . nil)))

(cons 'a (cons 'b (cons 'c 'd)))
(list 'a . ('b . nil))

(setf trans '((+ . "add") (- . "substract")))
(assoc '+ trans)
(assoc '* trans)

(defun our-assoc (key alist)
  (and (consp alist)
       (let ((pair (car alist)))
         (if (eql key (car pair))
             pair
             (our-assoc key (cdr alist))))))

(our-assoc '+ trans)

(setf min '((a b c) (b c) (c d)))

(defun shortest-path (start end net)
  (bfs end (list (list start)) net))

(defun bfs (end queue net)
  (if (null queue)
      nil
      (let ((path (car queue)))
        (let ((node (car path)))
          (if (eql node end)
              (reverse path)
              (bfs end
                   (append (cdr queue) 
                           (new-paths path node net))
                   net))))))

; IF DOPLHINS ARE SO SMART,
; THEN WHY THEY LIVE IN IGLOS?

(defun new-paths (path node net)
  (mapcar #'(lambda (n)
              (cons n path))
          (cdr (assoc node net))))



(cdr (assoc 'a min))
(shortest-path 'a 'd min)

(new-paths '(b c) 'a min)

('EXERCISES)
;; 1
;; a> b>
;;     c> d> nil

;; a>
;;  b>
;;   c>
;;    d> nil

;;    > d> nill
;;    > c> nil  
;;   a> b> nil

;; a>   >d
;;     bc
;; a very nice format that only i can understand

;; 2


(defun new-union (a b)
  (append a (let ((new nil))
              (dolist (el b)
                (print el)
                (if (not (member el a))
                    (setf new (push el new))))
              new)))


(member-if #'oddp '(1 2 3 4))
(mapcar #'(lambda (x)
            (member-if #'oddp (list x)))
        '(1 2 3 4 5))
(new-union '(a b c) '(x a z))

;; 3
(defun occurrences (lst)
  (sort (occur lst '()) #'>
        :key #'cdr))

(defun occur (lst times)
  (print (car lst))
  (if (null lst)
      times
      (let ((item (car lst))
            (pair (assoc (car lst) times)))
        (if pair
            (progn
              (setf (cdr pair)
                    (+ 1 (cdr pair)))
              (occur (cdr lst) times))
            (occur (cdr lst) (push (cons item 1) times))))))

(occurrences '(a b a d a c d c a)) ;heh!

;; 4 because
(eql '(a) '(a))

;;5
(defun pos-rec+ (lst)
  (pos lst 0))

(defun pos-rec (lst n)
  (if lst
      (cons (+ n (car lst))
            (pos (cdr lst) (+ 1 n)))))

(defun pos-iter+ (lst)
  (let ((done nil)
        (len (length lst)))
    (do ((i 0 (+ i 1)))
        ((>= i len) done)
      (push (+ i (car lst))
            done)
      (setf lst
            (cdr lst)))))

(defun pos-map+ (lst)
  (let ((n 0))
    (mapcar #'(lambda (x)
                (setf x (+ n x))
                (setf n (+ n 1))
                x)
            lst)))

(pos-rec+ '(7 5 1 4))
(pos-iter+ '(7 5 1 4))
(pos-map+ '(7 5 1 4))


;; 7

(defun compress (x)
  (if (consp x)
      (compr (car x) 1 (cdr x))
      x))

(defun compr (elt n lst)
  (if (null lst)
      (list (n-elts elt n))
      (let ((next (car lst)))
        (if (eql next elt)
            (compr elt (+ n 1) (cdr lst))
            (cons (n-elts elt n)
                  (compr next 1 (cdr lst)))))))

(defun n-elts (elt n)
  (if (> n 1)
      (cons n elt) ; <-- just this?
      elt))

(compress '(1 1 1 0 1 0 0 0 0 1))

;; 8
(defun showdots (lst)
  (if lst
      (progn
        (format t "[ ~a . " (car lst))
        (showdots (cdr lst))
        (format t "]"))
      (format t "~a" nil)))

(showdots '(a b c)) ;; but its only shallow print

;; 9 for later
