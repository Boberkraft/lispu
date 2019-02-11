
(defvar +width+ 10 "width of the map")
(defvar +height+ 20 "height of the map")

(defparameter *map* (loop for rows below +height+
                       collect (loop for column below +width+
                                  collect '-)) "List of lists representing map")

(defparameter *shapes* nil "Alist containing all of the shapes.")

(defparameter *curr-shape* nil)
(defparameter *curr-column* nil)
(defparameter *curr-row* nil)
(defparameter *hilighted-rows* nil)
(defparameter *game-over* nil)
(defparameter *events* nil)
(ql:quickload :bt-semaphore)

(push '(o . ((x x)
             (x x))) *shapes*)

(push '(t . ((x x x)
             (- x -))) *shapes*)

(push '(j . ((- x)
             (- x)
             (x x))) *shapes*)

(push '(z . ((- x x)
             (x x -))) *shapes*)

(push '(z-inv . ((x x -)
                 (- x x))) *shapes*)

(push '(l . ((x -)
             (x -)
             (x x))) *shapes*)

(push '(i . ((x -)
             (x -)
             (x -)
             (x -))) *shapes*)

(defun get-random-shape ()
  (let ((num-of-shapes 0))
    (mapc (lambda (x) (declare (ignore x))
             (incf num-of-shapes))
                  *shapes*)
    (get-shape (random num-of-shapes))))

;; ref by number
(defmethod get-shape ((num number))
  (cdr (nth num *shapes*)))

;; ref by letter
(defmethod get-shape ((s symbol))
  (cdr (assoc s *shapes*)))


;; '((- x -)(x x x)(x x x)) => '((x x -)(x x x)(x x -))
;;
;;    - x -        x x -
;;    x x x   ->   x x x
;;    x x x        x x -
(defun rotate-h (shape)
  (mapcar 'reverse
          (apply #'mapcar #'list shape)))

(defun rotate-shape ()
  (let ((rotated (rotate-h *curr-shape*)))
    (unless (or (will-shape-touch-others rotated *curr-column* *curr-row*)
                (will-shape-touch-floor rotated *curr-column* *curr-row*)
                (will-shape-touch-walls rotated *curr-column* *curr-row*))
      (setf *curr-shape* rotated))))

(defmacro symbol-at (col row 2d-list &optional check)
  (if check
      `(if (or (< ,col 0)
               (< ,row 0))
           '-
           (nth ,col
                (nth ,row ,2d-list)))
      `(nth ,col
            (nth ,row ,2d-list))))

(defun will-shape-touch-others (shape start-col start-row)
  (loop for row below (length shape)
     do (loop for column below (length (car shape))
           for shape-symbol-at = (symbol-at column
                                            row
                                            shape)
           for map-symbol-at = (symbol-at (+ column start-col)
                                          (+ row start-row)
                                          *map* t)
           when (and (eql shape-symbol-at 'x)
                     (eql map-symbol-at 'x))
           do (return-from will-shape-touch-others t)))
  nil)


(defun will-shape-touch-floor (shape start-col start-row)
  (declare (ignore start-col))
  (loop for row from (1- (length shape)) downto 0
     when (find 'x (nth row shape)) ; block starts on this row
     do (return-from will-shape-touch-floor
          (<= +height+ (+ start-row row)))))

(defun will-shape-touch-walls (shape start-col start-row)
  (declare (ignore start-row))
  ;; find x to se where to shape starts
  (labels ((find-first-x (shape)
             (loop for column below (length (car shape))
                do (loop for row below (length shape)
                      for shape-symbol = (symbol-at column
                                                    row
                                                    shape)
                      if (eq shape-symbol 'x)
                      do (return-from find-first-x column)))))
    (let ((x-start-left (find-first-x shape))
          (width  (- (length (car shape))
                     (find-first-x (mapcar 'reverse shape)))))
      (if (or (> 0 (+ start-col x-start-left)) ;left site
              (<  +width+ (+ start-col width)) ;right site
              )
          t
          nil)))) ;; yes i know



(defun add-shape-to-map (shape start-col start-row)
  (loop for row below (length shape)
     do (loop for column below (length (car shape))
           for shape-symbol = (symbol-at column
                                         row
                                         shape)
           when (and (eq shape-symbol 'x)
                     ;; checks whatever symbol is stil inbouds of map
                     (not (null (symbol-at (+ start-col column)
                                           (+ start-row row)
                                           *map* t))))
           do (setf (symbol-at (+ start-col column)
                               (+ start-row row)
                               *map*)
                    'x))))

(defun remove-shape-from-map (shape start-col start-row)
  (loop for row below (length shape)
     do (loop for column below (length (car shape))
           for shape-symbol = (symbol-at column
                                         row
                                         shape)
           when (eq shape-symbol 'x)
           do (setf (symbol-at (+ start-col column)
                               (+ start-row row)
                               *map*)
                    '-))))


(defun generate-new-piece ()
  (setf *curr-shape* (get-random-shape)
        *curr-column* (floor (/ +width+ 2))
        *curr-row* 0))

(defun remove-curr-shape-from-map ()
  (remove-shape-from-map *curr-shape* *curr-column* *curr-row*))

(defun add-curr-shape-to-map ()
  (add-shape-to-map *curr-shape* *curr-column* *curr-row*))


(defun test-number-of-complete-lines ()
  (add-shape-to-map '((x x x x x x x x x x x)) 0 8)
  (add-shape-to-map '((x x x x x x x x x x x)) 0 9)
  (show-map)
  (format t "number of complete lines: ~a " (number-of-complete-lines))
  (assert (= 2 (number-of-complete-lines))))

(defun number-of-complete-lines ()
  "returns number of made lines, counting from the bottom"
  ;; reverse the map and return the position of the first row that contains symbol -
  (let ((number 0))
    (position-if (lambda (row)
                   (if (find '- row)
                       (return-from number-of-complete-lines number)
                       (incf number))
                   nil)
                 (reverse *map*)
                 )))

(defun get-complete-lines ()
  (let ((lines nil)
        (line-num 0))
    (mapc (lambda (row)
            (when (every (lambda (symbol) (not (eq symbol '-)))
                         row)
              (push line-num lines))
            (incf line-num))
          *map*)
    lines))


(defun remove-complete-lines ()
  (let* ((lines (number-of-complete-lines)))
    (setf *map* (append (loop for y below lines collect (loop for x below +width+ collect '-))
                        (subseq *map* 0 (- +height+ lines))))))

(defun move-shape (delta-column delta-row)
  
  (let ((new-column (+ delta-column *curr-column*))
        (new-row (+ delta-row *curr-row*)))
    (cond ((or (will-shape-touch-others *curr-shape* new-column new-row)
               (will-shape-touch-floor *curr-shape* new-column new-row))
           ;;; restart piece
           (format t "Floor or piece touched!~%")
           (add-shape-to-map *curr-shape* *curr-column* *curr-row*) ; put piece on the spot
           (setf *curr-shape* nil) ;to gen new piece
           (when (> (number-of-complete-lines) 0)
             (cond (*hilighted-tiles*
                    (remove-complete-lines)
                    (setf *hilighted-tiles* nil))
                   (t
                    (hilight-complete-lines)
                    (setf *hilighted-tiles* t))))
           )
          ;;; move down
          ((will-shape-touch-walls *curr-shape* new-column new-row)
           (format t "Wall touched!~%")
           ) ; move it back)

          (t (setf *curr-row* new-row)
             (setf *curr-column* new-column)
             ))))

(defun left ()
  (game-tick -1 0))
(defun right ()
  (game-tick +1 0))
(defun down ()
  (game-tick 0 1))
(defun up ()
  (game-tick 0 -1))
(defun stop ()
  (game-tick 0 0))

(defun rotate ()
  (rotate-shape)
  (game-tick 0 0))

(defun game-tick (d-x d-y)
  (when (null *curr-shape*)
    (generate-new-piece)
    (when (will-shape-touch-others *curr-shape* *curr-column* *curr-row*)
      (game-over-screen)))
  (move-shape d-x d-y)
  ;; (when *curr-shape*
  ;;   (move-shape 0 1))
  )

(defun simple-repl ()
  (bt:make-thread (lambda ()
                    (loop while (not *game-over*)
                       do (progn (sleep 1)
                                 (down)
                                 (show-map)))))
  (loop
     ;;(format t " A - LEFT ~% S - DOWN ~% D - RIGHT~% Input: ")
     (case (read-char)
       (#\a (left))
       (#\s (down))
       (#\d (right))
       (#\r (rotate))
       (#\q (return-from simple-repl) (setf *game-over* t)))
     (show-map)))

(defun show (2d-list)
  (labels ((slashes-to-spaces (lst)
             (mapcar (lambda (l) (substitute #\SPACE '- l)) lst)))
    (format *standard-output* "|~{~a ~}|~%" (loop for i below +width+  collect #\=))
    (format *standard-output* "~{|~{~a ~}|~%~}" (slashes-to-spaces 2d-list))
    (format *standard-output* "|~{~a ~}|~%" (loop for i below +width+  collect #\=)))
  )


(defun show-map ()
  (format t "Current shape:~%")
  (show *curr-shape*)
  (format t "[X: ~a] [Y: ~a]~%" *curr-column* *curr-row*)
  (add-curr-shape-to-map)
  (show *map*)
  (remove-curr-shape-from-map))

(defun game-over-screen ()
  (setf *game-over* t)
  (format t "GAME OVER!~%"))

(simple-repl)

;; very cool code!
;; (reduce (lambda (list-a new-one)
;;           (mapcar (lambda (a b) (append (if (atom b)
;;                                        (list b)
;;                                        b)
;;                                    (if (atom a)
;;                                        (list a)
;;                                        a)))
;;                   list-a new-one))
;;  '((- x -)(x x x)(x x x)))
