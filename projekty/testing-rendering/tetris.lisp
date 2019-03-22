

(ql:quickload :bt-semaphore)
(ql:quickload :cepl.sdl2)
(ql:quickload :livesupport)
(defpackage #:tetris
  (:use :bt-semaphore
        :cepl.sdl2
        :livesupport
        :cl)
  (:export :+width+
           :+height+
           :*curr-shape*
           :*curr-row*
           :*curr-column*
           :*map*
           :*game-over*
           :left
           :right
           :down
           :drop-down
           :get-ghost-shape-cords
           :get-current-ghost-shape
           :get-current-color
           :rotate
           :create-computer
           :*shape-touched-callback*
           :a :b :c :d :e :f :g :h :x :* :-
           :SYMBOL-AT
           ))

(in-package #:tetris)

(defvar +width+ 10 "width of the map")
(defvar +height+ 20 "height of the map")

(defparameter *map* (loop for rows below +height+
                       collect (loop for column below +width+
                                  collect '-)) "List of lists representing map")

(defparameter *shapes* nil "Alist containing all of the shapes.")

(defparameter *curr-shape* nil)
(defparameter *curr-shape-number* nil)
(defparameter *curr-column* nil)
(defparameter *curr-row* nil)
(defparameter *hilighted-rows* nil)
(defparameter *game-over* nil)
(defparameter *events* nil)
(defparameter *lock* (bt:make-lock))
(defparameter *difficulty* 1)
(defparameter *shape-touched-callback* (lambda (piece-color)
                                         (declare (ignore piece-color))
                                         nil))

(defun init-tetris ()
  (setf *map* (loop for rows below +height+
                 collect (loop for column below +width+
                            collect '-))
        *curr-shape* nil
        *events* nil
        *game-over* nil
        *hilighted-rows* nil
        *difficulty* 1))


(push '(o . (((a a)
              (a a))
             a)) *shapes*)

(push '(t . (((b b b)
              (- b -))
             b)) *shapes*)

(push '(j . (((- c)
              (- c)
              (c c))
             c)) *shapes*)

(push '(z . (((- d d)
              (d d -))
             d)) *shapes*)

(push '(z-inv . (((e e -)
                  (- e e))
                 e)) *shapes*)

(push '(l . (((f -)
              (f -)
              (f f))
             f)) *shapes*)

(push '(i . (((g -)
              (g -)
              (g -)
              (g -))
             g)) *shapes*)

#+nil(push '(duzy-kloc . (((- a e)
                      (- b f)
                      (- c g)
                      (- d *))
                     * )) *shapes*)

(defun get-current-colored-shape ()
  (color-shape *curr-shape* (get-color *curr-shape-number*)))

(defun color-shape (shape for)
  (subst for 'x shape))

(defun get-random-shape ()
  (let* ((num-of-shapes (length *shapes*))
         (choice (random num-of-shapes)))
    (values (get-shape choice)
            choice)))

;; ref by number
(defmethod get-shape ((num number))
  (cadr (nth num *shapes*)))

;; ref by letter
(defmethod get-shape ((s symbol))
  (cadr (assoc s *shapes*)))

(defmethod get-color ((num number))
  (caddr (nth num *shapes*)))

(defmethod get-color ((s symbol))
  (caddr (assoc s *shapes*)))

(defun get-current-color ()
  (get-color *curr-shape-number*))

(flet ((help-get-ghost-shape (shape-list)
         "Replaces all colors with '+"
         (subst-if '+ (lambda (symbol)
                        (if (and (not (eql symbol '-))
                                 (not (listp symbol)))
                            t))
                   shape-list)))
  (defmethod get-ghost-shape ((s symbol))
    (help-get-ghost-shape (get-shape s)))
  (defmethod get-ghost-shape ((n number))
    (help-get-ghost-shape (get-shape n)))
  (defun get-current-ghost-shape ()
    (help-get-ghost-shape *curr-shape*)))


(defun get-ghost-shape-cords ()
  "Returns (VALUES X Y) that's are cords for ghost shape
   or nil"
  (let ((ghost-shape (get-current-ghost-shape)))
    (loop for row from *curr-row* to +height+ ; to should be -1, but it doesnt mater
       when (or (will-shape-touch-others ghost-shape *curr-column* row)
                 (will-shape-touch-floor ghost-shape *curr-column* row))
       do (return-from get-ghost-shape-cords
            (values *curr-column*
                    (1- row)))))
  nil)

;; '((- x -)(x x x)(x x x)) => '((x x -)(x x x)(x x -))
;;
;;    - x -        x x -
;;    x x x   ->   x x x
;;    x x x        x x -
(defun rotate-h (shape)
  (when shape
    (mapcar 'reverse
            (apply #'mapcar #'list shape))))

(defun rotate-shape ()
  (let ((rotated (rotate-h *curr-shape*)))
    (unless (or (will-shape-touch-others rotated *curr-column* *curr-row*)
                (will-shape-touch-floor rotated *curr-column* *curr-row*)
                (will-shape-touch-walls rotated *curr-column* *curr-row*))
      (setf *curr-shape* rotated))))


(defmacro symbol-at (col row 2d-list &optional check)
  (if check
      `(if (or (< ,col 0)
               (< ,row 0)
               (<= (length ,2d-list), row)
               (<= (length (car ,2d-list)) ,col))
           '- ;; out of bouts
           (nth ,col
                (nth ,row ,2d-list)))
      `(nth ,col
            (nth ,row ,2d-list))))

(defun will-shape-touch-others (shape start-col start-row)
  (loop for row below (length shape)
     do (loop for column below (length (car shape))
           for shape-symbol-at = (symbol-at column
                                            row
                                            shape t)
           for map-symbol-at = (symbol-at (+ column start-col)
                                          (+ row start-row)
                                          *map* t)
           when (and (not (eql shape-symbol-at '-))
                     (not (eql map-symbol-at '-)))
           do (progn
                #+nil(format t "~a ~a" shape-symbol-at map-symbol-at)
                (return-from will-shape-touch-others t))))
  nil)


(defun will-shape-touch-floor (shape start-col start-row)
  (declare (ignore start-col))
  (loop for row from (1- (length shape)) downto 0
     when (find-if (lambda (symbol-at) (not (eql '- symbol-at)))
                   (nth row shape)) ; block starts on this row
     do (return-from will-shape-touch-floor 
          (<= +height+ (+ start-row row)))))

(defun will-shape-touch-walls (shape start-col start-row)
  (declare (ignore start-row))
  ;; find x to se where to shape starts
  ;; if nil is not NUMBEr then its because tha shapes has no pieces
  (labels ((find-first-x (shape)
             (loop for column below (length (car shape))
                do (loop for row below (length shape)
                      for shape-symbol = (symbol-at column
                                                    row
                                                    shape)
                      if (not (eq shape-symbol '-))
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
           when (and (not (eql shape-symbol '-)) ;; not empty
                     ;; checks whatever symbol is stil inbouds of map
                     (not (null (symbol-at (+ start-col column)
                                           (+ start-row row)
                                           *map* t))))
           do (setf (symbol-at (+ start-col column)
                               (+ start-row row)
                               *map*)
                    shape-symbol))))


(defun remove-shape-from-map (shape start-col start-row)
  (loop for row below (length shape)
     do (loop for column below (length (car shape))
           for shape-symbol = (symbol-at column
                                         row
                                         shape)
           when (not (eq shape-symbol '-)) ;; eq. 'x. Byp
           do (setf (symbol-at (+ start-col column)
                               (+ start-row row)
                               *map*)
                    '-))))


(defun generate-new-piece ()
  (format t "Generating new piece.~%")
  (multiple-value-setq (*curr-shape* *curr-shape-number*)
    (get-random-shape))
  (setf *curr-column* (floor (/ +width+ 2))
        *curr-row* 0))

(defun remove-curr-shape-from-map ()
  (remove-shape-from-map *curr-shape* *curr-column* *curr-row*))

(defun add-curr-shape-to-map ()
  (add-shape-to-map (get-current-colored-shape)
                    *curr-column*
                    *curr-row*))


(defun test-number-of-complete-lines ()
  (add-shape-to-map '((x x x x x x x x x x x)) 0 8)
  (add-shape-to-map '((x x x x x x x x x x x)) 0 9)
  (show-map)
  (format t "number of complete lines: ~a " (number-of-complete-lines))
  '(assert (equal (list 8 9) (number-of-complete-lines))))

(defun number-of-complete-lines ()
  "returns number of made lines, counting from the bottom"
  ;; reverse the map and return the position of the first row that contains symbol -
  (length (get-complete-lines)))

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

(defun hilight-complete-lines ()
  (let* ((lines (get-complete-lines))
         (hilighted-line (loop for x below +width+ collect '*)))
    (mapc (lambda (row-num)
            (setf (nth row-num *map*)
                  hilighted-line))
          lines)
    lines))

(defun remove-complete-lines ()
  (format t "Removing lines~%")
  (let* ((lines (get-complete-lines)))
    (setf *map* (append (loop ;add empty lines on top
                          for y below (length lines) collect (loop for x below +width+ collect '-))
                       (loop ;collect not empty lines
                          for row below +height+
                          when (not (find row lines))
                          collect (nth row *map*))))))

(defun move-shape (delta-column delta-row)
  (let ((new-column (+ delta-column *curr-column*))
        (new-row (+ delta-row *curr-row*)))
    (cond ((or (will-shape-touch-others *curr-shape* new-column new-row)
               (will-shape-touch-floor *curr-shape* new-column new-row))
           ;;; restart piece --
           (format t "Floor or piece touched!~%")
           (add-curr-shape-to-map) ; put piece on the spot
           (funcall *shape-touched-callback* ; a cool animation 
                    (get-color *curr-shape-number*))
           (setf *curr-shape* nil) ;to gen new piece
           
           )
          ;;; do nothing --
          ((will-shape-touch-walls *curr-shape* new-column new-row)
           (format t "Wall touched!~%")
           )
          ;;; move down --
          (t (setf *curr-row* new-row)
             (setf *curr-column* new-column)))
    (when (get-complete-lines)
      (push '(lambda ()
              (remove-complete-lines)) *events*) ;; execute later
      (hilight-complete-lines)
      (format t "Hilighting completed lines~%")
      (funcall *shape-touched-callback* ; a cool animation 
               '*))))

(defun execute-all-events ()
  (mapc (lambda (event) (funcall (eval event))) *events* )
  (setf *events* nil))

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
(defun drop-down ()
  (format t "Dropping down ~%")
  (loop while *curr-shape*
     do (game-tick 0 1)))

(defun rotate ()
  (rotate-shape)
  (game-tick 0 0))

(defun increase-difficulty ()
  (setf *difficulty* (* *difficulty*
                        0.999)))

(defun game-tick (d-x d-y)
  (format t "~a~%" (multiple-value-list (tetris:get-ghost-shape-cords)))
  (bt:with-lock-held (*lock*)
    (execute-all-events)
    (when (null *curr-shape*)
      (generate-new-piece)
      (when (will-shape-touch-others *curr-shape* *curr-column* *curr-row*)
        (game-over-screen)
        (init-tetris)
        (return-from game-tick)))
    (move-shape d-x d-y)
    ;; (when *curr-shape*
    ;;   (move-shape 0 1))
    ))

(defun computer-loop ()
    (loop while (not *game-over*)
       do (progn (sleep *difficulty*)
                 (increase-difficulty)
                 (down)
                 (show-map))))

(defun create-computer ()
  (bt:make-thread #'computer-loop))

(defun simple-repl ()
  (create-computer)
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
  (show (get-current-colored-shape))
  (format t "[X: ~a] [Y: ~a]~%" *curr-column* *curr-row*)
  (add-curr-shape-to-map)
  (show *map*)
  (remove-curr-shape-from-map))

(defun game-over-screen ()
  (setf *game-over* t)
  (format t "GAME OVER!~%"))



'(simple-repl)

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

