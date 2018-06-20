
(defparameter *num-players* 2)
(defparameter *max-dice* 3)
(defparameter *board-size* 2)
(defparameter *board-hexnum* (* *board-size* *board-size*))


(defun board-array (lst)
  (make-array *board-hexnum* :initial-contents lst))

(defun gen-board ()
  (board-array (loop for n below *board-hexnum*
                  collect (list (random *num-players*)
                                (1+ (random *max-dice*))))))

(defun player-letter (n)
  (code-char (+ 97 n)))
(player-letter 1)

(defun draw-board (board)
  (loop for y below *board-size*
     do (progn (fresh-line)
               (loop repeat (* 2 (- *board-size* y 1))
                  do (princ " "))
               (loop for x below *board-size*
                  for hex = (aref board (+ x (* *board-size* y)))
                  do (format t "~a-~a " (player-letter (first hex))
                             (second hex))))))



(progn
  (terpri)
  (draw-board #((0 3) (0 3) (1 3) (1 1))))

(defun game-tree (board player spare-dice first-move)
  (list player
        board
        (add-passing-move board
                          player
                          spare-dice
                          first-move
                          (attacking-moves board
                                           player
                                           spare-dice))))

(defun add-passing-move (board player spare-dice first-move moves)
  (if first-move
      moves
      (cons (list nil
                  (game-tree (add-new-dice board player (1- spare-dice))
                             (mod (1+ player) *num-players*)
                             0
                             t))
            moves)))

(defun attacking-move (board cur-player spare-dice)
  (labels ((player (pos)
             (car (aref board pos)))
           (dice (post)
             (cadr (aref board pos))))
    (mapcan (lambda (src)
              (when (eq (player src) ;; kiedy to twoje pole
                        cur-player)
                (mapcan (lambda (dst)
                          (when (and (not (eq (player dst) ;; kiedy nie jest to twoja wlase pole
                                              cur-player))
                                     (> (dice src) (dice dst))) ;; kiedy 
                            (list
                             (list (list src dst)
                                   (game-tree (board-attack board cur-player src dst (dice src))
                                              cur-player
                                              (+ spare-dice (dice dst))
                                              nil)))))
                        (neighbors src))))
            (loop for n below *board-hexnum*
               collect n))))

(defun neighbors (pos)
  (let ((up (- pos *board-size*))
        (down (+ pos *board-size*)))
    (loop
       for p in (append (list up down)
                        (unless (zerop (mod pos *board-size*))
                          (list (1- up) (1- pos)))
                        (unless (zerop (mod (1+ pos) *board-size*))
                          (list (1+ pos) (1+ down))))
       when (and (>= p 0)
                 (< p *board-hexnum*))
       collect p)))

(defun board-attack (board player src dst dice)
  (board-array (loop
                  for pos
                  for hex across board
                  collect (cond ((eql pos src)
                                 (list player 1))
                                ((eql pos dst)
                                 (list player (1- dice)))
                                (t
                                 hex)))))
;;🌕🌕🌕🌕🌕🌕🌕🌕
;;🌕🌕🌕🌕🌘🌑🌒🌕
;;🌕🌕🌕🌘🌑🌑🌑🌓
;;🌕🌕🌖🌑👁🌑👁🌓
;;🌕🌕🌗🌑🌑👅🌑🌔
;;🌕🌕🌘🌑🌑🌑🌒🌕
;;🌕🌕🌘🌑🌑🌑🌓🌕
;;🌕🌕🌘🌑🌑🌑🌔🌕
;;🌕🌕🌘🌔🌘🌑🌕🌕
;;🌕🌖🌒🌕🌗🌒🌕🌕
;;🌕🌗🌓🌕🌗🌓🌕🌕
;;🌕🌘🌔🌕🌗🌓🌕🌕
;;🌖🌑🌔🌕🌗🌓🌕🌕
