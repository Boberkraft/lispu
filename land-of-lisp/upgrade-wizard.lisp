(load "wizards_game.lisp")


(defparameter *bucket-filled* nil)

(defun dunk (subject object)
  (if (and (eq *location* 'garden)
           (eq subject 'bucket)
           (eq object 'well)
           (have 'bucket)
           *chain-welded*)
      (progn (setf *bucket-filled* 't)
             '(the bucket is now full of water))
      '(you cannot dunk like that.)))

(defmacro game-action (command subj obj place &body body)
  `(progn (defun ,command (subject object)
            (if (and (eq *location* ',place)
                     (eq subject ',subj)
                     (eq object ',obj)
                     (have ',subj))
                ,@body
                '(i cant, command like that.)))
          (pushnew ',command *allowed-commands*)))

(defparameter *chain-welderd* nil)



(setf *bucket-filled* nil)

(game-action dunk bucket well garden
  (if *chain-welderd*
      (progn (setf *bucket-filled* t)
             '(the bucket is now full of water.))
      '(the water level is too low to reach.)))



(macroexpand (game-action splash bucket wizard living-room
               (cond ((not *bucket-filled*) '(the bucket has nothing in it.))
                     ((have 'frog) '(the wizzard awakens and sees that
                                     you stole his frog.
                                     he is so upset he banishes you to the
                                     netherworlds- you lose! the end.))
                     (t '(the wizzard awakens from his slumber and
                          greets you warmly.
                          he hands you the magic low-carb donut-
                          you win! the end.)))))

