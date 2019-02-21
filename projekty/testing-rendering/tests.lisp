
(ql:quickload :prove)
(use-package :prove)
(setf *enable-colors* nil)

(subtest "Tetris.lisp"
  (init-tetris))
(is (+ 4 4) 4)
