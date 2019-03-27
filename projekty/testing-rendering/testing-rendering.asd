;;;; testing-rendering.asd


(asdf:defsystem #:testing-rendering
  :description "System for visualizaion of tetris"
  :author "Andrzej Bisewski <andrzej.bisewski@gmail.com>"
  :license  "you can use, just don't make money of it"
  :version "0.0.1"
  :serial t
  :depends-on (#:cepl.sdl2
               #:cepl
               #:nineveh
               #:livesupport
               #:cepl.skitter.sdl2
               #:harmony-simple
               #:harmony-out123
               #:bt-semaphore
               #:tetris
               )
  :components (
               (:file "testing-rendering")
               (:file "sounds")
