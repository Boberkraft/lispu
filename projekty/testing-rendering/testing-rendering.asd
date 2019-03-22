;;;; testing-rendering.asd

(asdf:defsystem #:testing-rendering
  :description "Describe testing-rendering here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:cepl.sdl2
               #:nineveh
               #:livesupport
               #:cepl.skitter.sdl2
               #:harmony-simple
               #:harmony-out123
               )
  :components ((:file "package")
               (:file "testing-rendering")
               (:file "tetris")))
