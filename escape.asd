;;;; escape.asd

(asdf:defsystem #:escape
  :description "Describe escape here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (#:alexandria
               #:cl-fad
               #:parse-float
               #:arnesi
               #:sdl2
               #:cl-opengl
               #:bordeaux-threads
               #:cl-svg
               #:cl-devil
               #:cl-ilut
               #:smalltalk-block-syntax)
  :components ((:file "package")
               (:file "utils")
               (:file "api")
               (:file "vfs")
               (:file "sprites")
               (:file "inventory")
               (:file "scene")
               (:file "display")
               (:file "escape")))

