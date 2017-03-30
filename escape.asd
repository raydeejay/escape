;;;; escape.asd

(asdf:defsystem #:escape
  :description "Describe escape here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (#:alexandria
               #:parse-float
               #:arnesi
               #:sdl2
               #:cl-opengl
               #:bordeaux-threads
               #:cl-svg
               #:cl-devil
               #:cl-ilut)
  :components ((:file "package")
               (:file "utils")
               (:file "textures")
               (:file "sprites")
               (:file "display")
               (:file "escape")))

