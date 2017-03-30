;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:escape)

(defparameter *display-window* nil)
(defparameter *display-thread* nil)
(defparameter *display-width* 640)
(defparameter *display-height* 400)

(defparameter *paper* (list 0 0 0 1)
  "RGBA color of the background of the drawing field.")

;; accounting for the inverted Y axis and operating on the fourth quadrant
;; not quite sure how... other than by swapping SIN and COS in the usual formula
(defun rotate-point (p degrees)
  (let ((f (to-radians degrees)))
    (cons (+ (* (car p) (cos f)) (* (cdr p) (sin f)))
          (- (* (cdr p) (cos f)) (* (car p) (sin f))))))

(defun to-origin (p anchor)
  (cons (- (car p) (car anchor))
        (- (cdr p) (cdr anchor))))

(defun from-origin (p anchor)
  (cons (+ (car p) (car anchor))
        (+ (cdr p) (cdr anchor))))

(defun gl-setup (&optional (width *display-width*) (height *display-height*))
  "Set up 1:1 pixel ortho matrix"
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho 0 width height 0 -1 1)
  (gl:matrix-mode :modelview))


(defun idle-func (win)
  (declare (ignore win))
  ;; redraw lines
  (loop :for sprite :in *sprites*
     :initially (progn (gl:load-identity)
                       (gl-setup *display-width* *display-height*)
                       ;; clear the display
                       (destructuring-bind (r g b a)
                           *paper*
                         (gl:clear-color r g b a))
                       (gl:clear :color-buffer :depth-buffer))

     :finally (progn (draw-sprites))))


(defun handle-keydown (keysym)
  (let ((scancode (sdl2:scancode-value keysym))
        ;; (sym (sdl2:sym-value keysym))
        ;; (mod-value (sdl2:mod-value keysym))
        )
    (cond
      ((sdl2:scancode= scancode :scancode-w)
       (forward 10))
      ((sdl2:scancode= scancode :scancode-s)
       (back 10))
      ((sdl2:scancode= scancode :scancode-a)
       (left 15))
      ((sdl2:scancode= scancode :scancode-d)
       (right 15)))
    ;; (format t "Key sym: ~a, code: ~a, mod: ~a~%"
    ;;         sym
    ;;         scancode
    ;;         mod-value)
    ))

(defun handle-keyup (keysym)
  (when (sdl2:scancode= (sdl2:scancode-value keysym)
                        :scancode-escape)
    (sdl2:push-event :quit)))

(defun handle-mouse (button)
  (declare (ignore button))
  (multiple-value-bind (x y)
      (sdl2:mouse-state)
    (gl:clear-color 0 0 0 1)
    (gl:clear :color-buffer)
    (gl:enable :scissor-test)
    (gl:scissor x (- *display-height* y) 1 1)
    (render-sprites-for-picking)
    (let ((c (1- (elt (gl:read-pixels x (- *display-height* y) 1 1
                                      :rgba :unsigned-byte)
                      0))))
      (when (not (minusp c))
        (let ((*sprite* (nth c *sprites*)))
          ;; action here
          (forward 40)))))
  (gl:scissor 0 0 *display-width* *display-height*)
  (gl:disable :scissor-test))

(defun open-display% ()
  (sdl2:with-init
    (:everything)
    (sdl2:with-window (win :title "Minilang Display"
                           :w *display-width* :h *display-height*
                           :flags '(:shown :opengl))
      (setf *display-window* win)
      (sdl2:with-gl-context (gl-context win)
        (setf *sprite* (make-instance 'sprite :filename "fire.png"))
        (setf *sprites* (list *sprite*))

        (sdl2:gl-make-current win gl-context)
        (gl-setup *display-width* *display-height*)
        (destructuring-bind (r g b a)
            *paper*
          (gl:clear-color r g b a))
        (gl:clear :color-buffer :depth-buffer)
        (load-textures)
        
        (sdl2:with-event-loop (:method :poll)
          (:quit () t)

          (:keydown
           (:keysym keysym)
           (handle-keydown keysym))

          (:keyup
           (:keysym keysym)
           (handle-keyup keysym))

          (:mousebuttondown
           (:button button)
           (handle-mouse button))

          (:idle
           ()
           (idle-func win)
           (sdl2:gl-swap-window win)
           (sleep 0.001)))))))          ; naively 100fps?

(defun open-display ()
  (setf *display-thread*
        (make-thread 'open-display%
                     :name "Minilang Display")))

(defun close-display ()
  (sdl2:push-quit-event))

(defun paper (r g b &optional (a 1.0))
  (setf *paper* (list r g b a)))

(defun save-png (filename)
  (il:with-init
    (sdl2:in-main-thread ()
      (ilut:renderer :opengl)
      (let ((screenshot (il:gen-image)))
        (il:with-bound-image screenshot
          (ilut:gl-screen)
          (il:save-image filename))))))
