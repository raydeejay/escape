;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:escape)

(defclass sprite ()
  ((id       :accessor id       :initarg :id)
   (name     :accessor name     :initarg :name)
   (x        :accessor x        :initarg :x        :initform 320)
   (y        :accessor y        :initarg :y        :initform 200)
   (heading  :accessor heading  :initarg :heading  :initform 90)
   (color    :accessor color    :initarg :color    :initform (list 1 1 1))
   (visible  :accessor visible  :initarg :visible  :initform T)
   (filename :accessor filename :initarg :filename)
   (texture  :accessor texture  :initarg :texture)
   (on-click :accessor on-click :initarg :on-click :initform nil)
   (selected :accessor selected :initarg :selected :initform nil)))

(defmethod initialize-instance :around ((sprite sprite)
                                        &rest initargs)
  (declare (ignore initargs))
  (call-next-method)
  (when (filename sprite)
    (load-texture sprite (filename sprite))))

(defmethod load-texture ((sprite sprite) filename)
  (sdl2:in-main-thread ()
    (let ((tex (gl:gen-texture)))
      (gl:enable :texture-2d)
      (gl:bind-texture :texture-2d tex)
      (gl:tex-parameter :texture-2d
                        :texture-mag-filter :linear)
      (gl:tex-parameter :texture-2d
                        :texture-min-filter :linear)
      (il:with-init
        (ilut:renderer :opengl)
        (let ((img (il:gen-image)))
          (il:with-bound-image img
            (let ((data (load-resource filename)))
              (cffi:with-foreign-array (ptr data `(:array :unsigned-char ,(length data)))
                (il:load-l :unknown ptr (length data))))
            (il:convert-image :rgba :unsigned-byte)
            (gl:tex-image-2d :texture-2d 0 :rgba
                             (il:image-width)
                             (il:image-height)
                             0 :rgba :unsigned-byte
                             (il:get-data))
            (setf (texture sprite)
                  (list tex
                        (il:image-width)
                        (il:image-height))))))))
  (gl:bind-texture :texture-2d 0)
  (gl:disable :texture-2d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *sprite* nil
  "The current sprite.")
(defparameter *sprites* nil
  "A list of all the sprites.")

;; (defun init-sprite ()
;;   (let ((sprite (make-sprite)))
;;     (setf *sprites* (append *sprites* (list sprite)))
;;     (setf *sprite* sprite)))

;; (defun new ()
;;   (init-sprite))

(defun make-sprite (id name filename &rest args)
  (apply 'make-instance 'sprite
         :id id
         :name name
         :filename filename
         args))

;; (defun clone ()
;;   (let ((trt *sprite*))
;;     (init-sprite)
;;     (apply 'ink (color trt))
;;     (setf (x *sprite*) (x trt)
;;           (y *sprite*) (y trt)
;;           (heading *sprite*) (heading trt))))

(defmethod draw ((sprite sprite))
  (when (visible sprite)
    ;; (let ((anchor (cons (x sprite)
    ;;                     (y sprite))))
    ;;   (labels ((transform (p)
    ;;              (from-origin
    ;;               (rotate-point
    ;;                (to-origin p anchor)
    ;;                (heading sprite))
    ;;               anchor)))
    ;;     (let ((p1 (transform
    ;;                   (cons (car anchor)
    ;;                         (- (cdr anchor) 4))))
    ;;           (p2 (transform
    ;;                   (cons (+ 13 (car anchor))
    ;;                         (cdr anchor))))
    ;;           (p3 (transform
    ;;                   (cons (car anchor)
    ;;                         (+ 4 (cdr anchor)))))))))
    (let ((id (first (texture sprite)))
          (width (second (texture sprite)))
          (height (third (texture sprite))))
      (gl:enable :texture-2d :blend)
      (gl:blend-func :src-alpha :one-minus-src-alpha)
      (gl:bind-texture :texture-2d id)
      (gl:color 1 1 1)
      (gl:with-primitives :quads
        (gl:tex-coord 0 0)
        (gl:vertex (x sprite) (y sprite))
        (gl:tex-coord 0 1)
        (gl:vertex (x sprite) (+ (y sprite) height))
        (gl:tex-coord 1 1)
        (gl:vertex (+ (x sprite) width) (+ (y sprite) height))
        (gl:tex-coord 1 0)
        (gl:vertex (+ (x sprite) width) (y sprite)))
      (gl:bind-texture :texture-2d 0)
      (gl:disable :texture-2d :blend))))

(defun render-sprites-for-picking ()
  (loop :for sprite :in (sprites *scene*)
     :for c :from 1 :do
     (when (visible sprite)
       ;; (let ((anchor (cons (x sprite)
       ;;                     (y sprite))))
       ;;   (labels ((transform (p)
       ;;              (from-origin
       ;;               (rotate-point
       ;;                (to-origin p anchor)
       ;;                (heading sprite))
       ;;               anchor)))
       ;;     (let ((p1 (transform
       ;;                   (cons (car anchor)
       ;;                         (- (cdr anchor) 4))))
       ;;           (p2 (transform
       ;;                   (cons (+ 13 (car anchor))
       ;;                         (cdr anchor))))
       ;;           (p3 (transform
       ;;                   (cons (car anchor)
       ;;                         (+ 4 (cdr anchor)))))))))
       (let (;; (id (first (texture sprite)))
             (width (second (texture sprite)))
             (height (third (texture sprite))))
         (apply 'gl:color (list (/ c 255.0) 0 0))
         (gl:with-primitives :quads
           (gl:tex-coord 0 0)
           (gl:vertex (x sprite) (y sprite))
           (gl:tex-coord 0 1)
           (gl:vertex (x sprite) (+ (y sprite) height))
           (gl:tex-coord 1 1)
           (gl:vertex (+ (x sprite) width) (+ (y sprite) height))
           (gl:tex-coord 1 0)
           (gl:vertex (+ (x sprite) width) (y sprite)))))))


;; sprite primitives
(defun forward (n)
  (let ((dx (* n (cos (to-radians (heading *sprite*)))))
        (dy (* n (sin (to-radians (heading *sprite*))))))
    ;; calculate the new coordinates and call goto
    ;; decrement because the Y axis is backwards
    (let ((new-x (+ (x *sprite*) dx))
          (new-y (- (y *sprite*) dy)))
      (goto new-x new-y))))

(defun back (n)
  (forward (- n)))

(defun left (n)
  (incf (heading *sprite*) n))

(defun right (n)
  (decf (heading *sprite*) n))

(defun point-to (n)
  (setf (heading *sprite*) n))

;; Y axis is inverted
(defun point-at (x y)
  (let ((dx (- x (x *sprite*)))
        (dy (- (y *sprite*) y)))
    (setf (heading *sprite*)
          (/ (* (atan dy dx) 180) pi))))

(defun home ()
  (goto 320 200))

(defun goto (x y)
  (setf (x *sprite*) x
        (y *sprite*) y))

(defun coords ()
  (cons (x *sprite*) (y *sprite*)))

(defun hide ()
  (setf (visible *sprite*) NIL))

(defun show ()
  (setf (visible *sprite*) T))
