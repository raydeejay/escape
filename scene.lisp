;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:escape)

(enable-block-syntax)

(defparameter *scene* nil)
(defparameter *scenes* nil)

(defclass scene ()
  ((name       :accessor name       :initarg :name       :initform nil)
   (background :accessor background :initarg :background :initform nil)
   (sprites    :accessor sprites    :initarg :sprites    :initform nil)))

(defun draw-scene ()
  (draw (background *scene*))
  (loop :for sprite :in (sprites *scene*) :do
     (draw sprite)))

(defmethod add ((scene scene) (sprite sprite))
  (setf (sprites scene)
        (append (sprites scene) (list sprite))))

(defmethod discard ((scene scene) (symbol symbol))
  "Extracts an object from a scene and returns it."
  (let ((removed (find symbol (sprites scene) :key 'id :test 'equal)))
    (setf (sprites scene) (remove removed (sprites scene)))
    removed))

(defmethod @ ((scene scene) (symbol symbol))
    (find symbol (sprites scene) :key 'id :test 'equal))

(defun spawn (scene-def)
  (destructuring-bind (name background . sprites)
      scene-def
    (let ((scene (make-instance 'scene
                                :name name
                                :background (make-sprite :background "Background" background
                                                         :x 0 :y 0)
                                :sprites (loop :for s :in sprites :collect
                                            (apply 'make-sprite
                                                   (car s)
                                                   (cadr s)
                                                   (caddr s)
                                                   (cdddr s))))))
      (push scene *scenes*)
      scene)))

(defun switch-to-scene (scene-def)
  (let ((scene (find (car scene-def) *scenes* :key 'name :test 'equal)))
    (setf *scene* (or scene (spawn scene-def)))))

(defun make-scene ()
  ;; (freetype2:with-open-face (font "Vera.ttf")
  ;;   (freetype2:set-pixel-sizes font 0 48)
  ;;   (freetype2:print-with-face ))
  (setf *scenes* nil)
  (switch-to-scene (room01))
  (setf *inventory*
        (make-instance 'inventory
                       :background (make-sprite :background "Background" "images/inventory.png"
                                                :x *inventory-x* :y 0))))
