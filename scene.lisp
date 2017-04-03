;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:escape)

(enable-block-syntax)

(defparameter *scene* nil)
(defparameter *scenes* nil)

(defclass scene ()
  ((name       :accessor name       :initarg :name       :initform nil)
   (background :accessor background :initarg :background :initform nil)
   (sprites    :accessor sprites    :initarg :sprites    :initform nil)))

(defmethod @ ((scene scene) (symbol symbol))
    (find symbol (sprites scene) :key 'id :test 'equal))

(defmethod add ((scene scene) (sprite sprite))
  (setf (sprites scene)
        (append (sprites scene) (list sprite))))

(defmethod discard ((scene scene) (symbol symbol))
  "Extracts an object from a scene and returns it."
  (let ((removed (find symbol (sprites scene) :key 'id :test 'equal)))
    (setf (sprites scene) (remove removed (sprites scene)))
    removed))

(defmethod draw ((scene scene))
  (draw (background scene))
  (loop :for sprite :in (sprites scene) :do
     (draw sprite)))

(defun spawn (scene-def)
  (destructuring-bind (name background . sprites)
      scene-def
    (let ((scene (make-instance 'scene
                                :name name
                                :background (make-sprite :background "Background" background
                                                         :x 0 :y 0)
                                :sprites (loop :for s :in sprites :collect
                                            (switch (s :test 'equal :key 'car)
                                              (:left (make-sprite (car s) "left" "images/arrowLeft.png" :x 10 :y 240
                                                                  :on-click (let ((target (cadr s)))
                                                                              [(switch-to-scene (funcall target))])))
                                              (:right (make-sprite (car s) "right" "images/arrowRight.png" :x 630 :y 240
                                                                   :on-click (let ((target (cadr s)))
                                                                               [(switch-to-scene (funcall target))])))
                                              (otherwise
                                               (apply 'make-sprite
                                                      (car s)
                                                      (cadr s)
                                                      (caddr s)
                                                      (cdddr s))))))))
      (push scene *scenes*)
      scene)))

(defun switch-to-scene (scene-def)
  (let ((scene (find (car scene-def) *scenes* :key 'name :test 'equal)))
    (setf *scene* (or scene (spawn scene-def)))))

(defun build-world ()
  ;; (freetype2:with-open-face (font "Vera.ttf")
  ;;   (freetype2:set-pixel-sizes font 0 48)
  ;;   (freetype2:print-with-face ))
  (setf *scenes* nil)
  (switch-to-scene (room01))
  (setf *inventory*
        (make-instance 'inventory
                       :background (make-sprite :background "Background" "images/inventory.png"
                                                :x *inventory-x* :y 0))))
