;;;; escape.lisp

(in-package #:escape)

;;; "escape" goes here. Hacks and glory await!

(defparameter *scene* nil)

(defclass scene ()
  ((name    :accessor name    :initarg :name    :initform nil)
   (sprites :accessor sprites :initarg :sprites :initform nil)))

(defmethod add ((scene scene) (sprite sprite))
  (setf (sprites scene)
        (append (sprites scene) (list sprite))))

(defmethod discard ((scene scene) (symbol symbol))
  (setf (sprites scene)
        (remove symbol (sprites scene) :key 'id)))

(defun make-scene ()
  (let ((background (make-instance 'sprite
                                   :id 'background
                                   :name "Background"
                                   :filename "images/room01.png"
                                   :x 0 :y 0))
        (fire (make-instance 'sprite
                             :id 'fire
                             :name "Fire"
                             :filename "images/fire.png"
                             :x 560
                             :y 380))
        (door (make-instance 'sprite
                             :id 'door
                             :name "Door"
                             :filename "images/door.png"
                             :x 260 :y 144
                             :on-click (let ((fired nil))
                                         (lambda ()
                                           (when (not fired)
                                             (setf fired T)
                                             (add *scene*
                                                  (make-instance
                                                   'sprite
                                                   :id 'knife
                                                   :name "Knife"
                                                   :filename "images/knife.png"
                                                   :x 100 :y 400
                                                   :on-click
                                                   (lambda ()
                                                     (discard *scene*
                                                             'knife))))))))))
    (setf *scene* (make-instance 'scene
                                 :name "room 1"
                                 :sprites (list background door fire)))))

(defun run ()
  (open-display))
