;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:escape)

(defparameter *inventory* nil)
(defparameter *inventory-x* 700)
(defparameter *inventory-y-offset* 20)
(defparameter *unselected-size* 40)
(defparameter *selected-size* 48)

(defclass inventory ()
  ((items      :accessor items      :initarg :items      :initform nil)
   (background :accessor background :initarg :background :initform nil)
   (selected   :accessor selected   :initarg :selected   :initform nil)))

(defun draw-inventory ()
  (draw (background *inventory*))
  (loop :for item :in (items *inventory*) :do
     (if (equal (selected *inventory*) item)
         (progn (setf (texture item) (list (car (texture item)) *selected-size* *selected-size*))
                (draw item)
                (setf (texture item) (list (car (texture item)) *unselected-size* *unselected-size*)))
         (draw item))))

(defun coords-for-position (n)
  (let ((x (+ 5 *inventory-x* (* 50 (mod n 2))))
        (y (+ 10 (* 60 (truncate (/ n 2))))))
    (values x y)))

(defmethod reorder ((inv inventory))
  (loop :for item :in (items inv)
     :for n :by 1 :doing
     (multiple-value-bind (x y)
         (coords-for-position n)
       (setf (x item) x
             (y item) y
             (texture item) (list (car (texture item))
                                  *unselected-size*
                                  *unselected-size*)))))

(defmethod add ((inv inventory) (sprite sprite))
  (setf (items inv) (append (items inv) (list sprite)))
  (reorder inv))

(defmethod discard ((inv inventory) (symbol symbol))
  "Extracts an object from an inventory and returns it."
  (let ((removed (find symbol (items inv) :key 'id :test 'equal)))
    (setf (items inv) (remove removed (items inv)))
    (reorder inv)
    removed))

(defmethod select ((inv inventory) (sprite sprite))
  (setf (selected inv) sprite))

(defun selectedp (key)
  (and (selected *inventory*)
       (equal (selected *inventory*)
              (@ *inventory* key))))

(defmethod @ ((inv inventory) (symbol symbol))
    (find symbol (items inv) :key 'id :test 'equal))

(defun render-inventory-for-picking ()
  (loop :for item :in (items *inventory*)
     :for c :from 1 :do
     (when (visible item)
       ;; (let ((anchor (cons (x item)
       ;;                     (y item))))
       ;;   (labels ((transform (p)
       ;;              (from-origin
       ;;               (rotate-point
       ;;                (to-origin p anchor)
       ;;                (heading item))
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
       (let (;; (id (first (texture item)))
             (width (second (texture item)))
             (height (third (texture item))))
         (apply 'gl:color (list (/ c 255.0) 0 0))
         (gl:with-primitives :quads
           (gl:tex-coord 0 0)
           (gl:vertex (x item) (y item))
           (gl:tex-coord 0 1)
           (gl:vertex (x item) (+ (y item) height))
           (gl:tex-coord 1 1)
           (gl:vertex (+ (x item) width) (+ (y item) height))
           (gl:tex-coord 1 0)
           (gl:vertex (+ (x item) width) (y item)))))))
