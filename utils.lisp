;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:escape)

;; helper
(defun <- (key plist)
  (assoc-value (plist-alist plist) key :test 'equal))

(defun dismiss (&rest args)
  "Takes any number of arguments and simply ignores them, doing absolutely nothing."
  (declare (ignore args)))

(defun to-radians (degrees)
  (/ (* degrees pi) 180.0))

(defun to-degrees (radians)
  (/ (* radians 180.0) pi))

;; macros
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro toggle (place)
    `(setf ,place (not ,place))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro once (body)
    `(let (fired)
       (,@(subseq body 0 2)
          (when (not fired)
            (setf fired T)
            ,@(subseq body 2))))))
