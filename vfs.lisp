;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:escape)

(defparameter *vfs* nil)

(defun build-vfs ()
  (setf *vfs* (make-hash-table :test 'equal))
  (loop :for file :in (cl-fad:list-directory "images/") :do
     (let ((resource-name (format nil "images/~A" (file-namestring file))))
       (setf (gethash resource-name *vfs*)
             (alexandria:read-file-into-byte-vector file)))))

(defun load-resource (filename)
  (gethash filename *vfs*))
