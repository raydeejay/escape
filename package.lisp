;;;; package.lisp

(defpackage #:escape
  (:use #:cl #:alexandria #:bordeaux-threads #:smalltalk-block-syntax)
  (:export #:run
           #:buildapp-entry-point
           #:build-vfs))

