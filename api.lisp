;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:escape)

;; these apply both to scenes and inventory
(defgeneric @ (place obj)
    (:documentation "Reference an object from a scene or inventory."))

(defgeneric add (place obj)
  (:documentation "Adds a sprite to a scene or inventory."))

(defgeneric discard (place id)
  (:documentation "Extract and return an object from a scene or inventory."))

(defgeneric draw (obj)
  (:documentation "Draws the object on screen."))


;; sprite specific methods
(defgeneric load-texture (obj filename0)
  (:documentation "Loads the image from the VFS into an OpenGL texture."))


;; inventory specific methods
(defgeneric reorder (inventory)
  (:documentation "Calculate and assign proper coordinates for items in the inventory."))

(defgeneric select (inventory obj)
  (:documentation "Set the object as the currently selected one, or deselect it if already selected."))
