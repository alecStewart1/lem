(defpackage :lem-sdl3/frame
  (:use :cl)
  (:export
   :frame
   :*frames*))
(in-package :lem-sdl3/frame)

;; TODO print-object

;; Captures a given instance of an
;; SDL3 window and renderer.
(defstruct frame
  (name "default" :type string)
  window
  renderer
  font
  (width :type single-float)
  (height :type single-float))

(defun list-of-frames-p (lst)
  "Type predicate function"
  (and (consp lst)
       (every #'frame-p lst)))

(deftype list-of-frames ()
  `(satisfies lisp-of-frames-p))

(declaim (type (or null list-of-frames) *frames*))
(defparameter *frames* nil
  "List of `frame` instances.
Potential for working with many frames.")
