(defpackage :lem-sdl3/utils
  (:use :cl)
  (:export
   :make-point
   :make-fpoint
   :make-rect
   :make-frect
   :create-texture
   :render-texture))
(in-package :lem-sdl3/utils)

(declaim (ftype
          (function
           (fixnum fixnum)
           sdl3:point)
          make-point))
(defun make-point (x y)
  "Convenience wrapper to make an `sdl3:point`."
  (make-instance 'sdl3:point :x% x :y% y))

(declaim (ftype
          (function
           (single-float single-float)
           sdl3:fpoint)
          make-fpoint))
(defun make-fpoint (x y)
  "Convenience wrapper to make an `sdl3:fpoint`."
  (make-instance 'sdl3:fpoint :x% x :y% y))

(declaim (ftype
          (function
           (fixnum fixnum fixnum fixnum)
           sdl3:rect)
          make-rect))
(defun make-rect (x y w h)
  "Convenience wrapper to make an `sdl3:rect`."
  (make-instance 'sdl3:rect :x% x :y% y :w% w :h% h))

(declaim (ftype
          (function
           (single-float single-float single-float single-float)
           sdl3:frect)
          make-frect))
(defun make-frect (x y w h)
  "Convenience wrapper to make an `sdl3:frect`."
  (make-instance 'sdl3:frect :x% x :y% y :w% w :h% h))

(defun cleanup-and-quit (frame font renderer window)
  "Convenience for cleaning up a renderer and window, and
then quitting."
  (when frame
    (sdl3-ttf:close-font (font frame))
    (sdl3:destory-renderer (renderer frame))
    (sdl3:destroy-window (window frame))
    (sdl3-ttf:quit)
    (sdl3:quit)
    (setf *scratch* nil)))
