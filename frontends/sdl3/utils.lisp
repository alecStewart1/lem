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

(defun create-texture (renderer texture x y w h)
  (sdl3:render-texture renderer
                       texture
                       nil
                       (make-frect x y w h)))

(defun render-texture (renderer texture x y w h)
  (sdl3:with-rects ((dest-rect x y width height))
    (sdl3:render-copy-ex renderer
                         texture
                         :source-rect nil
                         :dest-rect dest-rect
                         :flip (list :none))))
