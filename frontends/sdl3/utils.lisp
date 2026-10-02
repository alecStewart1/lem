(defpackage :lem-sdl3/utils
  (:use :cl)
  (:export :create-texture
           :render-texture))
(in-package :lem-sdl3/utils)

(defun create-texture (renderer texture width height)
  (sdl3:render-texture renderer
                       texture
                       nil
                       (sdl3:frect :x (coerce x 'single-float)
                                   :y (coerce y 'single-float)
                                   :w (coerce width 'single-float)
                                   :h (coerce height 'single-float))))

(defun render-texture (renderer texture x y width height)
  (sdl3:with-rects ((dest-rect x y width height))
    (sdl3:render-copy-ex renderer
                         texture
                         :source-rect nil
                         :dest-rect dest-rect
                         :flip (list :none))))
