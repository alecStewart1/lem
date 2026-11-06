;;; scratch-frame.lisp --- REPL-driven frame bench for the SDL3 frontend.
;;;
;;; Purpose: let you create a frame, draw into it, capture it to disk, and
;;; inspect the results — all from the REPL, without lem's view machinery.
;;; This is the §9 workflow from SDL2-MIGRATION-REFERENCE.md made concrete.
;;;
;;; Headless use (no window pops up):
;;;   (setf (uiop:getenv "SDL_VIDEODRIVER") "dummy")
;;;   (lem-sdl3/scratch-frame:open-scratch-frame :headless t)
;;;   (lem-sdl3/scratch-frame:draw-demo-frame)
;;;   (lem-sdl3/scratch-frame:capture-frame "/tmp/frame.bmp")
;;;
;;; With WSLg, omit :headless and the window appears live; every REPL call
;;; redraws it immediately.

(defpackage :lem-sdl3/scratch-frame
  (:use :cl)
  (:export :open-scratch-frame
           :close-scratch-frame
           :with-scratch-frame
           :clear-frame
           :fill-rect
           :draw-text
           :draw-demo-frame
           :present
           :capture-frame
           :scratch-window
           :scratch-renderer
           :scratch-font
           :scratch-width
           :scratch-height))
(in-package :lem-sdl3/scratch-frame)

(defstruct scratch
  window
  renderer
  font
  width
  height)

(defvar *scratch* nil
  "The live scratch frame. All draw/capture functions operate on this.")

(defun check (ok context)
  "SDL3 calls return :bool and fail silently — always check."
  (unless ok
    (error "SDL3 call failed (~A): ~A" context (sdl3:get-error))))

(defun open-scratch-frame (&key (width 640) (height 400)
                             headless
                             (font-file #p"/home/alecStewart/Projects/Lisp/lem/frontends/sdl2/resources/fonts/NotoSansMono-Regular.ttf")
                             (font-size 24.0))
  "Create a window + software renderer + one TTF font. Pass :headless t
when SDL_VIDEODRIVER=dummy so no window is created."
  (when *scratch*
    (close-scratch-frame))
  (check (sdl3:init '(:video)) "SDL_Init")
  (check (sdl3-ttf:init) "TTF_Init")
  ;; Under SDL_VIDEODRIVER=dummy the software renderer is the only backend,
  ;; which also guarantees render-read-pixels works headless.
  (multiple-value-bind (ok window renderer)
      (sdl3:create-window-and-renderer
       "lem sdl3 scratch" width height
       (if headless 0 '(:hidden)))
    (check ok "CreateWindowAndRenderer")
    (let ((font (sdl3-ttf:open-font (namestring font-file) (coerce font-size 'single-float))))
      (when (cffi:null-pointer-p font)
        (error "TTF_OpenFont failed for ~A: ~A" font-file (sdl3:get-error)))
      (setf *scratch* (make-scratch :window window
                                    :renderer renderer
                                    :font font
                                    :width width
                                    :height height)))))

(defun close-scratch-frame ()
  (when *scratch*
    (sdl3-ttf:close-font (scratch-font *scratch*))
    (sdl3:destroy-renderer (scratch-renderer *scratch*))
    (sdl3:destroy-window (scratch-window *scratch*))
    (sdl3-ttf:quit)
    (sdl3:quit)
    (setf *scratch* nil))
  (values))

(defmacro with-scratch-frame ((&key width height headless) &body body)
  `(unwind-protect
       (progn (open-scratch-frame :width ,width :height ,height :headless ,headless)
              ,@body)
     (close-scratch-frame)))

(defun ensure-scratch ()
  (unless *scratch*
    (error "No scratch frame open — call OPEN-SCRATCH-FRAME first.")))

(defun clear-frame (&optional (r 0) (g 0) (b 0))
  (ensure-scratch)
  (check (sdl3:set-render-draw-color (scratch-renderer *scratch*) r g b 255)
         "SetRenderDrawColor")
  (check (sdl3:render-clear (scratch-renderer *scratch*)) "RenderClear"))

(defun fill-rect (x y w h &optional (r 255) (g 255) (b 255))
  (ensure-scratch)
  (check (sdl3:set-render-draw-color (scratch-renderer *scratch*) r g b 255)
         "SetRenderDrawColor")
  (check (sdl3:render-fill-rect (scratch-renderer *scratch*)
                                (make-instance 'sdl3:frect :%x (coerce x 'single-float)
                                                           :%y (coerce y 'single-float)
                                                           :%w (coerce w 'single-float)
                                                           :%h (coerce h 'single-float)))
         "RenderFillRect"))

(defun draw-text (string x y &optional (r 255) (g 255) (b 255))
  "Rasterize STRING with the scratch font, upload as a texture, blit at X Y.
This is the full sdl2-frontend text path (render → texture → render-copy)
reproduced so its cost can be measured in isolation."
  (ensure-scratch)
  (let ((surface (sdl3-ttf:render-text-blended (scratch-font *scratch*) string 0
                                               (make-instance 'sdl3:color
                                                              :%r r :%g g :%b b :%a 255))))
    (when (cffi:null-pointer-p surface)
      (error "TTF_RenderText_Blended failed: ~A" (sdl3:get-error)))
    (unwind-protect
         (let ((texture (sdl3:create-texture-from-surface (scratch-renderer *scratch*) surface)))
           (when (cffi:null-pointer-p texture)
             (error "SDL_CreateTextureFromSurface failed: ~A" (sdl3:get-error)))
           (unwind-protect
                (multiple-value-bind (w h) (surface-size surface)
                  (check (sdl3:render-texture (scratch-renderer *scratch*) texture
                                              nil
                                              (make-instance 'sdl3:frect
                                                             :%x (coerce x 'single-float)
                                                             :%y (coerce y 'single-float)
                                                             :%w (coerce w 'single-float)
                                                             :%h (coerce h 'single-float)))
                         "RenderTexture"))
             (sdl3:destroy-texture texture)))
      (sdl3:destroy-surface surface))))

(defun surface-size (surface)
  (values (cffi:foreign-slot-value surface '(:struct sdl3:surface) 'sdl3:%w)
          (cffi:foreign-slot-value surface '(:struct sdl3:surface) 'sdl3:%h)))

(defun draw-demo-frame ()
  "One complete frame: background, a filled rect, and text — then present."
  (clear-frame 24 24 28)
  (fill-rect 40 320 560 40 60 120 220)
  (draw-text "lem sdl3 scratch frame" 40 40)
  (draw-text "hello, REPL-driven rendering" 40 90 200 200 90)
  (present))

(defun present ()
  (ensure-scratch)
  (check (sdl3:render-present (scratch-renderer *scratch*)) "RenderPresent"))

(defun capture-frame (&optional (pathname "/tmp/lem-sdl3-frame.bmp"))
  "Read the current back buffer into a surface and save it as a BMP.
This is the ground-truth view of what was actually rendered."
  (ensure-scratch)
  (let ((surface (sdl3:render-read-pixels (scratch-renderer *scratch*) nil)))
    (when (cffi:null-pointer-p surface)
      (error "SDL_RenderReadPixels failed: ~A" (sdl3:get-error)))
    (unwind-protect
         (progn
           (check (sdl3:save-bmp surface (namestring (merge-pathnames pathname)))
                  "SaveBMP")
           (values pathname (surface-size surface)))
      (sdl3:destroy-surface surface))))

;;; The defstruct accessors (scratch-window, scratch-renderer, scratch-font,
;;; scratch-width, scratch-height — all exported above) let the REPL poke at
;;; the live objects directly.
