(defpackage :lem-sdl3/font
  (:use :cl)
  ())

(in-package :lem-sdl3/font)

(declaim (type fixnum *default-font-size*))
(defparameter *default-font-size* 15)

(declaim (ftype (function nil fixnum) default-font-size))
(defun default-font-size () *default-font-size*)


(defstruct (font-config (:constructor %make-font-config))
  size
  latin-normal-file
  latin-bold-file
  cjk-normal-file
  cjk-bold-file
  emoji-file
  braille-file)

(defstruct font
  latin-normal-font
  latin-bold-font
  cjk-normal-font
  cjk-bold-font
  emoji-font
  braille-font
  char-width
  char-height
  ;; Latin-normal-font TTF intrinsic metrics, cached at open-font time so the
  ;; glyph-draw path can anchor baselines without round-tripping into FFI on
  ;; every blit.  ASCENT is positive (pixels above baseline), DESCENT is
  ;; negative (pixels below baseline) per SDL_ttf convention.
  ascent
  descent)
