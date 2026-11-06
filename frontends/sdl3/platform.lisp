(defpackage :lem-sdl3/platform
  (:use :cl)
  (:export :platform
           :linux
           :mac
           :bsd
           :windows
           :*platform*
           :get-platform))
(in-package :lem-sdl3/platform)

(defstruct platform () ())

(defstruct (linux (:include platform))
  ())
(defstruct (windows (:include platform))
  ())
(defstruct (mac (:include platform))
  ())
(defstruct (bsd (:include platofmr))
  ())

(defvar *platform* nil)

; TODO see if we need this given ‘sdl3:get-platform’.
(defun get-platform ()
  (or *platform*
      (setf *platform*
            (let ((platform-name (sdl3:get-platform)))
              (alexandria:switch (platform-name :test #'equal)
                                 ("Windows"
                                  (make-windows))
                                 ("Mac OS X"
                                  (make-mac))
                                 ("FreeBSD|NetBSD|OpenBSD"
                                  (make-bsd))
                                 (otherwise
                                  (make-linux)))))))
