(defpackage :lem-core/commands/type-utils
  (:use :cl :lem-core)
  (:export #:cursor-list-p
           #:point-list-p
           #:buffer-list-p
           #:cursor-list
           #:point-list
           #:buffer-list)
  #+sbcl
  (:lock t))
(in-package :lem-core/commands/type-utils)

;;; Predicates
;;;

(declaim (ftype (function (cons) boolean) cursor-list-p))
(declaim (ftype (function (cons) boolean) point-list-p))
(declaim (ftype (function (cons) boolean) buffer-list-p))

(defun cursor-list-p (lst)
  "Return T if every element of LST is a cursor.
Returns NIL otherwise."
  (declare (optimize (speed 3) (safety 3)))
  (and (consp lst)
       (every (lambda (el)
                (typep el 'lem:cursor))
              lst)))

(defun point-list-p (lst)
  "Return T if every element of LST is a point.
Returns NIL otherwise."
  (declare (optimize (speed 3) (safety 3)))
  (and (consp lst)
       (every (lambda (el)
                (typep el 'lem/buffer/internal:point))
              lst)))

(defun buffer-list-p (lst)
  "Return T if every element of LST is a buffer.
Return NIL otherwise."
   (declare (optimize (speed 3) (safety 3)))
   (and (consp lst)
       (every (lambda (el)
                (typep el 'lem:buffer))
              lst)))

;;; Types
;;;

(deftype cursor-list ()
  "A non-empty list whose elements are of type LEM:CURSOR."
  '(satisfies cursor-list-p))

(deftype point-list ()
  "A non-empty list whose elements are of type LEM/BUFFER/INTERNAL:POINT."
  '(satisfies point-list-p))

(deftype buffer-list ()
  "A non-empty list whose elements are of type LEM:BUFFER."
  '(satisfies buffer-list-p))
