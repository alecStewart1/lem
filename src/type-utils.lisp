(in-package :lem-core)



(defmacro define-list-type (name element-type &key (predicate-ftype '(function (list) boolean)) export)
  "Define a predicate NAME-P and a type NAME for non-empty lists of ELEMENT-TYPE.
The predicate returns NIL for non-list arguments rather than signaling a type
error.  PREDICATE-FTYPE specifies the `ftype` declaration for the generated
predicate.  When EXPORT is true, both the predicate and the type are exported
from the current package."
  (declare (type symbol name element-type)
           (type list predicate-ftype)
           (type (or nil t) export))
  (let* ((pred (intern (format nil "~A-P" (symbol-name name)) (symbol-package name)))
         (pred-doc (format nil "Return T if LST is a proper non-empty list whose elements are of type ~A. Non-list arguments return NIL." element-type))
         (type-doc (format nil "A non-empty list whose elements are of type ~A." element-type)))
    (declare (type symbol pred)
             (type string pred-doc type-doc))
    `(progn
       (declaim (ftype ,predicate-ftype ,pred))
       (defun ,pred (lst)
         ,pred-doc
         (declare (type (or nil t list) lst)
                  (optimize (speed 3) (safety 2)))
         ;; Non-list arguments yield NIL rather than a type error.
         (and (listp lst)
              (not (endp lst))
              (every #'(lambda (el)
                        (declare (type (or nil t) el)
                                 (optimize (speed 3) (safety 2)))
                        (typep el ',element-type))
                     lst)))
       (deftype ,name ()
         ,type-doc
         '(satisfies ,pred))
       ,(when export
          `(eval-when (:compile-toplevel :load-toplevel :execute)
             (export ',pred)
             (export ',name))))))

(define-list-type cursor-list lem:cursor :export t)
(define-list-type point-list lem/buffer/internal:point :export t)
(define-list-type buffer-list lem:buffer :export t)
