(in-package :lem-core)



(defmacro define-list-type (name element-type &key (predicate-ftype '(function (cons) (or nil t))) export)
  "Define a predicate NAME-P and a type NAME for non-empty lists of ELEMENT-TYPE.
PREDICATE-FTYPE specifies the `ftype` declaration for the generated predicate.
When EXPORT is true, both the predicate and the type are exported from the
current package."
  (declare (type symbol name element-type)
           (type list predicate-ftype)
           (type (or nil t) export))
  (let* ((pred (intern (format nil "~A-p" (symbol-name name)) (symbol-package name)))
         (pred-doc (format nil "Return T if every element of LST is a ~A. Returns NIL otherwise." element-type))
         (type-doc (format nil "A non-empty list whose elements are of type ~A." element-type)))
    (declare (type symbol pred)
             (type string pred-doc type-doc))
    `(progn
       (declaim (ftype ,predicate-ftype ,pred))
       (defun ,pred (lst)
         ,pred-doc
         (declare (type (or nil t list) lst)
                  (optimize (speed 3) (safety 2)))
         (and (consp lst)
              (listp lst)
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
