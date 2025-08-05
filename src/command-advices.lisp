(in-package :lem-core)

(defclass movable-advice () ())
(defclass jump-cursor-advice () ())
(defclass editable-advice () ())

;;; multiple cursors
(defun process-each-cursors (fn)
  (let* ((buffer (current-buffer))
         (current (buffer-point buffer))
         (cursors (buffer-cursors buffer)))
    (declare (type function fn)
             (type lem:buffer buffer)
             (type lem/buffer/internal:point current)
             (type (list lem:cursor) cursors)
             (optimize (speed 3) (safety 2)))
    (dolist (point cursors)
      (unless (eq point current)            ; skip the real cursor
        (with-buffer-point (buffer point)
          (with-current-killring (fake-cursor-killring point)
            (handler-case
                (save-continue-flags
                  (funcall fn))
              (move-cursor-error ()))))))
    (funcall fn)))

(defmacro do-each-cursors (() &body body)
  `(process-each-cursors (lambda () ,@body)))

(defmethod execute :around (mode
                            (command movable-advice)
                            argument)
  (process-each-cursors #'call-next-method))

(defmethod execute :around (mode
                            (command editable-advice)
                            argument)
  (process-each-cursors #'call-next-method))

(defmethod execute :around (mode
                            (command jump-cursor-advice)
                            argument)
  (prog1 (call-next-method)
    (clear-cursors (current-buffer))))
