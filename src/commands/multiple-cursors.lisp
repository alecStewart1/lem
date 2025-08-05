(defpackage :lem-core/commands/multiple-cursors
  (:use :cl :lem-core)
  (:export :add-cursors-to-next-line)
  #+sbcl
  (:lock t))
(in-package :lem-core/commands/multiple-cursors)

(define-key *global-keymap* "M-C" 'add-cursors-to-next-line)

(define-command add-cursors-to-next-line () ()
  "Duplicates the cursor under the currently existing cursors."
  (let ((cursors (buffer-cursors (current-buffer))))
    (declare (optimize (speed 3) (safety 2))
             (type (list lem:cursor) cursors))
    (loop :for (cursor next-cursor) :on cursors
          :do (with-point ((p cursor))
                (declare (type lem:cursor cursor next-cursor)
                         (type lem/buffer/internal:point p))
                (let ((line-step 1)
                      (char-step (point-charpos p)))
                  (declare (type fixnum line-step char-step))
                  (when (and (line-offset p line-step char-step)
                             (or (null next-cursor)
                                 (not (same-line-p p next-cursor))))
                    (make-fake-cursor p))))))

(defun clear-duplicate-cursors (buffer)
  (declare (optimize (speed 3) (safety 2))
           (type lem:buffer buffer))
  (loop :for (cursor next-cursor) :on (buffer-cursors buffer)
        :do (declare (type lem:cursor cursor next-cursor))
        :when (and next-cursor (same-line-p cursor next-cursor))
        :do (delete-fake-cursor
             (if (eq cursor (buffer-point buffer))
                 next-cursor
                 cursor))))

(defun garbage-collection-cursors ()
  (clear-duplicate-cursors (current-buffer)))

(add-hook *post-command-hook* 'garbage-collection-cursors)

(defun clear-cursors-when-aborted ()
  (let ((string (merge-cursor-killrings (current-buffer))))
    (clear-cursors (current-buffer))
    (copy-to-clipboard-with-killring string)))

(add-hook *editor-abort-hook* 'clear-cursors-when-aborted)
