(defpackage :lem-core/commands/multiple-cursors
  (:use :cl :lem-core)
  (:export #:add-cursors-to-next-line
           #:add-cursors-to-previous-line
           #:add-cursors-to-right
           #:add-cursors-to-left
           #:clear-cursors)
  #+sbcl
  (:lock t))
(in-package :lem-core/commands/multiple-cursors)

(define-key *global-keymap* "C-M-Down" 'add-cursors-to-next-line)
(define-key *global-keymap* "C-M-Up" 'add-cursors-to-previous-line)
(define-key *global-keymap* "C-M-Right" 'add-cursors-to-right)
(define-key *global-keymap* "C-M-Left" 'add-cursors-to-left)
(define-Key *global-keymap* "C-M-c" 'clear-cursors)

(defun duplicate-cursors (&key line-step char-step move-fn duplicate-p (n 1))
  (declare (type (or null fixnum) line-step char-step)
           (type (or null function) move-fn)
           (type function duplicate-p)
           (type fixnum n)
           (optimize (speed 3) (safety 2)))
  (let ((cursors (buffer-cursors (current-buffer))))
    (declare (type (list lem:cursor) cursors))
    (loop :for (cursor next-cursor) :on cursors
          :do (with-point ((p cursor))
                (declare (type lem:cursor cursor next-cursor)
                         (type lem/buffer/internal:point p))
                (let ((column (point-charpos p)))
                  (declare (type fixnum column))
                  (dotimes (i (or n 1))
                    (declare (ignore i))
                    (let ((moved (cond (move-fn (funcall move-fn p))
                                       (line-step (line-offset p line-step (or char-step column)))
                                       (char-step (character-offset p char-step))
                                       (t nil))))
                      (unless moved (return))
                      (when (or (null next-cursor)
                                (not (funcall duplicate-p p next-cursor)))
                        (make-fake-cursor p))))))))

(define-command add-cursors-to-next-line (n) (:universal)
  "Duplicates the cursor under the currently existing cursors."
  (duplicate-cursors :line-step 1
                     :duplicate-p #'same-line-p
                     :n n))

(define-command add-cursors-to-previous-line (n) (:universal)
  "Duplicates the cursor above the currently existing cursors."
  (duplicate-cursors :line-step -1
                     :duplicate-p #'same-line-p
                     :n n))

(define-command add-cursors-to-right (n) (:universal)
  "Duplicates the cursor to the right of the currently existing cursors."
  (duplicate-cursors :char-step 1
                     :duplicate-p #'point=
                     :n n))

(define-command add-cursors-to-left (n) (:universal)
  "Duplicates the cursor to the left of the currently existing cursors."
  (duplicate-cursors :char-step -1
                     :duplicate-p #'point=
                     :n n))

(defun clear-duplicate-cursors (buffer)
  (declare (type lem:buffer buffer)
           (optimize (speed 3) (safety 2)))
  (loop :for (cursor next-cursor) :on (buffer-cursors buffer)
        :do (declare (type lem:cursor cursor next-cursor))
        :when (and next-cursor (same-line-p cursor next-cursor))
        :do (delete-fake-cursor
             (if (eq cursor (buffer-point buffer))
                 next-cursor
                 cursor))))

(define-command clear-cursors () ()
  "Clear all cursors in the current buffer."
  (let ((buffer (current-buffer)))
    (declare (type lem:buffer buffer))
    (clear-duplicate-cursors buffer)))

(defun garbage-collection-cursors ()
  (clear-duplicate-cursors (current-buffer)))

(add-hook *post-command-hook* 'garbage-collection-cursors)

(defun clear-cursors-when-aborted ()
  (let ((string (merge-cursor-killrings (current-buffer))))
    (clear-cursors (current-buffer))
    (copy-to-clipboard-with-killring string)))

(add-hook *editor-abort-hook* 'clear-cursors-when-aborted)
