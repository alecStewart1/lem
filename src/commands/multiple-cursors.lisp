(defpackage :lem-core/commands/multiple-cursors
  (:use :cl :lem-core)
  (:export #:add-cursors-to-next-line
           #:add-cursors-to-previous-line
           #:add-cursors-to-right
           #:add-cursors-to-left
           #:clear-all-cursors
           #:move-to-next-fake-cursor
           #:move-to-previous-fake-cursor)
  #+sbcl
  (:lock t))
(in-package :lem-core/commands/multiple-cursors)

(define-key *global-keymap* "C-M-Down" 'add-cursors-to-next-line)
(define-key *global-keymap* "C-M-Up" 'add-cursors-to-previous-line)
(define-key *global-keymap* "C-M-Right" 'add-cursors-to-right)
(define-key *global-keymap* "C-M-Left" 'add-cursors-to-left)
(define-Key *global-keymap* "C-M-c" 'clear-all-cursors)

(defun duplicate-cursors (&key line-step char-step move-fn (n 1))
  "Create a duplicate (fake) cursor by LINE-STEP (line above or below) or CHAR-STEP (character left or right).
MOVE-FN can be specified where to place a duplicate (fake) cursor if it's not a simple up/down/left/right.
Can be repeated N times."
  (let ((cursors (buffer-cursors (current-buffer))))
    (declare (type (list lem-core:cursor) cursors)
             (optimize (speed 3) (safety 2)))
    (loop :for (cursor next-cursor) :on cursors
          :do (with-point ((p cursor))
                (let ((column (point-charpos p)))
                  (declare (type integer column))
                  (dotimes (i (or n 1))
                    (declare (ignore i))
                    (let ((moved (cond (move-fn (funcall move-fn p))
                                       (line-step (line-offset p line-step (or char-step column)))
                                       (char-step (character-offset p char-step))
                                       (t nil))))
                      (unless moved (return))
                      (when (or (null next-cursor)
                                (not (point= p next-cursor)))
                        (make-fake-cursor p)))))))))

(define-command add-cursors-to-next-line (n) (:universal)
  "Duplicates the cursor under the currently existing cursors."
  (duplicate-cursors :line-step 1
                     :n n))

(define-command add-cursors-to-previous-line (n) (:universal)
  "Duplicates the cursor above the currently existing cursors."
  (duplicate-cursors :line-step -1
                     :n n))

(define-command add-cursors-to-right (n) (:universal)
  "Duplicates the cursor to the right of the currently existing cursors."
  (duplicate-cursors :char-step 1
                     :n n))

(define-command add-cursors-to-left (n) (:universal)
  "Duplicates the cursor to the left of the currently existing cursors."
  (duplicate-cursors :char-step -1
                     :n n))

(defun cycle-real-cursor (step)
  "Move the real cursor to take next or previous fake cursor position by STEP.
  Works both horizontally and vertically, as it is based on buffer positions."
  (let* ((buffer (current-buffer))
         (fake-cursors (buffer-fake-cursors buffer)))
    (declare (type lem-core:buffer buffer)
             (type (or null (list lem-core:cursor)) fake-cursors)
             (optimize (speed 3) (safety 2)))
    (when fake-cursors
      (let* ((real-cursor  (buffer-point buffer))
             (cursors      (buffer-cursors buffer))
             (index        (position real-cursor cursors))
             (target-index (mod (+ index step) (length cursors)))
             (target       (nth target-index cursors)))
        (declare (type lem-core:cursor real-cursor target)
                 (type (list lem-core:cursor) cursors)
                 (type integer index target-index))
        (unless (eq target real-cursor)
          (let ((killring (lem-core::fake-cursor-killring target))
                (mark (fake-cursor-mark target)))
            (make-fake-cursor real-cursor)
            (move-point real-cursor target)
            (setf lem-core::*killring* killring)
            (when (mark-active-p mark)
              (set-cursor-mark real-cursor (copy-point (mark-point mark))))
            (delete-fake-cursor target)))))))

(define-command move-to-next-fake-cursor (n) (:universal-nil)
  "Move the real cursor to the Nth next fake cursor."
  (let ((n (or n 1)))
    (declare (type integer n))
    (cycle-real-cursor n)))

(define-command move-to-previous-fake-cursor (n) (:universal-nil)
  "Move the real cursor to the Nth previous fake cursor."
  (let ((n (or n 1)))
    (declare (type integer n))
    (cycle-real-cursor (- n))))

(defun clear-duplicate-cursors (buffer)
  "Clear all duplicate (fake) cursors in BUFFER."
  (let ((cursors (buffer-cursors buffer)))
    (declare (type (list lem-core:cursor) cursors)
             (optimize (speed 3) (safety 2)))
    (loop :for (cursor next-cursor) :on cursors
          :when (and next-cursor (point= cursor next-cursor))
          :do (delete-fake-cursor
               (if (eq cursor (buffer-point buffer))
                   next-cursor
                   cursor)))))

(define-command clear-all-cursors () ()
  "Clear all cursors in the current buffer."
  (let ((buffer (current-buffer)))
    (declare (type lem-core:buffer buffer))
    (clear-duplicate-cursors buffer)))

(defun garbage-collection-cursors ()
  "Function to run to clear all duplicate (fake) cursors on `*post-command-hook*`."
  (clear-duplicate-cursors (current-buffer)))

(add-hook *post-command-hook* 'garbage-collection-cursors)

(defun clear-cursors-when-aborted ()
  "Function to run to clear all duplicate (fake) cursors on `*editor-abort-hook*`."
  (let ((str (merge-cursor-killrings (current-buffer))))
    (declare (type string str))
    (clear-cursors (current-buffer))
    (copy-to-clipboard-with-killring str)))

(add-hook *editor-abort-hook* 'clear-cursors-when-aborted)
