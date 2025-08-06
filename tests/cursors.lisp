(defpackage :lem-tests/cursors
  (:use :cl
        :rove
        :lem-tests/utilities)
  (:import-from :lem/common/killring
                :make-killring
                :push-killring-item
                :peek-killring-item)
  (:export :all-positions
           :positions-set-equal))
(in-package :lem-tests/cursors)

(defun make-testing-killring ()
  (let ((killring (lem/common/killring:make-killring 10)))
    (push-killring-item killring "aaa")
    (push-killring-item killring "bbb")
    (push-killring-item killring "ccc")
    killring))

(deftest simple-fake-cursor-test
  (with-testing-buffer (buffer (lem:make-buffer "cursor test"))
    (let* ((lem-core::*killring* (make-testing-killring))
           (cursor (lem-core::make-fake-cursor (lem:current-point))))
      (testing "Test the fake-cursor created"
        (testing "buffer-fake-cursors"
          (ok (alexandria:length= 1 (lem-core::buffer-fake-cursors buffer)))
          (ok (eq cursor (first (lem-core::buffer-fake-cursors buffer)))))
        (testing "killring"
          (let ((killring (lem-core::fake-cursor-killring cursor)))
            (ok (not (eq (lem-core::current-killring) killring)))
            (ok (equal "aaa" (peek-killring-item killring 2)))
            (ok (equal "bbb" (peek-killring-item killring 1)))
            (ok (equal "ccc" (peek-killring-item killring 0)))))
        (testing "point-kind"
          (ok (eq :left-inserting (lem:point-kind cursor)))))
      (testing "Delete cursor"
        (lem-core::delete-fake-cursor cursor)
        (ok (null (lem-core::buffer-fake-cursors buffer)))))))

(defun make-testing-fake-cursors (point n)
  (lem:with-point ((p point))
    (loop :repeat n
          :do (assert (not (null (lem:line-offset p 1))))
              (lem:make-fake-cursor p))))

(deftest test-to-execute-a-series-of-commands
  (lem-fake-interface:with-fake-interface ()
    (let ((lem-core::*killring* (lem/common/killring:make-killring 10)))
      (with-testing-buffer (buffer (make-text-buffer (lines "abcdefg" "hijklmn" "opqrstu")))
        (make-testing-fake-cursors (lem:buffer-point buffer) 2)
        (testing "execute self-insert command"
          (lem:execute-key-sequence (list (lem:make-key :sym " ")))
          (ok (string= (lines " abcdefg" " hijklmn" " opqrstu")
                       (lem:buffer-text buffer))))
        (testing "execute delete-previous-character command"
          (lem:execute-key-sequence (list (lem:make-key :ctrl t :sym "h")))
          (lem:buffer-text buffer)
          (ok (string= (lines "abcdefg" "hijklmn" "opqrstu")
                       (lem:buffer-text buffer))))
        (testing "multiple cursor killring"
          (lem:execute (lem:buffer-major-mode buffer)
                       (make-instance 'lem:delete-next-char)
                       4)
          (ok (equal '("abcd"
                       "opqr"
                       "hijk")
                     (mapcar (lambda (killring)
                               (peek-killring-item killring 0))
                             (cons lem-core::*killring*
                                   (mapcar (lambda (cursor)
                                             (lem-core::fake-cursor-killring cursor))
                                           (lem-core::buffer-fake-cursors buffer)))))))))))

(defun linum-and-column (point)
  (list (lem:line-number-at-point point)
        (lem:point-column point)))

(defun all-positions (buffer)
  (mapcar #'linum-and-column (lem-core::buffer-cursors buffer)))

(defun positions-set-equal (positions1 positions2)
  (alexandria:set-equal positions1 positions2 :test #'equal))

(deftest next-line/previous-line
  (lem-fake-interface:with-fake-interface ()
    (lem-core::save-continue-flags
      (with-testing-buffer (buffer (make-text-buffer (lines "abcdefghijklmn"
                                                            "opqrstuvwxyz"
                                                            "0123456789"
                                                            "------------------")))
        (lem-core::set-window-buffer buffer (lem:current-window))
        (make-testing-fake-cursors (lem:buffer-point buffer) 2)
        (lem:execute (lem:buffer-major-mode buffer)
                     (make-instance 'lem:move-to-end-of-line)
                     nil)

        (lem:execute (lem:buffer-major-mode buffer)
                     (make-instance 'lem:next-line)
                     nil)
        (ok (positions-set-equal '((2 12) (3 10) (4 10))
                                 (all-positions buffer)))

        (lem:execute (lem:buffer-major-mode buffer)
                     (make-instance 'lem:previous-line)
                     nil)
        (ok (positions-set-equal '((1 14) (2 12) (3 10))
                                 (all-positions buffer)))

        (lem:execute (lem:buffer-major-mode buffer)
                     (make-instance 'lem:next-line)
                     nil)
         (ok (positions-set-equal '((2 12) (3 10) (4 10))
                                  (all-positions buffer)))))))

(deftest move-to-next/previous-fake-cursor-horizontal
  (lem-fake-interface:with-fake-interface ()
    (with-testing-buffer (buffer (make-text-buffer (lines "abcdef")))
      (let ((p (lem:buffer-point buffer)))
        (lem:character-offset p 2)
        (lem:with-point ((q p))
          (lem:character-offset q -1)
          (lem:make-fake-cursor q))
        (lem:with-point ((q p))
          (lem:character-offset q 2)
          (lem:make-fake-cursor q)))
      (lem:execute (lem:buffer-major-mode buffer)
                   (make-instance 'lem-core/commands/multiple-cursors:move-to-next-fake-cursor)
                   nil)
      (ok (equal '(1 4) (linum-and-column (lem:buffer-point buffer))))
      (ok (positions-set-equal '((1 1) (1 2) (1 4))
                               (all-positions buffer)))
      (lem:execute (lem:buffer-major-mode buffer)
                   (make-instance 'lem-core/commands/multiple-cursors:move-to-previous-fake-cursor)
                   nil)
      (ok (equal '(1 2) (linum-and-column (lem:buffer-point buffer))))
      (ok (positions-set-equal '((1 1) (1 2) (1 4))
                               (all-positions buffer))))))

(deftest move-to-next/previous-fake-cursor-vertical
  (lem-fake-interface:with-fake-interface ()
    (with-testing-buffer (buffer (make-text-buffer (lines "aaa" "bbb" "ccc" "ddd")))
      (let ((p (lem:buffer-point buffer)))
        (lem:line-offset p 1)
        (lem:with-point ((q p))
          (lem:line-offset q -1)
          (lem:make-fake-cursor q))
        (lem:with-point ((q p))
          (lem:line-offset q 1)
          (lem:make-fake-cursor q)))
      (lem:execute (lem:buffer-major-mode buffer)
                   (make-instance 'lem-core/commands/multiple-cursors:move-to-next-fake-cursor)
                   nil)
      (ok (equal '(3 0) (linum-and-column (lem:buffer-point buffer))))
      (ok (positions-set-equal '((1 0) (2 0) (3 0))
                               (all-positions buffer)))
      (lem:execute (lem:buffer-major-mode buffer)
                   (make-instance 'lem-core/commands/multiple-cursors:move-to-previous-fake-cursor)
                   nil)
      (ok (equal '(2 0) (linum-and-column (lem:buffer-point buffer))))
      (ok (positions-set-equal '((1 0) (2 0) (3 0))
                               (all-positions buffer))))))

(deftest move-to-next-fake-cursor-with-argument
  (lem-fake-interface:with-fake-interface ()
    (with-testing-buffer (buffer (make-text-buffer (lines "abcdef")))
      (let ((p (lem:buffer-point buffer)))
        (lem:with-point ((q p))
          (lem:character-offset q 1)
          (lem:make-fake-cursor q))
        (lem:with-point ((q p))
          (lem:character-offset q 2)
          (lem:make-fake-cursor q))
        (lem:with-point ((q p))
          (lem:character-offset q 3)
          (lem:make-fake-cursor q)))
      (lem:execute (lem:buffer-major-mode buffer)
                   (make-instance 'lem-core/commands/multiple-cursors:move-to-next-fake-cursor)
                   2)
      (ok (equal '(1 2) (linum-and-column (lem:buffer-point buffer))))
      (ok (positions-set-equal '((1 0) (1 1) (1 2) (1 3))
                               (all-positions buffer))))))
