(cl:defpackage :test-asdf-dependency-grovel
  (:use :cl)
  (:export #:test-result))

(cl:in-package :test-asdf-dependency-grovel)

(define-condition failed-component ()
  ((file :accessor failed-file :initarg :file)
   (should :accessor failed-dependency :initarg :should)
   (has :accessor actual-dependency :initarg :has)))

(defun canonicalize-name (name)
  (subseq name (mismatch "u-" name) (position #\. name)))

(defun 1-component (&key file depends-on)
  (when (eql 2 (mismatch "u-" file))
    (let* ((name (canonicalize-name file))
           (dependency (list (format nil "p-~A" name)))
           (depends-on (remove "package" depends-on :test #'equal)))
      (unless (equal dependency depends-on)
        (error 'failed-component
               :file file :should dependency
               :has depends-on)))))

(load "test-serial.asd")
(asdf:oos 'asdf-dependency-grovel:dependency-op :asdf-dependency-grovel-test/generator)

(defun test-result ()
  (with-open-file (f "groveled-components.lisp" :direction :input)
    (let ((comps (read f))
          (failed nil))
      (loop for comp in comps
            do (handler-case (apply #'1-component comp)
                 (failed-component (c)
                   (push (list (failed-file c) (failed-dependency c) (actual-dependency c))
                         failed))))
      (if (null failed)
          (format t "~&;;; ALL TESTS PASSED!~%")
          (format t "~&;;; TESTS failed: ~:{~&;; ~A should have: ~S, has ~S~}~%" failed))
      (length failed))))