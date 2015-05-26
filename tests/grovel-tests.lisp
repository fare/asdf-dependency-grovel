;;; Define the package of the test framework
(cl:require "asdf")
#-asdf3.1 (error "ASDF 3.1 required")

(cl:defpackage :asdf-dependency-grovel-tester
  (:use :cl :asdf :uiop)
  (:export #:test-result #:check-base-deps))

(cl:in-package :asdf-dependency-grovel-tester)


(define-condition failed-component ()
  ((file :accessor failed-file :initarg :file)
   (should :accessor failed-dependency :initarg :should)
   (has :accessor actual-dependency :initarg :has)))

(defun canonicalize-name (name)
  (subseq name (mismatch "u-" name) (position #\. name)))

(defun 1-component (all-comps &key file depends-on)
  (when (eql 2 (mismatch "u-" file))
    (let* ((name (canonicalize-name file))
           (provider-comp (format nil "p-~A" name))
           (dependency (mapcar (lambda (comp)
                                 (getf comp :file))
                               (remove-if-not
                                (lambda (comp
                                         &aux (mismatch
                                               (mismatch provider-comp
                                                         (getf comp :file))))
                                  (or (not mismatch)
                                      (eql (length provider-comp) mismatch)))
                                all-comps)))
           (depends-on (remove "package" depends-on :test #'equal)))
      (unless (equal dependency depends-on)
        (error 'failed-component
               :file file :should dependency
               :has depends-on)))))

(defparameter *adg-dir* (pathname-parent-directory-pathname *load-truename*))

(push *adg-dir* *central-registry*)
(push (subpathname *adg-dir* "tests/") *central-registry*)

;; (setf *break-on-signals* '(or error warning))
(load-system "asdf-dependency-grovel")

(defun test-result ()
  (asdf:operate 'asdf-dependency-grovel:dependency-op :test-serial)
  (let ((comps (asdf-dependency-grovel:read-component-file
                "groveled-components.lisp" :test-serial-system))
        (failed nil))
    (loop for comp in comps
          do (handler-case (apply #'1-component comps comp)
               (failed-component (c)
                 (push (list (failed-file c)
                             (failed-dependency c)
                             (actual-dependency c))
                       failed))))
    (if (null failed)
        (format t "~&;;; ALL TESTS PASSED!~%")
        (format t "~&;;; TESTS failed: ~:{~&;; ~A should have: ~S, has ~S~}~%"
                failed))
    (length failed)))

;(trace asdf:perform asdf-dependency-grovel:signal-user asdf-dependency-grovel:signal-provider)


;; Currently unused (commented out in the test script).
;; (defun check-base-deps ()
;;   (asdf:oos 'asdf-dependency-grovel:compare-dependency-op :test-serial))
