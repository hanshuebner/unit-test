;;;;   -*- Mode: lisp; Package: common-lisp-user; Syntax: Common-lisp -*-
;;
;; Copyright (C) 2000-2001 Memetrics Pty. Ltd.
;; This code may be used for any purpose but without warrenty or liability
;; of any kind.
;;
;;;;   Implements: Mimimalist sanity check of unit test code.
;;;;   Author: Alain Picard, Ken Dickey Memetrics Pty.
;;;;   File: $Id: testing-tests.lisp 4505 2010-01-27 15:17:31Z hans $

(in-package :unit-test)

(deftest :test "Test crash (unexpected error, not a failed test)"
  (let ((a 1))
    (+ a 10)
    (= a 2))
  (string= 1 2))

(deftest :test "Test ignored result"
  (= 1 2))

(deftest :test "Test Assert with crash"
  (test-assert (string= 1 2)))

(deftest :test "Test Assert with fail"
  (test-assert (= 1 2)))

(deftest :test "Test Assert with success"
  (test-assert (= 1 1)))

(deftest :test "ensure-exception-raised with crash"
  (test-condition (string= 1 2) 'simple-error))

(deftest :test "ensure-exception-raised success"
  (test-condition (string= 1 2) 'simple-type-error))

(deftest :test "ensure-exception-raised fails"
  (test-condition (string= "a" "a") 'simple-type-error))

;;;; Subclass test-class for setup/restore

(define-test-class sample-db-test)

(defvar *db-initialized* nil)

(defmethod run-test :before ((test sample-db-test) &optional (output *debug-io*))
   "Add tables, sample db data, whatever"
   (declare (ignore output))
   (if *db-initialized*
     (error "DB already set up for test.  What happened?")
     (setf *db-initialized* t)))

(defmethod run-test :after  ((test sample-db-test) &optional (output *debug-io*))
   "Restore clean slate"
   (declare (ignore output))
   (if *db-initialized*
       (setf *db-initialized* nil)
       (error "DB not set up.  What happened?")))

(defmacro def-dbtest (unit name &rest body)
  "Create an instance of a unit test called NAME for UNIT."
  `(make-instance 'sample-db-test
     :unit ,unit
     :name ,name
     :body #'(lambda () ,@body)))

(deftest :clean-db "DB in vanilla state"
    (test-assert (not *db-initialized*)))

(def-dbtest :db-setup-test "Test Assert on DB setup success"
  (test-assert *db-initialized*))

(deftest :clean-db "DB in vanilla state (again)"
    (test-assert (not *db-initialized*)))

;; (run-all-tests :unit :db-setup-test)
;; (run-all-tests :unit :test)

;; Print usage info upon loading
(format t "~%~%~TType (run-all-tests) to test")
(format t "~%~Tand (delete-all-tests) to clear~%~%")
(finish-output)
