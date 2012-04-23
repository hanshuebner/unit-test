;;
;; Copyright (C) 2000-2001 Memetrics Pty. Ltd.
;;           (C) 2004 Manuel Odendahl
;; This code may be used for any purpose but without warrenty or liability
;; of any kind.
;;
;;;;   Implements: A simple unit test harness package.
;;;;   Author: Alain Picard, Memetrics Pty.
;;;;   File: $Id: unit-test.lisp 3773 2008-09-02 15:50:01Z ksprotte $

;;;;   Modified for generic unit testing
;;;;	29 September 2001 by Ken Dickey.
;;;;   Modified for standalone version, test-equal, no more eval of test-forms,
;;;;            added asdf file
;;;;    March 2004 by Manuel Odendahl

;;;;
;;;;   Commentary
;;;;   ----------
;;;;
;;;;   For simple tests, users are expected to use the DEFTEST macro,
;;;;   which creates an instance of a new test which will run its body.
;;;;   When all the DEFTESTs in a file have been seen, the tests can be
;;;;   run by evaluating (run-all-tests).  Tests may also be run individually,
;;;;   if the test object which result from DEFTEST is stored and given as argument 
;;;;   to the RUN-TEST method.
;;;;
;;;;   The user writes forms in the DEFTEST which end up evaluating to
;;;;   test-fails, test-passes (possibly via the handy test-assert,
;;;;   test-equals, test-conditions macros.  e.g.
;;;;
;;;;   (deftest :unit-name "Test name"
;;;;      (let ((foo (make-bar)))
;;;;         (big-long-calc foo)
;;;;         (test-assert (some-condition foo) "Foo failed---wrong phase of moon")
;;;;
;;;;         ... more code here ...
;;;;
;;;;         (test-passes)))
;;;;
;;;;         
;;;;   For more complex tests requiring fancy setting up and tearing down
;;;;   (as well as reclamation of resources in case a test fails), users are expected
;;;;   to create a subclass of the unit-test class using the DEFINE-TEST-CLASS macro.
;;;;   The syntax is meant to be reminiscent of CLOS, e.g
;;;;   
;;;;   (define-test-class my-test-class
;;;;     ((my-slot-1 :initarg :foo ...)
;;;;      (my-slot-2 (any valid CLOS slot options)
;;;;      ....))
;;;;   After this, the methods
;;;;   (defgeneric run-test :before
;;;;               ((test my-test-class) &key (output *debug-io*))   and
;;;;   (defgeneric run-test :after
;;;;               ((test my-test-class) &key (output *debug-io*))   may be
;;;;   specialized to perform the required actions, possibly accessing the
;;;;   my-slot-1's, etc.
;;;;
;;;;   The test form is protected by a handler case.  Care should be taken
;;;;   than any run-test specialization also be protected not to crash.
;;;;
;;;;   You can run tests for a specific test unit via
;;;;	(run-all-tests :unit :unit-name)
;;;;   Of course (run-all-tests) runs all the tests.
;;;;
;;;;   See file "testing-tests.lisp" for sample usage.

;;;;
;;;;   We are busy folks and can't commit to update, or even respond,
;;;;   to enhancements and bug reports but we _are_ interested and may
;;;;   help as we get time:
;;;;
;;;;    Manuel Odendahl: manuel@bl0rg.net
;;;;	Ken Dickey:   kend@memetrics.com, kend0@earthlink.net
;;;;    Alain Picard: Alain.Picard@memetrics.com
;;;;	bugs@memetrics.com
;;;;

(defpackage :unit-test
  (:use :cl)
  (:export deftest
	   define-test-class
	   get-test-by-name
	   run-all-tests
	   run-test

	   test-result
	   test-equal-result
	   test-error-result
	   test-condition-result

	   test-assert
	   test-equal
	   test-condition

	   *unit-test-debug*))

(in-package :unit-test)  

;; conditions

(defvar *unit-tests* nil
  "This contains all unit tests.  Tests are added
simply by creating instances of test objects.")

(defvar *unit-test-debug* nil
  "(Setq *unit-test-debug* t) for debugging")

(defvar *unit-test-results* nil
  "Collects the results of the running tests.")

(defclass test-result ()
  ((form :reader test-result-form
	 :initarg :form
	 :initform nil)
   (status :reader test-result-status
	   :initarg :status
	   :documentation "one of :pass, :fail, :crash")
   (reason :reader test-result-reason
	   :initarg :reason
	   :initform nil)))

(defgeneric print-result (result stream))
(defgeneric print-result-form (result stream))
(defgeneric print-result-status (result status stream))

(defun test-passed-p (test)
  (eq (test-result-status test) :pass))

(defun test-failed-p (test)
  (eq (test-result-status test) :fail))

(defun test-crashed-p (test)
  (eq (test-result-status test) :crash))

(defmethod print-object ((result test-result) stream)
  (print-unreadable-object (result stream :type t)
    (format stream "FORM: ~A STATUS: ~A REASON: ~A"
            (test-result-form result)
            (test-result-status result)
            (test-result-reason result))))

(defclass test-assert-result (test-result)
  ())

(defclass test-equal-result (test-result)
  ((value :reader test-result-value
	  :initarg :value)
   (real-value :reader test-result-real-value
	       :initarg :real-value)))

(defclass test-error-result (test-result)
  ((error :reader test-result-error
	  :initarg :error)))

(defclass test-condition-result (test-result)
  ((result :reader test-result-result
	   :initarg :result)))

(defmethod print-object ((result test-result) stream)
  (format stream "#<TEST-EQUAL-RESULT FORM: ~A STATUS: ~A REASON: ~A>"
	  (test-result-form result)
	  (test-result-status result)
	  (test-result-reason result)))

(defmethod print-result ((result test-result) stream)
  (let ((*print-lines* 1))
    (print-result-form result stream))
  (print-result-status result (test-result-status result) stream)
  (when (test-result-reason result)
    (format stream "~A~%" (test-result-reason result))))

(defmethod print-result-form ((result test-result) stream)
  (format stream "test: ~A" (test-result-form result)))

(defmethod print-result-form ((result test-assert-result) stream)
  (format stream "assert: ~A" (test-result-form result)))

(defmethod print-result-form ((result test-equal-result) stream)
  (format stream "equal: ~A" (test-result-form result)))

(defmethod print-result-form ((result test-error-result) stream)
  (format stream "error: ~A" (test-result-form result)))

(defmethod print-result-form ((result test-condition-result) stream)
  (format stream "condition: ~A" (test-result-form result)))

(defmethod print-result-status ((result test-result) (status (eql :fail)) stream)
  (format stream "~80TFAILS.~%"))

(defmethod print-result-status ((result test-result) (status (eql :pass)) stream)
  (format stream "~80TPASSES.~%"))

(defmethod print-result-status ((result test-result) (status (eql :crash)) stream)
  (format stream "~80TCRASHES.~%"))

(defmethod print-result-status ((result test-error-result) (status (eql :crash)) stream)
  (format stream "~80TCRASHES.~&Error was: ~A~%"
	  (test-result-error result)))

(defmethod print-result-status ((result test-condition-result) (status (eql :fail)) stream)
  (format stream " returned ~A~80TFAILS.~%"
	  (test-result-result result)))

(defmethod print-result-status ((result test-equal-result) (status (eql :fail)) stream)
  (format stream "~80TFAILS.~&        Should have been ~S, was ~S.~%"
	  (test-result-value result)
	  (test-result-real-value result)))

;; test macros

(defun test-fails (&optional (reason 'no-reason-given))
  "Used to signal that the test has failed.  REASON may be
a string or symbol which will be noted in the test report."
  (push (make-instance 'test-result
		       :reason reason
		       :status :fail)
	*unit-test-results*))

(defun test-passes ()
  "Used to signal that the test has passed."
  (push (make-instance 'test-result
		       :status :pass)
	*unit-test-results*))

(defmacro test-assert (form &optional (reason nil))
  (let ((result-sym (gensym "RESULT")))
    `(push (handler-case (let ((,result-sym ,form))
			   (if ,result-sym
			       (make-instance 'test-assert-result
					      :status :pass
					      :reason ,reason
					      :form ',form)
			       (make-instance 'test-assert-result
					      :status :fail
					      :reason ,reason
					      :form ',form)))
	     (error (e)
	       (make-instance 'test-error-result
			      :error e
			      :status :crash
			      :reason ,reason
			      :form ',form)))
      *unit-test-results*)))

(defmacro test-equal (value form &key (test '#'equal))
  (let ((result-sym (gensym "RESULT")))
    `(push (handler-case (let ((,result-sym ,form))
			   (if (funcall ,test ,value ,result-sym)
			       (make-instance 'test-equal-result
					      :status :pass
					      :value ,value
					      :real-value ,result-sym
					      :form ',form)
			       (make-instance 'test-equal-result
					      :status :fail
					      :real-value ,result-sym
					      :value ,value
					      :form ',form)))
	     (error (e)
	       (make-instance 'test-error-result
			      :error e
			      :status :crash
			      :form ',form)))
      *unit-test-results*)))

(defmacro test-condition (form expected-condition)
  (let ((result-sym (gensym "RESULT"))
	(e-sym (gensym "ERROR")))
    `(push (handler-case
	       (let ((,result-sym ,form))
		 (make-instance 'test-condition-result
				:status :fail
				:result ,result-sym
				:form ',form
				:reason (format nil "No exception of type ~A raised"
						,expected-condition)))
	     (error (,e-sym)
	       (if (subtypep (type-of ,e-sym) ,expected-condition)
		   (make-instance 'test-condition-result
				  :status :pass
				  :form ',form)
		   (make-instance 'test-error-result
				  :status :crash
				  :error ,e-sym
				  :form ',form))))
      *unit-test-results*)))

(defclass unit-test ()
  ((unit :reader unit-test-unit
	 :initarg :unit
	 :documentation "Name of unit being tested.
Used to group related tests.")
   (name :reader unit-test-name
	 :initarg :name
	 :initform "Unknown test")
   (body :initarg :body
	 :reader unit-test-body)
   (failed-form :accessor unit-test-failed-form
		:initform "Unknown form"))
  (:documentation "The base class for all unit tests.  When tests are defined,
they will be stored in a global list of tests which can be run at any time with
RUN-ALL-TESTS."))

(defgeneric run-test (test &optional output))

(defmethod print-object ((self unit-test) stream)
  (print-unreadable-object (self stream :type t :identity t)
     (format stream "~s ~(~s~)" (unit-test-name self) (unit-test-unit self))))

(defmethod initialize-instance :after ((self unit-test)
				       &rest rest
				       &key &allow-other-keys)
  (declare (ignore rest))
  ;; If (deftest ...) is called repeatedly, replace
  ;; the test by the same name, if possible.
  (let ((found? (position (unit-test-name self) *unit-tests* 
			  :test #'string= 
			  :key #'unit-test-name)))
    (if found?
	;; We need to replace the test
	 (setf (nth found? *unit-tests*) self)
	 ;; new test.
	 (push self *unit-tests*))))

(defun get-test-by-name (name)
  (find name *unit-tests* :test #'string-equal :key #'unit-test-name))

(defmethod run-test ((test unit-test) &optional (output *debug-io*))
  (handler-case (funcall (unit-test-body test))
    (error (e)
      (format output "Test \"~A\" issued error ~A~%" (unit-test-name test) e)
      (push (make-instance 'test-error-result
			   :form (format nil "(Test \"~A\")" (unit-test-name test))
			   :status :crash
			   :error e) *unit-test-results*))))

(defun run-all-tests (&key (verbose nil) (output *standard-output*) (unit :all))
  ;; Take care to run the tests in the order
  ;; in which they were added.
  (flet ((eq-or-all (element element-unit-set)
	   (or (eq element element-unit-set) (eq element-unit-set :all))))
    (let ((unit-set (if (listp unit)
			unit
			(list unit)))
	  (total-fail 0)
	  (total-pass 0)
	  (total-crash 0))
      
      ;; Run each test and collect result
      (dolist (test (reverse *unit-tests*))
	(when (member (unit-test-unit test) unit-set :test #'eq-or-all)
	  (format output "Running \"~A\" ...~%"
		  (unit-test-name test))
	  (let (*unit-test-results*)
	    (run-test test output)
	    
	    (mapc #'(lambda (result) (print-result result output))
		  (if verbose
		      (reverse *unit-test-results*)
		      (reverse (remove-if #'test-passed-p *unit-test-results*))))
	    
	    (let ((npass (count-if #'test-passed-p *unit-test-results*))
		  (nfail (count-if #'test-failed-p *unit-test-results*))
		  (ncrash (count-if #'test-crashed-p *unit-test-results*)))
	      (format output "=>")
	      (when (> npass 0)
		(format output " ~a passed." npass))
	      (when (> nfail 0)
		(format output " ~a failed." nfail))
	      (when (> ncrash 0)
		(format output " ~a crashed." ncrash))
	      (format output "~%")

	      (incf total-fail nfail)
	      (incf total-pass npass)
	      (incf total-crash ncrash)))))

      ;; Reporting
      (format output   "~&~%====================================")
      (format output   "~&~D tests total." (+ total-fail total-crash total-pass))
      (format output   "~&   ~D passed."   total-pass)
      (when (not (zerop total-fail))
        (format output "~&   ~D failed."   total-fail))
      (when (not (zerop total-crash))
        (format output "~&   ~D crashed."  total-crash))
      (finish-output output)
      (every #'zerop (list total-fail total-crash)))))

(defun delete-all-tests ()
  "Throw away all tests from test suite."
  (setq *unit-tests* nil))

(defun delete-tests-for (unit)
  "Blow away all tests for UNIT from test set."
  (setq *unit-tests* (delete unit *unit-tests* :test #'eq :key #'unit-test-unit)))

;; Creating user-specific test which need special setting up/teardown actions
;; 
(defmacro define-test-class (class-name &optional slots)
  "Define a new test class.  SLOTS are specified as normal CLOS slots."
  `(defclass ,class-name (unit-test)
     ,slots))

(defmacro deftest (unit name &rest body)
  "Create an instance of a unit test called NAME for UNIT."
  `(make-instance 'unit-test
     :unit ,unit
     :name ,name
     :body #'(lambda () ,@body)))
