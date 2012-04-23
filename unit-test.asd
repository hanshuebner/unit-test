;;;; -*- Mode: LISP -*-

(in-package :cl-user)

(defpackage :unit-test.system
  (:use :cl :asdf))

(in-package :unit-test.system)

(defsystem :unit-test
  :name "unit-test"
  :description "unit-testing framework for common lisp"

  :components ((:file "unit-test")))