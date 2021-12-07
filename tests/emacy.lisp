(defpackage emacy/tests
  (:use :cl :emacy :fiveam))
(in-package :emacy/tests)

;; NOTE: To run this test file, execute `(asdf:test-system :emacy)' in your Lisp.

(def-suite :emacy)
(in-suite :emacy)

(test test-5am
      (is (equal 1 1))
      (is (equal 2 2))
      (is (not (equal 2 1))))
