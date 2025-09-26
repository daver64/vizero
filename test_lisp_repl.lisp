; Test file for Lisp REPL plugin
; Phase 1: SBCL Integration Testing

; Basic function definition
(defun hello-world ()
  (format t "Hello from Vizero Lisp REPL!~%"))

; Simple arithmetic
(+ 1 2 3 4 5)

; Package test
(defpackage :vizero-test
  (:use :cl)
  (:export #:test-function))

(in-package :vizero-test)

(defun test-function (x y)
  "A simple test function for the REPL"
  (* x y))

; Test list operations
(let ((numbers '(1 2 3 4 5)))
  (mapcar #'1+ numbers))

; Back to CL-USER package
(in-package :cl-user)

; Complex expression with nested calls
(defun factorial (n)
  (if (<= n 1)
      1
      (* n (factorial (1- n)))))

(factorial 5)