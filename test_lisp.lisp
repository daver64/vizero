;;; Sample Common Lisp file to test syntax highlighting
;;; with REPL-awareness features

(defpackage :test-package
  (:use :common-lisp)
  (:export #:factorial #:fibonacci #:process-list))

(in-package :test-package)

;;; Simple factorial function
(defun factorial (n)
  "Calculate factorial of N."
  (if (<= n 1)
      1
      (* n (factorial (1- n)))))

;;; Fibonacci with memoization
(let ((memo (make-hash-table)))
  (defun fibonacci (n)
    "Calculate nth Fibonacci number with memoization."
    (cond
      ((< n 2) n)
      ((gethash n memo))
      (t (setf (gethash n memo)
               (+ (fibonacci (1- n))
                  (fibonacci (- n 2))))))))

;;; Macro example
(defmacro with-timing ((&key (stream t)) &body body)
  "Execute BODY and print timing information."
  (let ((start-time (gensym "START-TIME"))
        (end-time (gensym "END-TIME")))
    `(let ((,start-time (get-internal-real-time)))
       (multiple-value-prog1
           (progn ,@body)
         (let ((,end-time (get-internal-real-time)))
           (format ,stream "~&Execution time: ~,3F seconds~%"
                   (/ (- ,end-time ,start-time)
                      internal-time-units-per-second)))))))

;;; Class definition using CLOS
(defclass person ()
  ((name :initarg :name :accessor person-name)
   (age :initarg :age :accessor person-age :initform 0)
   (email :initarg :email :reader person-email :initform nil))
  (:documentation "A simple person class."))

(defmethod initialize-instance :after ((p person) &key)
  (when (< (person-age p) 0)
    (error "Age cannot be negative: ~A" (person-age p))))

;;; Generic function with methods
(defgeneric greet (person &optional formal-p)
  (:documentation "Greet a person."))

(defmethod greet ((p person) &optional (formal-p nil))
  (if formal-p
      (format nil "Good day, ~A." (person-name p))
      (format nil "Hi ~A!" (person-name p))))

;;; Working with lists
(defun process-list (lst &key (transform #'identity) (filter (constantly t)))
  "Process a list with optional transform and filter functions."
  (mapcar transform
          (remove-if-not filter lst)))

;;; Example usage with various Lisp features
(defun demo ()
  "Demonstrate various Lisp features."
  (let ((numbers '(1 2 3 4 5 6 7 8 9 10))
        (person (make-instance 'person :name "Alice" :age 30)))
    
    ;; Keywords and symbols
    (format t "~&Processing numbers: ~A~%" numbers)
    
    ;; Lambda expressions and closures
    (let ((multiplier 2))
      (setf numbers (process-list numbers
                                  :transform (lambda (x) (* x multiplier))
                                  :filter #'evenp)))
    
    ;; String manipulation
    (format t "~&Processed numbers: ~A~%" numbers)
    (format t "~&Greeting: ~A~%" (greet person))
    (format t "~&Formal greeting: ~A~%" (greet person t))
    
    ;; Complex numbers and ratios
    (let ((complex-num #C(3 4))
          (ratio 22/7)
          (float-num 3.14159d0))
      (format t "~&Complex: ~A, Ratio: ~A, Float: ~A~%"
              complex-num ratio float-num))
    
    ;; Character literals
    (format t "~&Characters: ~A ~A ~A~%"
            #\Space #\Newline #\Tab)
    
    ;; Package-qualified symbols
    (format t "~&Package symbol: ~A~%" 'common-lisp:car)
    
    ;; Timing example
    (with-timing ()
      (factorial 100)
      (fibonacci 20))
    
    ;; Return something
    :done))

;;; Condition handling example
(define-condition custom-error (error)
  ((message :initarg :message :reader error-message))
  (:report (lambda (condition stream)
             (format stream "Custom error: ~A" (error-message condition)))))

(defun risky-operation (x)
  "An operation that might signal an error."
  (cond
    ((not (numberp x))
     (error 'custom-error :message "Input must be a number"))
    ((zerop x)
     (error "Division by zero"))
    (t (/ 100 x))))

(defun safe-operation (x)
  "Safely perform a risky operation."
  (handler-case
      (risky-operation x)
    (custom-error (c)
      (format t "Caught custom error: ~A~%" c)
      nil)
    (error (c)
      (format t "Caught generic error: ~A~%" c)
      nil)))

;;; Entry point
(defun main ()
  "Main entry point."
  (format t "~&=== Common Lisp Syntax Highlighting Test ===~%")
  (demo)
  (safe-operation 0)
  (safe-operation "not a number")
  (safe-operation 5)
  (format t "~&=== Test Complete ===~%"))

;; Uncomment to run when loaded
;; (main)