;;;; package.lisp

(uiop:define-package #:zen
  (:use #:cl)
  (:export
   #:eval-always
   #:curry #:curry1
   #:iota
   #:alterf
   #:defplace
   #:defglobal
   #:list-of #:define-list-type
   #:defpattern #:match #:bind #:bind* #:var #:ref #:of #:guard #:match-error
   #:deftest #:test-success #:test-error #:equal-test-error #:collect-test-results #:check #:check-match))

(uiop:define-package #:lispgo
  (:use #:cl #:zen))

(uiop:define-package #:euler
  (:use #:cl #:zen))
