;;;; package.lisp — Package definition for jsmn
;;;; Author: misakawasaki
;;;; License: MIT

(in-package :cl-user)

(defpackage :jsmn
  (:use :cl)
  (:export
   ;; Main Entry Point
   #:parse
   #:reset-parser

   ;; Parser Structure
   #:parser
   #:make-parser
   #:parser-p
   #:parser-pos
   #:parser-toknext
   #:parser-toksuper

   ;; Token Structure
   #:token
   #:make-token
   #:token-p
   #:token-type
   #:token-start
   #:token-end
   #:token-size

   ;; Conditions (Errors)
   #:jsmn-error
   #:invalid-json
   #:not-enough-tokens
   #:incomplete-json))
