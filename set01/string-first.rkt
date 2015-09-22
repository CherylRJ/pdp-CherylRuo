;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname string-first) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(require 2htdp/image)

(provide distance-to-origin)
(provide string-first)
(provide image-area)
(provide string-insert)
(provide string-delete)
;Exercise 15
;string-first: a string-> a 1String
;GIVEN: a string
;RETURNS: a 1String
;Example: "Hello" -> "H"
(define (string-first str)
  (string-ith str 0))