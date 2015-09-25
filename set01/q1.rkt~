;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname distance-to-origain) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(require 2htdp/image)

(provide distance-to-origin)
(provide string-first)
(provide image-area)
(provide string-insert)
(provide string-delete)
;Exercise 13
;distance:two real numbers -> a non-negative integer
;GIVEN:a real, a real
;RETURNS: a non-negative integer
;sqrt(x^2 + y^2))
(define (distance-to-origin x y)
  (sqrt (+ (* x x) (* y y))))