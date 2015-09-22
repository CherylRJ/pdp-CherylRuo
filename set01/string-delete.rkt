;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname string-delete) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(require 2htdp/image)

(provide distance-to-origin)
(provide string-first)
(provide image-area)
(provide string-insert)
(provide string-delete)
;Exercise 22
;string-delete:remove the 1String in position i of a string
;GIVEN:a string and a position i
;RETURNS:a string
;Example:string is "read", i = 2 -> "red"
(define (string-delete str i)
  (string-append (substring str 0 i) (substring str (+ i 1))))