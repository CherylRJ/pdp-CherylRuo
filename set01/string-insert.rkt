;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname string-insert) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(require 2htdp/image)

(provide distance-to-origin)
(provide string-first)
(provide image-area)
(provide string-insert)
(provide string-delete)
;Exercise 21
;string-insert:insert a "_" to position i of a string
;GIVEN: a position index, a string, and 1String to insert
;RETURNS: a string
;Example: i=5, 1String is "_", string is "HelloWorld" -> "Hello_World"
(define (string-insert str i)
  (string-append (string-append (substring str 0 i) "_") (substring str i)))