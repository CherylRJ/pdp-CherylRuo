;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(require 2htdp/image)
(provide string-delete)

;; Exercise 22
;; string-delete:remove the 1String in position i of a string
;; GIVEN:a string and a position i
;; RETURNS:a string
;; EXAMPLES:
;; (string-delete "Hello" 0) => "ello"
;; (string-delete "Hello" 1) => "Hllo"
;; (string-delete "Hello" 0) => "Hell"
;; STRATEGY: Functional Composition
(define (string-delete str i)
  (string-append (substring str 0 i) (substring str (+ i 1))))

(begin-for-test
  (check-equal? (string-delete "Hello" 0)
                (string-append (substring "Hello" 0 0) (substring "Hello" (+ 0 1)))
                "First letter deleted"))