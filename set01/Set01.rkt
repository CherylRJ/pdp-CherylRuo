;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Set01_functions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/image)
;Exercise 13
;distance:two real numbers -> a non-negative integer
;GIVEN:a real, a real
;RETURNS: a non-negative integer
;sqrt(x^2 + y^2))
(define (distance x y)
  (sqrt (+ (* x x) (* y y))))
;Exercise 15
;string-first: a string-> a 1String
;GIVEN: a string
;RETURNS: a 1String
;Example: "Hello" -> "H"
(define (string-first str)
  (string-ith str 0))
;Exercise 18
;image-area: compute the area of an image
;GIVEN: a image
;RETURNS: the area of the image
;image-area = image-height * image-width
;Example: image-height=8, image-width=3, image-area=8 * 3 = 24
(define (image-area img)
  (* (image-height img) (image-width img)))
;Exercise 21
;string-insert:insert a "_" to position i of a string
;GIVEN: a position index, a string, and 1String to insert
;RETURNS: a string
;Example: i=5, 1String is "_", string is "HelloWorld" -> "Hello_World"
(define (string-insert str i)
  (string-append (string-append (substring str 0 i) "_") (substring str i)))                 
;Exercise 22
;string-delete:remove the 1String in position i of a string
;GIVEN:a string and a position i
;RETURNS:a string
;Example:string is "read", i = 2 -> "red"
(define (string-delete str i)
  (string-append (substring str 0 i) (substring str (+ i 1))))
