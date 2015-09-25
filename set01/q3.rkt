;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname image-area) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(require 2htdp/image)

(provide distance-to-origin)
(provide string-first)
(provide image-area)
(provide string-insert)
(provide string-delete)
;Exercise 18
;image-area: compute the area of an image
;GIVEN: a image
;RETURNS: the area of the image
;image-area = image-height * image-width
;Example: image-height=8, image-width=3, image-area=8 * 3 = 24
(define (image-area img)
  (* (image-height img) (image-width img)))