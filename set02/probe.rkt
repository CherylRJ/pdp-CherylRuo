;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname pluto) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require rackunit/text-ui)
(require "extras.rkt")

(provide
 initial-probe
 probe-left
 probe-right
 probe-forward
 probe-north?
 probe-south?
 probe-east?
 probe-west?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constant definition ;;;;;;;;;;;;;;;;;;;

(define rmx1 0)
(define rmx2 200)
(define rmy1 0)
(define rmy2 400)
(define pltsize 15)
;; constant data
;; Interpretation:
;; rmx1: the x-coordinates where the room range from
;; rmx2: the x-coordinates where the room range to
;; rmy1: the y-coordinates where the room range from
;; rmy2: the y-coordinates where the room range to
;; pluto-size: the radius of the circle pluto


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; data definition ;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct pluto (angle x y))
;; A pluto is a (make-pluto Angle Real Real)
;; (where Angle is an Int, and (remainder Angle 90) => 0)
;; It represents a pluto on the screen
;; Iterpretation:
;;   angle: the pluto's current direction
;;   x = the x-coordinate on the screen (in pixels from the left)
;;   y = the y-coordinate on the screen (in pixels from the top)
;; Template:
;; pluto-fn: Pluto -> ??
;; (define (plt ??)
;;    (...
;;     (pluto-angle plt)
;;     (pluto-x plt)
;;     (pluto-y plt)
;;     ...)
;; Examples:
;; (define pluto1 (make-robot 90 100 16))
;; (define pluto2 (make-robot 90 14 16))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; required functions definition ;;;;;;;;;;;;;;;;;;;;;;
;; initial-probe: Real Real -> Pluto
;; GIVEN: a set of (x,y) coordinates
;; RETURNS: a pluto with its center at those coordinates, facing north
;; Example:
;; (initial 50 50) => (make-pluto 90 50 50)
;; STRATEGY: function composition
(define (initial-probe x y)
  (make-pluto 90 x y))

;; probe-turn-left : Probe -> Probe
;; probe-turn-right : Probe -> Probe
;; GIVEN: a pluto
;; RETURNS: a pluto like the original, except turned 90 degrees either left or right
;; Example:
;; (probe-turn-left (make-pluto 90 50 50)) => (make-pluto 180 50 50)
;; (probe-turn-left (make-pluto 270 50 50)) => (make-pluto 360 50 50)
;; (probe-turn-right (make-pluto 0 50 50)) => (make-pluto -90 50 50)
;; (probe-turn-right (make-pluto 450 50 50)) => (make-pluto 360 50 50)
;; STRATEGY: structural decomposition
(define (probe-turn-left r)
  (make-pluto (+ (pluto-angle r) 90) (pluto-x r) (pluto-y r)))

(define (probe-turn-right r)
  (make-pluto (- (pluto-angle r) 90) (pluto-x r) (pluto-y r)))

;; probe-forward : Pluto PosReal -> Pluto
;; GIVEN: a pluto and a distance
;; RETURNS: a pluto like the given one, except moved forward by
;; the specified number of pixels. If moving forward the specified number of
;; pixels would cause the pluto to move from being
;; entirely inside the canvas to being even partially outside the canvas,
;; then the pluto should stop at the wall
;; Examples:
;; (forward (make-pluto 90 100 16) 100) => (make-pluto 90 100 15)
;; (forward (make-pluto 0 184 200) 100) => (make-pluto 0 185 200)
;; (forward (make-pluto 180 14 200) 100) => (make-pluto 180 -86 200)
;; (forward (make-pluto 270 100 384) 100) => (make-pluto 270 100 385)
;; STRATEGY: cases on events
(define (probe-forward r d)
  (if (and(in-bound? r)(not (outbound? r))(hit-wall? r d))
      (stop-at-wall r)
      (unblock-move r d)))

;; probe-north? : Pluto -> Boolean
;; probe-south? : Pluto -> Boolean
;; probe-east? : Pluto -> Boolean
;; probe-west? : Pluto -> Boolean
;; GIVEN: a Pluto
;; RETRUNS: true iff the robot is facing in the specified definition
;; STRATEGY: function composition
(define (probe-east? pluto)
  (= (modulo (probe-angle pluto) 360) 0))

(define (probe-north? robot)
  (= (modulo (probe-angle pluto) 360) 90))

(define (probe-west? robot)
  (= (modulo (probe-angle pluto) 360) 180))

(define (probe-south? robot)
  (= (modulo (probe-angle pluto) 360) 270))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; help functions definition;;;;;;;;;;;;;;
;; in-bound?: Pluto -> Boolean
;; GIVEN: a Pluto
;; RETURNS: true iff the pluto's coordinate which is verticle to its moving
;; direction is within the room's range
;; Examples: 
;; STRATEGY: structural decomposition
(define (in-bound? r)
  (cond 
    [(or(probe-east? r)(probe-west? r))
     (and(>= (probe-y r) (+ rmy1 pltsize))
         (<= (probe-y r) (- rmy2 pltsize)))]
    [else (and(>= (probe-x r) (+ rmx1 pltsize))
                  (<= (probe-x r) (- rmx2 pltsize)))]))

;; stop-at-wall: Pluto -> Pluto
;; GIVEN: a Pluto
;; RETURNS: a Pluto that stops at the wall on its moving direction
;; Example:
;; STRATEGY: structural decomposition
(define (stop-at-wall r)
  (cond
    [(Pluto-east? r) (make-pluto 0 (- rmx2 pltsize)(pluto-y r))]
    [(Pluto-north? r) (make-pluto 90 (pluto-x r)(+ rmy1 pltsize))]
    [(Pluto-west? r) (make-pluto 180 (+ rmx1 pltsize)(pluto-y r))]
    [(Pluto-south? r) (make-pluto 270 (pluto-x r)(- rmy2 pltsize))]))

;; hit-wall?: Pluto PosReal -> Boolean
;; GIVEN: a Pluto and the distance it wants to move
;; RETURNS: true iff the pluto will hit and stop at the wall
;; on its moving direction
;; Example:
;; STRATEGY: structural decomposition
(define (hit-wall? r d)
  (cond
    [(pluto-east? r) (> (+(pluto-x r) d)(- rmx2 pltsize))]
    [(pluto-north? r) (< (- (pluto-y r) d)(+ rmy1 pltsize))]
    [(pluto-west? r) (< (- (pluto-x r) d)(+ rmx1 pltsize))]
    [(pluto-south? r) (> (+(pluto-y r) d)(- rmy2 pltsize))]))

;; outbound?: Pluto -> Boolean
;; GIVEN: a Pluto
;; RETUNS: true iff the given pluto is located futher than the room's 
;; upper or lowest bound so that it can move forward without hiting the wall
;; Examples:
;; STRATEGY: structural decomposition
(define (outbound? r)
  (cond
    [(pluto-east? r)(> (pluto-x r)(- rmx2 pltsize))]
    [(pluto-north? r)(< (pluto-y r)(+ rmy1 pltsize))]
    [(pluto-west? r)(< (pluto-x r)(+ rmx1 pltsize))]
    [(pluto-south? r)(> (pluto-y r)(- rmy2 pltsize))]))

;; unblock-move: Pluto PosReal -> Pluto
;; GIVEN: a Pluto and the distance it will travel through
;; RETUNS: a Pluto on the location after it moved
;; STRATEGY: structural decomposition
(define (unblock-move r d)
  (cond
    [(probe-east? r)(make-pluto 0 (+ (pluto-x r) d)(pluto-y r))]
    [(probe-north? r) (make-pluto 90 (pluto-x r)(- (pluto-y r) d))]
    [(probe-west? r) (make-pluto 180 (- (pluto-x r) d)(pluto-y r))]
    [(probe-south? r) (make-pluto 270 (pluto-x r)(+ (pluto-y r) d))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Tests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(begin-for-test
;; check probe-forward function when the pluto faces north
  (check-equal?(probe-forward (make-pluto 90 100 16) 100)(make-pluto 90 100 15)
               "the robot arrives at unexpected place")
  (check-equal?(probe-forward (make-pluto 90 100 14) 100)(make-pluto 90 100 -86)
               "the robot arrives at unexpected place")
  (check-equal?(probe-forward (make-pluto 90 14 16) 100)(make-pluto 90 14 -84)
               "the robot arrives at unexpected place")
  (check-equal?(probe-forward (make-pluto 90 186 16) 100)(make-pluto 90 186 -84)
               "the robot arrives at unexpected place")
  (check-equal?(probe-forward (make-pluto 90 16 16) 100)(make-pluto 90 16 15)
               "the robot arrives at unexpected place")
  (check-equal?(probe-forward (make-pluto 90 184 16) 100)(make-pluto 90 184 15)
               "the robot arrives at unexpected place")
  (check-equal?(probe-forward (make-pluto 90 100 200) 100)(make-pluto 90 100 100)
               "the robot arrives at unexpected place")
;; check probe-forward function when the pluto faces east
  (check-equal?(probe-forward (make-pluto 0 184 200) 100)(make-pluto 0 185 200)
               "the robot arrives at unexpected place")
  (check-equal?(probe-forward (make-pluto 0 186 200) 100)(make-pluto 0 286 200)
               "the robot arrives at unexpected place")
  (check-equal?(probe-forward (make-pluto 0 184 14) 100)(make-pluto 0 284 14)
               "the robot arrives at unexpected place")
  (check-equal?(probe-forward (make-pluto 0 184 386) 100)(make-pluto 0 284 386)
               "the robot arrives at unexpected place")
  (check-equal?(probe-forward (make-pluto 0 184 14) 100)(make-pluto 0 284 14)
               "the robot arrives at unexpected place")
  (check-equal?(probe-forward (make-pluto 0 184 16) 100)(make-pluto 0 185 16)
               "the robot arrives at unexpected place")
  (check-equal?(probe-forward (make-pluto 0 0 50) 100)(make-pluto 0 100 50)
               "the robot arrives at unexpected place")
;; check probe-forward function when the pluto faces west
  (check-equal?(robot-forward (make-pluto 180 14 200) 100)(make-pluto 180 -86 200)
               "the pluto arrives at unexpected place")
  (check-equal?(probe-forward (make-pluto 180 16 200) 100)(make-pluto 180 15 200)
               "the pluto arrives at unexpected place")
  (check-equal?(probe-forward (make-pluto 180 16 16) 100)(make-pluto 180 15 16)
               "the pluto arrives at unexpected place")
  (check-equal?(probe-forward (make-pluto 180 16 14) 100)(make-pluto 180 -84 14)
               "the pluto arrives at unexpected place")
  (check-equal?(probe-forward (make-pluto 180 16 384) 100)(make-pluto 180 15 384)
               "the pluto arrives at unexpected place")
  (check-equal?(probe-forward (make-pluto 180 16 386) 100)(make-pluto 180 -84 386)
               "the pluto arrives at unexpected place")
  (check-equal?(probe-forward (make-pluto 180 200 200) 100)(make-pluto 180 100 200)
               "the pluto arrives at unexpected place")
;; check probe-forward function when the pluto faces south
  (check-equal?(probe-forward (make-pluto 270 100 384) 100)(make-pluto 270 100 385)
               "the pluto arrives at unexpected place")
  (check-equal?(probe-forward (make-pluto 270 100 386 ) 100)(make-pluto 270 100 486)
               "the pluto arrives at unexpected place")
  (check-equal?(probe-forward (make-pluto 270 16 384) 100)(make-pluto 270 16 385)
               "the pluto arrives at unexpected place")
  (check-equal?(probe-forward (make-pluto 270 14 384) 100)(make-pluto 270 14 484)
               "the pluto arrives at unexpected place")
  (check-equal?(probe-forward (make-pluto 270 185 385) 100)(make-pluto 270 185 385)
               "the pluto arrives at unexpected place")
  (check-equal?(probe-forward (make-pluto 270 186 385) 100)(make-pluto 270 186 485)
               "the pluto arrives at unexpected place")
  (check-equal?(probe-forward (make-pluto 270 100 0) 100)(make-pluto 270 100 100)
               "the pluto arrives at unexpected place")
;; check initial-pluto function
  (check-equal?(initial-pluto 50 50) (make-pluto 90 50 50)
               "the pluto arrives at unexpected place")
;; check probe-left function
  (check-equal?(probe-left (make-pluto 270 50 50))(make-pluto 360 50 50)
               "the pluto is turning to a unexpected direction")
  (check-equal?(probe-right (make-pluto 270 50 50))(make-pluto 180 50 50)
               "the pluto is turning to a unexpected direction")
;; others
  (check-equal?(probe-south? (make-pluto 90 50 50)) false 
               "can not define the pluto's direction"))
  
  


