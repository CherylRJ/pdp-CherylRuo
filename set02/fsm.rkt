;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname fsm) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require rackunit)
(require "extras.rkt")
(provide initial-state)
         next-state
         accepting-state?
         error-state?)


;; Data Definition

;; A State is one of:
;; -- BEGIN      Interp: BEGIN is the begin (1st) state
;; -- MIDDLE     Interp: MIDDLE is the middle (2nd) state
;; -- FINAL      Interp: FINAL is the final state
;; -- ERROR      Interp: ERROR is all errors go to this state
;; Decomposition Template:
;;(define (state-fn state)
;;   (cond
;;     [(string=? state BEGIN) ...]
;;     [(string=? state MIDDLE) ...]
;;     [(string=? state FINAL) ...]
;;     [(string=? state ERROR) ...]
;;     [else ...]))

;;;;;; constant definition ;;;;;;
(define BEGIN "begin")
(define MIDDLE "middle")
(define FINAL "final")

;; An MachineInput is one of:
;; -- A
;; -- B
;; -- C
;; -- D
;; -- E
;; -- F
;; Decomposition Template:
;;(define (state-fn state)
;;   (cond
;;     [(string=? state BEGIN) ...]
;;     [(string=? state MIDDLE) ...]
;;     [(string=? state FINAL) ...]
;;     [(string=? state ERROR) ...]
;;     [else ...]))

;;;;;;;;;;;;Constants;;;;;;;;;;;;;;;;;;;;;;;;
(define A "a")
(define B "b")
(define C "c")
(define D "d")
(define E "e")
(define F "f")

;;;;;;;;;;;;;;;FUNCTION;;;;;;;;;;;;;;;;;;;;;;;
;; initial-state: Number -> State
;; GIVEN: a number
;; RETURNS: a representation of the initial state
;; of your machine.  The given number is ignored.
(define (initial-state num)
  BEGIN)

;; next-state: State MachineInput -> State
;; GIVEN: a state of the machine and a machine input
;; RETURNS: the state that should follow the given input.
(define (next-state state input)
  (cond
    [(string=? state BEGIN) (begin-state-next input)]
    [(string=? state MIDDLE) (middle-state-next input)]
    [(string=? state FINAL) (final-state-next input)]
    [(string=? state ERROR) (error-state-next input)]
    [else ERROR]))

;; error-state-next: MachineInput -> State
;; GIVEN: a state of the machine and a machine input
;; RETURNS: the state that should follow the given input.
(define (error-state-next input) ERROR)

;; begin-state-next: MachineInput -> MachineInput
;; GIVEN: a machine input
;; RETURNS: the state that should follow the given input.
;; Example: 
(define (begin-state-next input)
  (cond
    [(string=? input A) BEGIN]
    [(string=? input B) BEGIN]
    [(string=? input C) MIDDLE]
    [else ERROR]))
;; middle-state-next: MachineInput -> MachineInput
;; GIVEN: a machine input
;; RETURNS: the state that should follow the given input.
(define (middle-state-next input)
  (cond
    [(string=? input A) MIDDLE]
    [(string=? input B) MIDDLE]
    [(string=? input D) FINAL]
    [else ERROR]))
;; final-state-next: MachineInput -> MachineInput
;; GIVEN: a machine input
;; RETURNS: the state that should follow the given input.
(define (final-state-next input)
  (cond
    [(string=? input E) FINAL]
    [(string=? input F) FINAL]
    [else ERROR]))
     
;; accepting-state? : State -> Boolean
;; GIVEN: a state of the machine
;; RETURNS: true iff the given state is a final (accepting) state
(define (accepting-state? state)
  (string=? state FINAL))


;; error-state? : State -> Boolean
;; GIVEN: a state of the machine
;; RETURNS: true iff there is no path (empty or non-empty) from the given
;; state to an accepting state
(define (error-state? state)
  (string=? state ERROR))