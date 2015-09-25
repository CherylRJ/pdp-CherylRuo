;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname coffee-machine) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require rackunit)
(require "extras.rkt")
(provide initial-machine
         machine-next-state
         machine-output
         machine-remaining-coffee
         machine-remaining-chocolate
         machine-bank
         
;;;;;;;;;;;;;;;;;;;;;;;;;;; constant data ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define co-price 1.50)
(define ch-price 0.60)
;; Interpretation:
;; --co-price: the price for a coffee
;; --ch-price: the price for a hot chocolate

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Data Definition ;;;;;;;;;;;;;;;;;;;;;;;
(define-struct machine (coffee chocolate bank deposit))
;; A machine is a (make-machine NonNegInt NonNegInt NonNegInt NonNegInt)
;; It represents a machine selling coffee and hot chocolate
;; Interpretation:
;;   coffee: the amount of remaining coffee
;;   hot chocolate: the amount of remaining hot chocolate
;;   banks: the money stored in the machine
;;   deposit: the money deposited by current customer in machine
;; Decomposition Template:
;; machine-fn?: Machine -> ???
;; (define (machine-fn? mac)
;;    (...
;;       (machine-coffee mac)
;;       (machine-hot chocolate mac)
;;       (machine-bank mac)
;;       (machine-deposit)
;;    ...)
;; (define mac1 (make-machine 10 10 200 50))
;; (define mac2 (make-machine 20 20 70 100))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; required functions definition ;;;;;;;;;
;; initial-machine : NonNegInt NonNegInt -> Machine
;; GIVEN: the amount of coffee and the amount of hot chocolate
;; RETURN: a machine loaded with the given amount of coffee and
;; hot chocolate, with an empty bank.
;; Examples:
;; (initial-machine 10 10) => (make-machine 10 10 0 0)
(define (initial-machine co ch)
  (make-machine co ch 0 0))

;; machine-next-state : Machine CustomerInput -> Machine
;; A CustomerInput is one of
;; -- a PosInt     interp: insert the specified number of money
;; -- "coffee"  interp: request a cup of coffee
;; -- "chocolate"    interp: request a cup of hot chocolate
;; -- "change"    interp: return all the coins that the customer has put in
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIVEN: a machine state and a customer input
;; RETURN: the state of the machine that should follow the customer's input
;; Examples:
;; (machine-next-state (make-machine 1 0 0 1.50) "coffee") => (make-machine 0 0 1.50 0)
;; (machine-next-state (make-machine 0 1 0 0.60) "chocolate") => (make-machine 0 0 0.60 0)
;; (machine-next-state (make-machine 1 0 0 0.60) "change") => (make-machine 1 0 0 0)
;; (machine-next-state (make-machine 1 0 0 0.60) 100) => (make-machine 1 0 0 1.50)
;; STRATEGY: cases on events
(define (machine-next-state mac input)
  (cond
    [(number? input) (deposit-in mac input)]
    [(string=? input "coffee")(coffee-process mac)]
    [(string=? input "chocolate") (chocolate-process mac)]
    [(string=? input "change") (release-deposit mac)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; help functions ;;;;;;;;;;;;;;;;;;
;; deposit-in: Machine NonNegInt -> Machine
;; GIVEN: a Machine and the number of input money
;; RETURNS: a Machine whose deposit has money added in
;; Examples:
;; STRATEGY: structural decomposition
(define (deposit-in mac money)
  (make-machine (machine-coffee mac)(machine-chocolate mac)
                (machine-bank mac)(+ (machine-deposit mac) money)))
;; chocolates-process: Machine -> Machine
;; GIVEN: a Machine
;; RETURNS: the given machine after customer's "coffee" request
;; Examples:
;; (coffee-process (make-machine 1 0 0 175)) => (make-machine 0 0 175 0)
;; (coffee-process (make-machien 1 0 0 70)) => (make-machine 1 0 0 70)
;; STRATEGY: cases on events
(define (coffee-process mac)
  (cond
    [(enough-for-co? mac)(coffee-output mac)]
    [else mac]))
                                
;; coffee-output: Machine -> Machine
;; GIVEN: a machine that meet the condition 
;; for customer to get a cup of coffee
;; RETURN: a Machine that after giving a cup of coffee
;; Examples:
;; (coffee-ouput (make-machine 10 10 50 200)) => (make-machine 9 10 225 0)
;; STRATEGY: structural decomposition
(define (coffee-output mac)
  (make-machine (-(machine-coffee mac) 1)
                (machine-chocolate mac)
                (+(machine-bank mac) co-price)
                0))

;; enough-for-co?: Machine -> Boolean
;; GIVEN: a Machine
;; RETURN: whether this machine meet the condition to
;; giving a coffee
;; Examples:
;; (enough-for-coffee? (make-machine 10 10 0 175)) => true
;; (enough-for-coffee? (make-machine 10 10 0 174)) => false
;; (enough-for-coffee? (make-machine 0 10 0 200)) => false
;; STRATEGY: structural decomposition
(define (enough-for-co? mac)
  (and(>= (machine-deposit mac) co-price)
      (> (machine-coffee mac) 0)))

;; hot chocolate-process: Machine -> Machine
;; GIVEN: a Machine
;; RETURNS: the given machine after a customer's "hot chocolate" request
;; Examples:
;; (hot chocolate-process (make-machien 10 10 50 75)) => (make-machien 10 9 120 0)
;; (hot chocolate-process (make-machine 10 10 50 69)) => (make-machine 10 10 50 69)
;; STRATEGY: cases on event
(define (chocolate-process mac)
  (cond
    [(enough-for-ch? mac) (chocolate-output mac)]
    [else mac]))

;; hot chocolate-output: Machine -> Machine
;; GIVEN: a Machine that meets the condition to give a cup of hot chocolate 
;; RETURNS: the given Machine after giving a cup of hot chocolate
;; Examples:
;; (hot chocolate-output (make-machine 10 10 50 80)) => (make-machine 10 9 120 0)
;; STRATEGY: structural decomposition
(define (chocolate-output mac)
  (make-machine (machine-coffee mac)
                (- (machine-chocolate mac) 1)
                (+ (machine-bank mac) ch-price)
                0))

;; enough-for-ch?: Machine -> Boolean
;; GIVEN: a Machine
;; RETURNS: whether the given Machine meets the condition to give a
;; hot chocolate
;; Examples:
;; (enough-for-ch? (make-machine 10 10 70 70)) => true
;; (enough-for-ch? (make-machien 10 10 70 69)) => false
;; (enough-for-ch? (make-machine 10 0 70 200)) => false
;; STRATEGY: structural decomposition
(define (enough-for-ch? mac)
  (and(>= (machine-deposit mac) ch-price)
      (> (machine-chocolate mac) 0)))

;; release-deposit: Machine -> Machine
;; GIVEN: a Machine
;; RETURN: the given Machine after release deposit in it
;; Examples:
;; (release-deposit (make-machine 10 10 50 100)) => (make-machine 10 10 50 0)
;; STRATEGY: strucutral decomposition
(define (release-deposit mac)
  (make-machine (machine-coffee mac)
                (machine-chocolate mac)
                (machine-bank mac)
                0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Tests ;;;;;;;;;;;;;;;;;;;;;;;;;;
(begin-for-test
;; check input money part
  (check-equal? (machine-next-state (make-machine 0 0 0 0) 1.00)(make-machine 0 0 0 1.00)
                "can not input deposit correctly")
  
;; check coffee part
  (check-equal? (machine-next-state (make-machine 1 0 0 0.75) "coffee")(make-machine 0 0 0.75 0)
                "the coffee part does not work")
  (check-equal? (machine-next-state (make-machine 0 0 0 1.00) "coffee")(make-machine 0 0 0 1.00)
                "the coffee part does not work")
  (check-equal? (machine-next-state (make-machine 1 0 0 1.25) "coffee")(make-machine 1 0 0 1.25)
                "the coffee part does not work")
  
;; check hot chocolate part
  (check-equal? (machine-next-state (make-machine 0 1 0 70) "chocolate")(make-machine 0 0 70 0)
                "the hot chocolate part does not work")
  (check-equal? (machine-next-state (make-machine 0 0 0 70) "chocolate")(make-machine 0 0 0 70)
                "the hot chocolate part does not work")
  (check-equal? (machine-next-state (make-machine 0 1 0 69) "chocolate")(make-machine 0 1 0 69)
                "the hot chocolate part does not work")

;; check release part
  (check-equal? (machine-next-state (make-machine 10 10 10 100) "change")(make-machine 10 10 10 0)
                "the release money part does not work")

;; check initial function
  (check-equal? (initial-machine 50 50)(make-machine 50 50 0 0)
                "the initial function does not work"))

  
  



;; A CustomerInput is one of
;; -- a PosInt
;; -- "coffee"
;; -- "hot chocolate"
;; -- "change"
;; interp: insert the specified amount of money, in cents
;; interp: request a coffee
;; interp: request a hot chocolate
;; interp: return all the unspent money that the
;; customer has inserted
         
;; A MachineOutput is one of
;; -- "coffee"         interp: machine dispenses a cup of coffee
;; -- "hot chocolate"  interp: machine dispenses a cup of hot chocolate
;; -- "Out of Item"
;; -- a PosInt
;; interp: machine displays "Out of Item"
;; interp: machine releases the specified amount of
;; money, in cents
;; interp: the machine does nothing
;; -- "Nothing"

;; initial-machine : NonNegInt NonNegInt -> MachineState
;; GIVEN: a number of cups of coffee and of hot chocolate
;; RETURNS: the state of a machine loaded with the given number of cups
;;         of coffee and of hot chocolate, with an empty bank.

;; machine-next-state : MachineState CustomerInput -> MachineState
;; GIVEN: a machine state and a customer input
;; RETURNS: the state of the machine that should follow the customer's
;; input

;; machine-output : MachineState CustomerInput -> MachineOutput
;; GIVEN: a machine state and a customer input
;; RETURNS: a MachineOutput that describes the machine's response to the
;; customer input

;; machine-remaining-coffee : MachineState -> NonNegInt
;; GIVEN: a machine state
;; RETURNS: the number of cups of coffee left in the machine

;; machine-remaining-chocolate : MachineState -> NonNegInt
;; GIVEN: a machine state
;; RETURNS: the number of cups of hot chocolate left in the machine

;; machine-bank : MachineState -> NonNegInt
;; GIVEN: a machine state
;; RETURNS: the amount of money in the machine's bank, in cents