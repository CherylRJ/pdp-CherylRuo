;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname editor) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require rackunit)
(require "extras.rkt")
(provide make-editor
         editor-pre
         editor-post
         edit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Data Definition ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct editor (pre post))
;; An Editor is a (make-editor String String)
;; It represents a string in an editor, the string is
;; (string-append pre post)
;; Interpretation:
;;   pre: string previous to the cursor
;;   post: string post to the cursor
;; Decomposition Template:
;; editor-fn: Editor -> ???
;; (define (editor-fn ed)
;;    (...
;;       (editor-pre ed)
;;       (editor-post ed)
;;     ...))
;; Examples:
(define ed1 (make-editor "hello" "world"))
(define ed2 (make-editor "" "helloworld"))
(define ed3 (make-editor "helloworld" ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; required function definition ;;;;;;;;;;;;;;;;;;;;
;; edit: Editor KeyEvent -> Editor
;; KeyEvent is one of:
;; GIVEN: an Editor and a KeyEvent
;; RETURNS: an Editor edited by the KeyEvent
;; Examples:
;; (edit (make-editor "1" "2") "1") => (make-editor "11" "2")
;; (edit (make-editor "1" "23") "right") => (make-editor "12" "3")
;; (edit (make-editor "12" "34") "left") => (make-editor "1" "234")
;; (edit (make-editor "12" "34") "\b") => (make-editor "1" "34")
;; (edit (make-editor "12" "34") "a") => (make-editor "12a" "34")
;; STRATEGY: cases on events
(define (edit ed ke)
  (cond
    [(key=? ke "\b") (others ed ke)]
    [(and (= (string-length ke) 1)
          (not (key=? ke "\b"))
          (not (key=? ke "\t"))
          (not (key=? ke "\u007F")))(insert ed ke)]
    [(or (key=? ke "left")(key=? ke "right")) (others ed ke)]
    [else ed]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; help functions definition ;;;;;;;;;;;;;;;;;;;;;;;
;; others: Editor KeyEvent -> Editor
;; GIVEN: an Editor and a "left" or "right" or "\b" KeyEvent command
;; RETURNS: an Editor edited by these Keyevent command
;; Examples:
;; (others (make-editor "1" "23") "right") => (make-editor "12" "3")
;; (others (make-editor "12" "34") "left") => (make-editor "1" "234")
;; (others (make-editor "12" "34") "\b") => (make-editor "1" "34")
;; STRATEGY: cases on events 
(define (others ed ke)
  (cond
    [(key=? ke "left") (cursor-left ed)]
    [(key=? ke "right") (cursor-right ed)]
    [(key=? ke "\b") (backspace ed)]))

;; insert: Editor KeyEvent -> Editor
;; GIVEN: an Editor and a KeyEvent
;; RETUNS: the given editor with the KeyEvents character inserted left to the cursor
;; Examples:
;; (insert (make-editor "12" "34") "5") => (make-editor "125" "34")
;; (insert (make-editor "12" "34") "b") => (make-editor "12b" "34")
;; STRATEGY: structural decomposition
(define (insert ed ke)
  (make-editor (string-append (editor-pre ed) ke)
               (editor-post ed)))

;; left: Editor -> Editor
;; GIVEN: an Editor
;; RETURNS: an Editor same to the given one, except its 
;; cursor moves left for one space (if possible)
;; Examples:
;; (left (make-editor "12" "34")) => (make-editor "1" "234")
;; STRATEGY: structural decomposition
(define (cursor-left ed)
  (cond
    [(>(string-length (editor-pre ed)) 0)
     (make-editor (string-remove-last (editor-pre ed))
                  (string-append (string-last (editor-pre ed)) 
                                 (editor-post ed)))]
    [else ed]))

;; right: Editor -> Editor
;; GIVEN: an Editor
;; RETURNS: an Editor same to the given one, except its 
;; cursor move right for one space (if possible)
;; Examples:
;; (right (make-editor "1" "23")) => (make-editor "12" "3")
;; STRATEGY: structural decomposition
(define (cursor-right ed)
  (cond
    [(> (string-length (editor-post ed)) 0) 
     (make-editor (string-append (editor-pre ed)(string-first (editor-post ed)))
                  (string-rest (editor-post ed)))]
    [else ed]))

;; backspace: String String -> Editor
;; GIVEN: an Editor's pre string and post string (divided by cursor)
;; RETURN: an Editor whose one charactor previous to cursor has been deleted if it exist
;; Examples:
;; (backspace (make-editor "12" "34")) => (make-editor "1" "34")
;; STRATEGY: structural decomposition
(define (backspace ed)
  (cond
    [(> (string-length (editor-pre ed)) 0) 
     (make-editor (string-remove-last (editor-pre ed)) (editor-post ed))]
    [else ed]))

;; string-rest: String -> String
;; GIVEN: a string data
;; RETURNS: the given string with its first char cut off
;; Example:
;; (string-rest "hello world") => "ello world")
;; STRATEGY: function composition
(define (string-rest str)
  (substring str 1))

;; string-first: String -> String:
;; GIVEN: a string
;; RETUNS: the first char of the string(in string data type)
;; Example:
;; (string-first "hello world") => "h"
;; STRATEGY: function composition
(define (string-first str)
  (string-ith str 0))

;; string-last: String -> String
;; GIVEN: a string
;; RETURNS: the given string's last char (in string data type)
;; Example
;; (string-last "hello world") => "d"
;; STRATEGY: function composition
(define (string-last str)
  (string-ith str (-(string-length str) 1)))

;; string-remove-last: String -> String
;; GIVEN: a string
;; RETURNS: the given string with its last char cut off
;; Example:
;; (string-remove-last "hello world") => "hello worl"
;; STRATEGY: function composition
(define (string-remove-last str)
  (substring str 0 (-(string-length str) 1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Tests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(begin-for-test
;; tests for specific inputs other than inserting character
  (check-equal? (edit ed1 "left")(make-editor "hell" "oworld")
                "output editor wrong")
  (check-equal? (edit ed2 "left") ed2
                "output editor wrong")
  (check-equal? (edit ed1 "right")(make-editor "hellow" "orld")
                "output editor wrong")
  (check-equal? (edit ed3 "right") ed3)
  (check-equal? (edit ed1 "\b")(make-editor "hell" "world")
                "output editor wrong")
  (check-equal? (edit (make-editor "" "helloworld") "\b")(make-editor "" "helloworld")
                "output editor wrong")
;; tests for inserting single character
  (check-equal? (edit ed1 "a")(make-editor "helloa" "world")
                "output editor wrong")
  (check-equal? (edit ed1 "/")(make-editor "hello/" "world")
                "output editor wrong")
  (check-equal? (edit ed1 "9")(make-editor "hello9" "world")
                "output editor wrong")
;; test for other inputs
  (check-equal? (edit (make-editor "hello" "world") "\u007F")(make-editor "hello" "world")
                "output editor wrong"))

