;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sequences) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ---------------------------
;;  James Sinn (20654551)
;;  CS 135 Fall 2016
;;  Assignment 9. 
;; ---------------------------
;;

;; (sequence n1 n2 count type) produces a type sequence after expanding the first two terms, n1 and n2 to the length of count terms.
;; sequence: Num Num Nat Sym -> (listof Num)
;; Requires: type be either 'arithmetic or 'geometric.
;; Examples
(check-expect (sequence 5 4.9 10 'arithmetic) (list 5 4.9 4.8 4.7 4.6 4.5 4.4 4.3 4.2 4.1))
(check-expect (sequence 1 5 4 'geometric) (list 1 5 25 125))

(define (sequence n1 n2 count type)
  (cond
    [(symbol=? type 'arithmetic)
     (rest (build-list (add1 count) (lambda (x)
                                      (+ n1 (* (- x 1) (* -1 (- n1 n2)))))))]
    [(symbol=? type 'geometric)
     (rest (build-list (add1 count) (lambda (x)
                                      (* n1 (expt (/ n2 n1) (- x 1))))))]))

;; Tests
(check-expect (sequence -3 -5 0 'arithmetic) empty)
(check-expect (sequence -3 -5 3 'arithmetic) (list -3 -5 -7))
(check-expect (sequence 3 -1 4 'arithmetic) (list 3 -1 -5 -9))
(check-expect (sequence 1 9 3 'geometric) (list 1 9 81))
(check-expect (sequence 2 4 6 'geometric) (list 2 4 8 16 32 64))

;; (arithmetic? series) produces true if the series is an arithmetic series. False otherwise.
;; arithmetic?: (listof Num) -> Bool
;; Examples
;(check-expect (arithmetic?  (list 1 9 81)) false)
;(check-expect (arithmetic?  (list 3 -1 -5 -9)) true)


(define (arithmetic? series)
  (map (lambda (x y) (- y x)) series (rest series)))