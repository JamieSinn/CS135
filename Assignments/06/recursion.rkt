;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname recursion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ---------------------------
;;  James Sinn (20654551)
;;  CS 135 Fall 2016
;;  Assignment 6. 
;; ---------------------------
;;


;; (list-nth-item lst n) produces the n-th element in the list lst of natural numbers
;;     if it exists, otherwise it returns false.
;; list-nth-item: (listof Nat) Nat -> (anyof Nat, Bool)
;; Examples
(check-expect (list-nth-item (list 1 2 3) 0) 1)
(check-expect (list-nth-item (list 1 2 3) 1) 2)
(check-expect (list-nth-item (list 1 2 3) 2) 3)

(define (list-nth-item lst n)
  (cond
    [(empty? lst) false]
    [(= n 0) (first lst)]
    [else (list-nth-item (rest lst) (- n 1))]))
;; Tests
(check-expect (list-nth-item empty 0) false)
(check-expect (list-nth-item (list 1 1 1) 1) 1)
(check-expect (list-nth-item (list 0 1 2 3 4 5) 9) false)


;; (list-evens start end) produces a list of all even integers in-between start and end
;;     inclusive.
;; list-evens: Num Num -> (listof Num)
;; requires start <= end
;; Examples
(check-expect (list-evens -4 3) (list -4 -2 0 2))

(define (list-evens start end)
  (cond
    [(> start end) empty]
    [(even? start) (list* start (list-evens (+ 1 start) end))]
    [else (list-evens (+ 1 start) end)]))

;; Tests
(check-expect (list-evens -4 -4) (list -4))
(check-expect (list-evens 1 1) empty)
(check-expect (list-evens 2 1) empty)

;; (is-contained? list1 list2) produces true if all values in the first list are
;;     contained in the second list in any order. If not, then false is returned.
;; is-contained?: (listof Any) (listof Any) -> Bool
;; Examples
(check-expect (is-contained? (list "a" 2 4 2) (list 4 "a" 2 5)) true)
(check-expect (is-contained? empty (list "wow")) true)
(check-expect (is-contained? (list 1 2) (list 3 2 'red)) false)

(define (is-contained? list1 list2)
  (cond
    [(empty? list1) true]
    [(member? (first list1) list2) (is-contained? (rest list1) list2)]
    [else false]))

;; Tests
(check-expect (is-contained? (list 3 1 2) empty) false)
(check-expect (is-contained? (list 1 2) (list (list 1 1 1) 11)) false)


;; (digits->nat digits) produces a natural number out of list digits. The
;;     indexes in digits will be reversed to make the natural number.
;; digits->nat: (listof Nat) -> Nat
;; Examples
(check-expect (digits->nat (list 3 0 1 9 5)) 59103)
(check-expect (digits->nat (list 1 2 0 0 0)) 21)

(define (digits->nat digits)
  (cond
    [(empty? digits) 0]
    [else (list->nat (m_reverse-list digits))]))

;; Tests
(check-expect (digits->nat empty) 0)
(check-expect (digits->nat (list 0 0 0 0 0)) 0)
(check-expect (digits->nat (list 1)) 1)

;; (m_reverse-list _list) produces list reversed. This is required due to
;;     reverse being disallowed unless you write your own.
;; m_reverse-list: (listof Any) -> (listof Any)
;; Examples
(check-expect (m_reverse-list (list 1 2 0 0 0)) (list 0 0 0 2 1))

(define (m_reverse-list _list)
  (cond
    [(empty? _list) empty]
    [else (append (m_reverse-list (rest _list)) (list (first _list)))]))

;; Tests
(check-expect (m_reverse-list (list 1 2 0 0 0)) (list 0 0 0 2 1))

;; (list->nat _list) produces a number made up of the list given.replacing the indexes
;;    as the exponent on 10 multiplied by the value.
;; list->nat: (listof Nat) -> Num
;; Examples
(check-expect (list->nat (list 5 9 1 0 3)) 59103)

(define (list->nat _list)
  (cond
    [(empty? _list) 0]
    [else (+ (* (first _list ) (expt 10 (- (length _list) 1))) (list->nat (rest _list)))]))

;; Tests
(check-expect (list->nat (list 1 1 1 1)) 1111)
;; (check-expect (list->nat (list 1.1 2 3 4)) 1334) This is just a reference to my FRC Team, who's team number is 1334. Not an actual test.