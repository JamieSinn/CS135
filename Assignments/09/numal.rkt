;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname numal) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ---------------------------
;;  James Sinn (20654551)
;;  CS 135 Fall 2016
;;  Assignment 9. 
;; ---------------------------
;;

;; A Number Association List (NAL) is one of:
;; * empty
;; * (cons (list Num Any) NAL)
;; A Pair is a (list Any Any).

;; (zip list1 list2) produces a Pair for each "pair" of values from list1 and list2
;; zip: (listof Any) (listof Any) -> (listof Pair)
;; Examples
(check-expect (zip (list 1 2 3 4) (list 1 2 3 4)) (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)))
(check-expect (zip (list 2 2 2 2) (list 1 2 3 4)) (list (list 2 1) (list 2 2) (list 2 3) (list 2 4)))
(define (zip list1 list2)
  (map list list1 list2))

;; Tests
(check-expect (zip (list 1 2 3 4) (list 1 2 3 4)) (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)))
(check-expect (zip empty empty) empty)

;; (unzip pairs) produces a list of two lists, as an expanded set of Pairs
;; unzip: (listof Pair) -> (listof (listof Any) (listof Any))
;; Examples
(check-expect (unzip (list (list 1 1) (list 2 2) (list 3 3) (list 4 4))) (list (list 1 2 3 4) (list 1 2 3 4)))
(check-expect (unzip (list (list 2 1) (list 2 2) (list 2 3) (list 2 4))) (list (list 2 2 2 2) (list 1 2 3 4)))

(define (unzip pairs)
  (list (map (lambda (x) (first x)) pairs)
        (map (lambda (x) (second x)) pairs)))

;; Tests
(check-expect (unzip empty) (list empty empty))

;; (occurences _list v) produces the amount of times v occurs in _list
;; occurences: (listof Any) Any -> Nat
;; Examples
(check-expect (occurrences (list 1 1 1 1 2) 1) 4)
(check-expect (occurrences (list 1 1 1 1 2) 3) 0)

(define (occurrences _list v)
  (length (filter (lambda (x) (equal? x v)) _list)))

;; Tests

(check-expect (occurrences (list "A" (list 1 1 1) 2 -1) -1) 1)

;; (euclidean-dist p1 p2) produces the euclidean distance between two sets of points in n dimentions
;; euclidean-dist: (listof Num) (listof Num) -> Num
;; Examples
(check-expect (euclidean-dist (list 1 2 3 4) (list 1 2 3 4)) 0)

(define (euclidean-dist p1 p2)
  (sqrt (foldr + 0 (map (lambda (x y) (expt (- x y) 2)) p1 p2))))

;; Tests
(check-expect (euclidean-dist (list 0 0 0) (list 0 0 3)) 3)
(check-expect (euclidean-dist (list 0 0 0) (list 0 0 13)) 13)
(check-expect (euclidean-dist (list 0 0 0) (list 0 0 1)) 1)

