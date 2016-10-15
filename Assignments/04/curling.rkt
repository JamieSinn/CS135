;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname curling) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ---------------------------
;;  James Sinn (20654551)
;;  CS 135 Fall 2016
;;  Assignment 4/5. 
;; ---------------------------
;;

;; (count-less-than listofnum totest) produces the amount of numbers in listofnum that are less than totest.
;; count-less-than (listof Num) Num -> Int
;; Examples:
(check-expect (count-less-than (cons 5 (cons 2 (cons 3 (cons 1 (cons 4 empty))))) 4) 3)
(check-expect (count-less-than (cons 35 (cons 40 (cons 50 (cons 60 empty)))) 15) 0)
(define (count-less-than listofnum totest)
  (cond
    [(empty? listofnum) 0]
    [(< (first listofnum) totest)
     (+ 1 (count-less-than (rest listofnum) totest))]
    [else (count-less-than (rest listofnum) totest)]))

;; (end-points team1 team2) produces the score that the team won. If the number is positive, then team1 won.
;;    If negative, team2 won.
;; end-points (listof Num) (listof Num) -> Int
;; Examples
(check-expect (end-points (cons 15 (cons 25 (cons 35 empty))) (cons 35 (cons 40 (cons 50 (cons 60 empty))))) 2)
(check-expect (end-points (cons 35 (cons 40 (cons 50 (cons 60 empty)))) (cons 15 (cons 25 (cons 35 empty)))) -2)
(check-expect (end-points empty empty) 0)
(check-expect (end-points (cons 15 (cons 25 (cons 35 empty))) (cons 15 (cons 25 (cons 35 empty)))) 0)
(check-expect (end-points (cons 188 (cons 190 (cons 200 empty))) (cons 180 (cons 185 empty))) -1)

(define (end-points team1 team2)
  (cond
    [(or (empty? team1) (empty? team2)) 0]
    [(= (all-greater (first team1) team2) 1) (+ 1 (all-greater (first (rest team1)) team2))]
    [(= (all-greater (first team2) team1) 1) (- 0 1 (all-greater (first (rest team2)) team1))]
    [else 0]))

;; (all-greater totest list) produces a 1 if list contains only numbers greater than totest
;;   and produces a 0 otherwise.
;; all-greater: Num (listof Num) -> Num
;; Examples
(check-expect (all-greater 1 (cons 3 (cons 4 (cons 5 empty)))) 1)

(define (all-greater totest list)
  (cond
    [(empty? list) 1]
    [(< totest (first list)) (all-greater totest (rest list))]
    [else 0]))

