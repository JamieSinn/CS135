;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname listsa09) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ---------------------------
;;	James Sinn (20654551)
;;  CS 135 Fall 2016
;; 	Assignment 9. 
;; ---------------------------
;;

;; (count-ints lon) produces the number of integers in the list of numbers provided.
;; count-ints: (listof Num) -> Int
;; Examples
(check-expect (count-ints (cons 1 (cons 2.1 (cons 0 empty)))) 2)
(check-expect (count-ints (cons 1 (cons 2.1 (cons 0 empty)))) 2)
(check-expect (count-ints (cons 1.3 (cons 2.1 (cons 0 empty)))) 1)
(check-expect (count-ints empty) 0)

(define (count-ints lon)
  (length (filter integer? lon)))


;; (string-replace string target replacement) produces a string with any occurances
;;    of the character target in string replaced with the character replacement. This is the wrapper function.
;; string-replace: Str Char Char -> Str
;; Examples
(check-expect (string-replace "word" #\o #\y) "wyrd")
(check-expect (string-replace "computer" #\e #\o) "computor")
(check-expect (string-replace "racket" #\s #\q) "racket")
(check-expect (string-replace "eeeeeeee" #\e #\a) "aaaaaaaa")

(define (string-replace string target replacement)
  (list->string   (map (lambda (list) (first list))
       (map (lambda (char)
              (cond
                [(char=? char target) (list replacement)]
                [else (list char)])) (string->list string)))))

;; (perfect-squares? listofnums) produces true when all elements in the list listofnums are perfect squares
;;    and produces false when one or more are not.
;; perfect-squares? (listof Num) -> Bool
;; Examples
(check-expect (perfect-squares? (cons 4 (cons 9 empty))) true)
(check-expect (perfect-squares? (cons 1.4 (cons 9 empty))) false)
(check-expect (perfect-squares? empty) true)
(check-expect (perfect-squares? (cons 16 empty)) true)

(define (perfect-squares? listofnums)
  (= (length (filter integer? (map sqrt listofnums))) (length listofnums)))

;; (keep-divisible listofnums divisor) produces a sub list of numbers from listofnums that are divisible by divisor
;; keep-divisible: (listof Num) Num -> (listof Num)
;; Examples
(check-expect (keep-divisible (cons 1 (cons 6 (cons 2 (cons 3 empty)))) 3) (cons 6 (cons 3 empty)))
(check-expect (keep-divisible (cons 1 (cons 6 (cons 2 (cons 3 empty)))) 1) (cons 1 (cons 6 (cons 2 (cons 3 empty)))))
(check-expect (keep-divisible (cons 1 (cons 6 (cons 2 (cons 3 empty)))) -1) (cons 1 (cons 6 (cons 2 (cons 3 empty)))))
(check-expect (keep-divisible (cons -1 (cons -6 (cons -2 (cons -3 empty)))) 1) (cons -1 (cons -6 (cons -2 (cons -3 empty)))))

(define (keep-divisible listofnums divisor)
  (filter (lambda (x) (integer? (/ x divisor))) listofnums))

