;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ---------------------------
;;	James Sinn (20654551)
;;  CS 135 Fall 2016
;; 	Assignment 4/5. 
;; ---------------------------
;;
;; Just as a note, I really love recursion. Try googling it!


;; (count-ints lon) produces the number of integers in the list of numbers provided.
;; count-ints: (listof Num) -> Int
;; Examples
(check-expect (count-ints (cons 1 (cons 2.1 (cons 0 empty)))) 2)
(check-expect (count-ints (cons 1 (cons 2.1 (cons 0 empty)))) 2)
(check-expect (count-ints (cons 1.3 (cons 2.1 (cons 0 empty)))) 1)
(check-expect (count-ints empty) 0)

(define (count-ints lon)
  (cond
    [(empty? lon) 0]
    [(integer? (first lon)) (+ 1 (count-ints (rest lon)))]
    [else (count-ints (rest lon))]))


;; (string-replace string target replacement) produces a string with any occurances
;;    of the character target in string replaced with the character replacement. This is the wrapper function.
;; string-replace: Str Char Char -> Str
;; Examples
(check-expect (string-replace "word" #\o #\y) "wyrd")
(check-expect (string-replace "computer" #\e #\o) "computor")
(check-expect (string-replace "racket" #\s #\q) "racket")
(check-expect (string-replace "eeeeeeee" #\e #\a) "aaaaaaaa")

(define (string-replace string target replacement)
  (list->string (string-replace-master (string->list string) target replacement)))

;; (string-replace-master charlist target replacement) produces a list of characters with any occurances
;;    of the character target in the character list of charlist with the character replacement
;; string-replace-master: (listof Char) Char Char -> (listof Char)
;; Example
(check-expect (string-replace-master (cons #\e (cons #\e (cons #\e (cons #\e (cons #\e (cons #\e '())))))) #\e #\a)
              (cons #\a (cons #\a (cons #\a (cons #\a (cons #\a (cons #\a '())))))))

(define (string-replace-master charlist target replacement)
  (cond
    [(empty? charlist) empty]
    [(char=? (first charlist) target) (cons replacement (string-replace-master (rest charlist) target replacement))]
    [else (cons (first charlist) (string-replace-master (rest charlist) target replacement))]))

;; (perfect-squares? listofnums) produces true when all elements in the list listofnums are perfect squares
;;    and produces false when one or more are not.
;; perfect-squares? (listof Num) -> Bool
;; Examples
(check-expect (perfect-squares? (cons 4 (cons 9 empty))) true)
(check-expect (perfect-squares? (cons 1.4 (cons 9 empty))) false)
(check-expect (perfect-squares? empty) true)
(check-expect (perfect-squares? (cons 16 empty)) true)

(define (perfect-squares? listofnums)
  (cond
    [(empty? listofnums) true]
    [(integer? (sqrt (first listofnums))) (perfect-squares? (rest listofnums))]
    [else false]))

;; (keep-divisible listofnums divisor) produces a sub list of numbers from listofnums that are divisible by divisor
;; keep-divisible: (listof Num) Num -> (listof Num)
;; Examples
(check-expect (keep-divisible (cons 1 (cons 6 (cons 2 (cons 3 empty)))) 3) (cons 6 (cons 3 empty)))
(check-expect (keep-divisible (cons 1 (cons 6 (cons 2 (cons 3 empty)))) 1) (cons 1 (cons 6 (cons 2 (cons 3 empty)))))
(check-expect (keep-divisible (cons 1 (cons 6 (cons 2 (cons 3 empty)))) -1) (cons 1 (cons 6 (cons 2 (cons 3 empty)))))
(check-expect (keep-divisible (cons -1 (cons -6 (cons -2 (cons -3 empty)))) 1) (cons -1 (cons -6 (cons -2 (cons -3 empty)))))

(define (keep-divisible listofnums divisor)
  (cond
    [(empty? listofnums) empty]
    [(= (modulo (first listofnums) divisor) 0) (cons (first listofnums) (keep-divisible (rest listofnums) divisor))]
    [else (keep-divisible (rest listofnums) divisor)]))

