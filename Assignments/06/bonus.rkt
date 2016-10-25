;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname bonus) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ---------------------------
;;  James Sinn (20654551)
;;  CS 135 Fall 2016
;;  Assignment 6. 
;; ---------------------------
;;


;; Testing Matricies.
(define M (list (list -1 2 3) (list 4 5 6) (list 7 8.5 9)))

(define Mt (list (list -1 4 7) (list 2 5 8.5) (list 3 6 9)))

(define M2 (list (list -1 2 3 1) (list 4 5 6 1) (list 7 8.5 9 1)))
(define M2t (list (list -1 4 7) (list 2 5 8.5) (list 3 6 9) (list 1 1 1)))

;; A Matrix is a list containing lists of the same length. This models rows and columns. Each list within
;; the parent list is a row of the matrix.


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


;; (get-col i matrix) produces the column i from the matrix.
;; get-col: Nat (listof (listof Nat)) -> (listof Nat)
;; requires: i to be a valid column
;; Examples
(check-expect (get-col 0 M) (list -1 4 7))
(check-expect (get-col 1 M) (list 2 5 8.5))
(check-expect (get-col 2 M) (list 3 6 9))

(define (get-col i matrix)
  (cond
    [(empty? matrix) empty]
    [else (list* (list-nth-item (first matrix) i) (get-col i (rest matrix)))]))

;; Tests
(check-expect (get-col 3 M) (list false false false))
(check-expect (get-col -1 M) (list false false false))
(check-expect (get-col 1 (list (list 1 2 3) (list 4 5 6) (list 7 8 9))) (list 2 5 8))
(check-expect (get-col 1 empty) empty)

;; (transpose matrix) produces the transpose of matrix by making each column a row.
;; transpose: (listof (listof Nat)) -> (listof (listof Nat))
;; Examples
(check-expect (transpose M) Mt)
(check-expect (transpose Mt) M)

(define (transpose matrix)
  (transpose-main matrix 0 (- (length (first matrix)) 1)))
  
;; Tests
(check-expect (transpose M2) M2t)
(check-expect (transpose (list (list 1))) (list (list 1)))
(check-expect (transpose (list (list 1 2 3) (list 4 5 6) (list 7 8 9))) (list (list 1 4 7) (list 2 5 8) (list 3 6 9)))


;; (transpose-main matrix n max) produces the transpose of matrix. Iterates until max is reached.
;; transpose-main: (listof (listof Nat)) Nat Nat -> (listof (listof Nat))
(define (transpose-main matrix n max)
  (cond
    [(> n max) empty]
    [else (list* (get-col n matrix) (transpose-main matrix (+ n 1) max))]))

(define r1 (list (list 1)))
(define r2 (list (list 1 0) (list 0 1)))
(define r3 (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))
(define r4 (list (list 1 0 0 0) (list 0 1 0 0) (list 0 0 1 0) (list 0 0 0 1)))

(define (orthogonal? matrix)
  (cond
    [(equal? (multiplymatrix matrix (transpose matrix)) (ident (length (first matrix)))) true]
    [else false]))


(check-expect (orthogonal? r3) true)


(define (multiplymatrix m1 m2)
  (cond
    [(empty? m1) empty]
    [else (list* (list (row-mult (first (first m1)) (first m2))
                       (row-mult (second (first m1)) (first m2))
                       (row-mult (third (first m1)) (first m2)))
                 (multiplymatrix (rest m1) (rest m2)))]))

(define (row-mult n listn)
  (cond
    [(empty? listn) 0]
    [else (+ (* n (first listn)) (row-mult n (rest listn)))]))

(define (ident len)
  (cond
    [(= len 1) r1]
    [(= len 2) r2]
    [(= len 3) r3]
    [(= len 4) r4]))