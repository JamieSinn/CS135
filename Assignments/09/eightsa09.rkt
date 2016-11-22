;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname eightsa09) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ---------------------------
;;  James Sinn (20654551)
;;  CS 135 Fall 2016
;;  Assignment 9. 
;; ---------------------------
;;

(define-struct card (suit value))
;; A Card is a (make-card Sym Nat)
;; requires: suit is one of ’hearts, ’diamonds, ’clubs, or ’spades
;;     value between 1 and 13, using 11 for Jack, 12 for Queen, and 13 for King.

(define (crazy-count hand center)
  (length (filter (lambda (c)
                    (or (= 8 (card-value c))
                        (= (card-value c) (card-value center))
                        (symbol=? (card-suit c) (card-suit center))))
                  hand)))

(check-expect (crazy-count (list (make-card 'spades 8)) (make-card 'diamonds 2)) 1)
(check-expect (crazy-count (list (make-card 'diamonds 2) (make-card 'spades 2)) (make-card 'hearts 3)) 0)
(check-expect (crazy-count (list (make-card 'clubs 2) (make-card 'clubs 3)) (make-card 'hearts 2)) 1)

(define (crazy-dumb hand center)
  (local
    [(define result
       (filter (lambda (c)
                 (or (= 8 (card-value c))
                     (= (card-value c) (card-value center))
                     (symbol=? (card-suit c) (card-suit center))))
               hand))]
    (cond
      [(empty? result) false]
      [else (first result)])))

(check-expect (crazy-dumb (list (make-card 'spades 7)
                                (make-card 'spades 8)
                                (make-card 'spades 9))
                          (make-card 'hearts 3)) (make-card 'spades 8))
(check-expect (crazy-dumb (list (make-card 'diamonds 1)
                                (make-card 'diamonds 2)
                                (make-card 'diamonds 3))
                          (make-card 'hearts 3)) (make-card 'diamonds 3))
(check-expect (crazy-dumb (list (make-card 'spades 7)
                                (make-card 'hearts 3)
                                (make-card 'spades 9))
                          (make-card 'clubs 1)) false)

(define (crazy-smart hand center)
  (local
    [(define filtered (filter (lambda (c)
            (or
             (= (card-value c) (card-value center))
             (symbol=? (card-suit c) (card-suit center)))) hand))
     (define dumb (crazy-dumb hand center))]
  (cond
    [(and (not (empty? filtered)) (card? (first filtered))) (first filtered)]
    [else dumb])))

(check-expect (crazy-smart (list (make-card 'spades 7)
                                 (make-card 'spades 8)
                                 (make-card 'spades 9))
                           (make-card 'hearts 3)) (make-card 'spades 8))

(check-expect (crazy-smart (list (make-card 'diamonds 1)
                                 (make-card 'diamonds 2)
                                 (make-card 'diamonds 3))
                           (make-card 'hearts 3)) (make-card 'diamonds 3))

(check-expect (crazy-smart (list (make-card 'diamonds 1)
                                 (make-card 'diamonds 2)
                                 (make-card 'diamonds 8))
                           (make-card 'hearts 3)) (make-card 'diamonds 8))

(check-expect (crazy-smart (list (make-card 'spades 7)
                                 (make-card 'hearts 3))
                           (make-card 'clubs 8)) false)



(define (crazy-points hand)
  (foldr + 0 (map (lambda (c)
         (cond
           [(= (card-value c) 8) 50]
           [(>= (card-value c) 10) 10]
           [else (card-value c)])) hand)))

(check-expect (crazy-points (list (make-card 'diamonds 8))) 50)
(check-expect (crazy-points (list (make-card 'spades 7)
                                 (make-card 'hearts 3))) 10)
(check-expect (crazy-points (list (make-card 'spades 13)
                                  (make-card 'spades 13)
                                 (make-card 'hearts 3))) 23)