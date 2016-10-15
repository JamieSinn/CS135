;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname eights) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ---------------------------
;;  James Sinn (20654551)
;;  CS 135 Fall 2016
;;  Assignment 4/5. 
;; ---------------------------
;;

(define-struct card (suit value))
;; A Card is a (make-card Sym Nat)
;; requires: suit is one of ’hearts, ’diamonds, ’clubs, or ’spades
;;     value between 1 and 13, using 11 for Jack, 12 for Queen, and 13 for King.

;; Values for check-expect to save time.
(define test-hand (cons (make-card 'spades 10) (cons (make-card 'diamonds 11) (cons (make-card 'hearts 12) empty))))
(define test-hand-hearts (cons (make-card 'hearts 10) (cons (make-card 'hearts 11) (cons (make-card 'hearts 12) empty))))
(define test-card (make-card 'hearts 2))
(define test-hand-eight (cons (make-card 'spades 8) (cons (make-card 'diamonds 11) (cons (make-card 'hearts 12) empty))))
(define test-hand-low (cons (make-card 'spades 2) (cons (make-card 'diamonds 3) (cons (make-card 'hearts 4) empty))))


;; (card=? card1 card2) produces true when the two cards provided are the same. It produces false otherwise.
;; card=? Card Card -> Bool
;; Examples
(check-expect (card=? (make-card 'hearts 3) (make-card 'hearts 3)) true)
(check-expect (card=? (make-card 'diamonds 3) (make-card 'hearts 3)) false)
(check-expect (card=? (make-card 'hearts 13) (make-card 'hearts 13)) true)

(define (card=? card1 card2)
  (and (symbol=? (card-suit card1) (card-suit card2))
       (= (card-value card1) (card-value card2))))

;; (crazy-count hand center) produces the number of cards that can be played ontop of the center card.
;; crazy-count: (listof Card) Card -> Num
;; Examples
(check-expect (crazy-count test-hand-hearts test-card) 3)
(check-expect (crazy-count test-hand test-card) 1)

(define (crazy-count hand center)
  (cond
    [(empty? hand) 0]
    [(can-play (first hand) center )
     (+ 1 (crazy-count (rest hand) center))]
    [else (crazy-count (rest hand) center)]))

;; (can-play card center) produces true when the card can be played and false otherwise.
;; can-play Card Card -> Bool
;; Examples
(check-expect (can-play (make-card 'spades 1) (make-card 'spades 4)) true)
(check-expect (can-play (make-card 'spades 1) (make-card 'diamonds 4)) false)

(define (can-play card center)
  (or (symbol=? (card-suit card) (card-suit center))
      (= (card-value card) (card-value center))
      (= (card-value card) 8)))

;; (crazy-dumb hand center) produces the first card that meets the requirements. If no cards meet the requirements
;;     to play, then false is returned as the player then has to draw a new card.
;; crazy-dumb (listof Card) Card -> (anyof Card Bool)
;; Examples
(check-expect (crazy-dumb test-hand test-card) (make-card 'hearts 12))
(check-expect (crazy-dumb test-hand-hearts test-card) (make-card 'hearts 10))
(check-expect (crazy-dumb test-hand-hearts (make-card 'diamonds 1)) false)

(define (crazy-dumb hand center)
  (cond
    [(empty? hand) false]
    [(can-play (first hand) center) (first hand)]
    [else (crazy-dumb (rest hand) center)]))

;; (crazy-smart hand center) produces the smartest card choice. This means choosing the first card that can
;;    be played that is not an 8, if no cards other than an 8 can be played, it will play it. If nothing can
;;    be played, it returns false.
;; crazy-smart: (listof Card) Card -> (anyof Card Bool)
;; Examples
(check-expect (crazy-smart test-hand-eight test-card) (make-card 'hearts 12))
(check-expect (crazy-smart test-hand-eight (make-card 'clubs 1)) (make-card 'spades 8))

(define (crazy-smart hand center)
  (crazy-smart-main hand center (contains-8? hand) (get-8 hand)))

;; (crazy-smart-main hand center contains8 the8)  produces the smartest card choice. This means choosing the first card that can
;;    be played that is not an 8, if no cards other than an 8 can be played, it will play it. If nothing can
;;    be played, it returns false.
;; crazy-smart-main (listof Card) Card Bool (anyof Card Bool) -> (anyof Card Bool)
;; Examples
(check-expect (crazy-smart-main test-hand-eight test-card (contains-8? test-hand-eight) (get-8 test-hand-eight)) (make-card 'hearts 12))
(check-expect (crazy-smart-main test-hand (make-card 'clubs 3) (contains-8? test-hand) (get-8 test-hand)) false)
(define (crazy-smart-main hand center contains8 the8)
   (cond
    [(empty? hand)
     (cond
       [contains8 the8]
       [else false])]
    [(and (can-play (first hand) center) (= (card-value (first hand)) 8)) (crazy-smart-main (rest hand) center contains8 the8)]
    [(can-play (first hand) center) (first hand)]
    [else (crazy-smart-main (rest hand) center contains8 the8)]))

;; (contains-8? hand) produces a boolean of whether or not the hand provided contains an 8.
;; contains-8? (listof Card) -> Bool
;; Examples
(check-expect (contains-8? test-hand) false)
(check-expect (contains-8? test-hand-eight) true)

(define (contains-8? hand)
  (cond
    [(empty? hand) false]
    [(= (card-value (first hand)) 8) true]
    [else (contains-8? (rest hand))]))

;; (get-8 hand) produces the 8 in the hand. This function relies on contains-8? to be
;;    true in order for it to be run. Otherwise it will be ignored.
;; Examples
(check-expect (get-8 test-hand) false)
(check-expect (get-8 test-hand-eight) (make-card 'spades 8))

(define (get-8 hand)
  (cond
    [(empty? hand) false]
    [(= (card-value (first hand)) 8) (first hand)]
    [else (get-8 (rest hand))]))

;; (crazy-points hand) produces the number of points a given hand would be worth at the end of the game.
;;    This is assumed to be another player's hand.
;; Examples
(check-expect (crazy-points test-hand) 30)
(check-expect (crazy-points test-hand-eight) 70)
(check-expect (crazy-points test-hand-low) 9)

(define (crazy-points hand)
  (cond
    [(empty? hand) 0]
    [(>= (card-value (first hand)) 10) (+ 10 (crazy-points (rest hand)))]
    [(= (card-value (first hand)) 8) (+ 50 (crazy-points (rest hand)))]
    [else (+ (card-value (first hand)) (crazy-points (rest hand)))]))