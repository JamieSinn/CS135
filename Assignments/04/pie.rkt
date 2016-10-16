;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname pie) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ---------------------------
;;  James Sinn (20654551)
;;  CS 135 Fall 2016
;;  Assignment 4/5. 
;; ---------------------------
;;
(define smallcost 6)
(define mediumcost 9)
(define largecost 15)
(define deluxecost-sm-md 2)
(define deluxecost-lg 5)



(define-struct pie (size fillings))
;; A Pie is a (make-pie Sym (listof Sym))
;; requires: size is one of 'small 'medium or 'large.

(define test-pielist (cons (make-pie 'small (cons 'strawberry empty))
                                (cons (make-pie 'medium (cons 'strawberry empty))
                                      (cons (make-pie 'large (cons 'strawberry (cons 'durian empty))) empty))))
;; (pie-cost pie) produces the cost of a pie by analyzing its components. I sweat it doesn't eat it.
;; pie-cost: Pie -> Int
;; Examples
(check-expect (pie-cost (make-pie 'small (cons 'strawberry empty))) smallcost)
(check-expect (pie-cost (make-pie 'medium (cons 'strawberry empty))) mediumcost)
(check-expect (pie-cost (make-pie 'large (cons 'strawberry (cons 'durian empty)))) 20)

(define (pie-cost pie)
  (+
   (* (pie-filling-cost (pie-fillings pie)) (pie-deluxe-cost (pie-size pie)))
   (pie-size-cost (pie-size pie))))


;; (pie-filling-cost fillings) produces the cost of the fillings. It returns
;;     0 unless there is a premium filling, which it then returns 1.
;; pie-filling-cost: (listof Sym) -> Int
;; Examples
(check-expect (pie-filling-cost (cons 'strawberry empty)) 0)

(define (pie-filling-cost fillings)
  (cond
    [(empty? fillings) 0]
    [(or
      (symbol=? (first fillings) 'blackberry)
      (symbol=? (first fillings) 'raspberry)
      (symbol=? (first fillings) 'durian)) 1]
    [else (pie-filling-cost (rest fillings))]))

;; (pie-size-cost size) produces the cost of a given size of pie.
;; pie-size-cost: Sym -> Int
;; Examples
(check-expect (pie-size-cost 'large) largecost)

(define (pie-size-cost size)
  (cond
    [(symbol=? size 'small) smallcost]
    [(symbol=? size 'medium) mediumcost]
    [(symbol=? size 'large) largecost]))

;; (pie-deluxe-cost size) produces the cost of a deluxe fillings.
;; pie-deluxe-cost: Sym -> Int
;; Examples
(check-expect (pie-deluxe-cost 'large) deluxecost-lg)

(define (pie-deluxe-cost size)
  (cond
    [(symbol=? size 'small) deluxecost-sm-md]
    [(symbol=? size 'medium) deluxecost-sm-md]
    [(symbol=? size 'large) deluxecost-lg]))

;; (order-cost order) produces the total cost of an order of pies.
;; order-cost: (listof Pie) -> Int
;; Examples
(check-expect (order-cost test-pielist)
              35)
(define (order-cost order)
  (cond
    [(empty? order) 0]
    [else (+ (pie-cost (first order)) (order-cost (rest order)))]))


;; (ok-to-order? pies filling) produces a Boolean value relating
;;    to whether or not the list of pies provided contained the filling.
;; ok-to-order? (listof Pie) Sym -> Bool
;; Examples
(check-expect (ok-to-order? test-pielist 'blueberry) true)
(check-expect (ok-to-order? test-pielist 'strawberry) false)

(define (ok-to-order? pies filling)
  (cond
   [(empty? pies) true]
   [(check-fillings? (pie-fillings (first pies)) filling) false]
   [else (ok-to-order? (rest pies) filling)]))

;; (check-fillings? fillings allergen) produces true when the list of fillings contains the allergen. It produces false otherwise.
;; check-fillings?: (listof Sym) Sym -> Bool
;; Examples
(check-expect (check-fillings? (cons 'strawberry empty) 'blueberry) false)
(check-expect (check-fillings? (cons 'redberry (cons 'strawberry empty)) 'strawberry) true)
 
(define (check-fillings? fillings allergen)
  (cond
    [(empty? fillings) false]
    [(symbol=? allergen (first fillings)) true]
    [else (check-fillings? (rest fillings) allergen)]))