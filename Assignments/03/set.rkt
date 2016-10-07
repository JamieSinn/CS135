;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname set) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct setcard (number colour shape shading))

;; (valid-set? set1 set2 set3) produces a boolean of whether or not the supplied 3 set cards match.
;; valid-set?: SetCard SetCard SetCard SetCard -> Bool
(define (valid-set? set1 set2 set3)
  (or
   (match-colour? set1 set2 set3)
   (match-shape? set1 set2 set3)
   (match-number? set1 set2 set3)
   (match-shading? set1 set2 set3)
   )
  )
;; (match-colour? set1 set2 set3) produces a boolean of whether the 3 set cards given match by their colour.
;; match-colour?: SetCard SetCard SetCard SetCard -> Bool
(define (match-colour? set1 set2 set3)
  (cond
    [(and (equal?
           (setcard-colour set1)
           (setcard-colour set2))
          (equal? (setcard-colour set3) (setcard-colour set2)))
     true]
    [(and (not (equal? (setcard-colour set1) (setcard-colour set2)))
          (not (equal? (setcard-colour set1) (setcard-colour set3))))
     true]
    [else false]
    )
  )
;; (match-shape? set1 set2 set3) produces a boolean of whether the 3 set cards given match by their shape.
;; match-shape?: SetCard SetCard SetCard SetCard -> Bool
(define (match-shape? set1 set2 set3)
  (cond
    [(and (equal?
           (setcard-shape set1)
           (setcard-shape set2))
          (equal? (setcard-shape set3) (setcard-shape set2)))
     true]
    [(and (not (equal? (setcard-shape set1) (setcard-shape set2)))
          (not (equal? (setcard-shape set1) (setcard-shape set3))))
     true]
    [else false]
    )
  )
;; (match-number? set1 set2 set3) produces a boolean of whether the 3 set cards given match by their number.
;; match-number?: SetCard SetCard SetCard SetCard -> Bool
(define (match-number? set1 set2 set3)
  (cond
    [(and (equal?
           (setcard-number set1)
           (setcard-number set2))
          (equal? (setcard-number set3) (setcard-number set2)))
     true]
    [(and (not (equal? (setcard-number set1) (setcard-number set2)))
          (not (equal? (setcard-number set1) (setcard-number set3))))
     true]
    [else false]
    )
  )
;; (match-shading? set1 set2 set3) produces a boolean of whether the 3 set cards given match by their shading
;; match-shading?: SetCard SetCard SetCard SetCard -> Bool
(define (match-shading? set1 set2 set3)
  (cond
    [(and (equal?
           (setcard-shading set1)
           (setcard-shading set2))
          (equal? (setcard-shading set3) (setcard-shading set2)))
     true]
    [(and (not (equal? (setcard-shading set1) (setcard-shading set2)))
          (not (equal? (setcard-shading set1) (setcard-shading set3))))
     true]
    [else false]
    )
  )

;; (complete-set set1 set2) produces a the final card that will complete the set given the 2 set cards. If it cannot find a match it returns false
;; complete-set: SetCard SetCard -> SetCard
(define (complete-set set1 set2)
  (cond
    [(checkFullMatch? set1 set2)
     (make-setcard (setcard-number set1) (setcard-colour set1) (setcard-shape set1) (setcard-shading set1))
     ]
    [(checkNoMatch? set1 set2)
     (make-setcard (get-number set1 set2) (get-colour set1 set2) (get-shape set1 set2) (get-shading set1 set2))
     ]
    [else false]
    ))


;; (checkFullMatch? set1 set2) produces a boolean of whether or not the given 2 set cards match in all catagories of colour, shape, number, and shading.
;; checkFullMatch?: SetCard SetCard -> Bool
(define (checkFullMatch? set1 set2)
  (and (equal? (setcard-colour set1)
               (setcard-colour set2))
       (equal? (setcard-number set1)
               (setcard-number set2))
       (equal? (setcard-shading set1)
               (setcard-shading set2))
       (equal? (setcard-shape set1)
               (setcard-shape set2)))
  )

;; (checkNoMatch? set1 set2) produces a boolean of whether or not the given 2 set cards are unique. Meaning that all catagories are not paired.
;; checkNoMatch?: SetCard SetCard -> Bool
(define (checkNoMatch? set1 set2)
  (and (not (equal? (setcard-colour set1)
                    (setcard-colour set2)))
       (not (equal? (setcard-number set1)
                    (setcard-number set2)))
       (not (equal? (setcard-shading set1)
                    (setcard-shading set2)))
       (not (equal? (setcard-shape set1)
                    (setcard-shape set2)))
       )
  )
;; (get-colour set1 set2) produces the colour of the card that will create a match with the given 2 set cards.
;; get-colour: SetCard SetCard -> Sym
(define (get-colour set1 set2)
  (cond
    [(and (not (symbol=? (setcard-colour set1) 'red)) (not(symbol=? (setcard-colour set2) 'red))) 'red]
    [(and (not (symbol=? (setcard-colour set1) 'green)) (not(symbol=? (setcard-colour set2) 'green))) 'green]
    [(and (not (symbol=? (setcard-colour set1) 'purple)) (not(symbol=? (setcard-colour set2) 'purple))) 'purple]
    )
  )
;; (get-shape set1 set2) produces the shape of the card that will create a match with the given 2 set cards.
;; get-shape: SetCard SetCard -> Sym
(define (get-shape set1 set2)
  (cond
    [(and (not (symbol=? (setcard-shape set1) 'diamond)) (not(symbol=? (setcard-shape set2) 'diamond))) 'diamond]
    [(and (not (symbol=? (setcard-shape set1) 'oval)) (not(symbol=? (setcard-shape set2) 'oval))) 'oval]
    [(and (not (symbol=? (setcard-shape set1) 'squiggle)) (not(symbol=? (setcard-shape set2) 'squiggle))) 'squiggle]
    )
  )
;; (get-number set1 set2) produces the number for the card that will create a match with the given 2 set cards.
;; get-number: SetCard SetCard -> Sym
(define (get-number set1 set2)
  (cond
    [(and (not (equal? (setcard-number set1) 1)) (not(equal? (setcard-number set2) 1))) 1]
    [(and (not (equal? (setcard-number set1) 2)) (not(equal? (setcard-number set2) 2))) 2]
    [(and (not (equal? (setcard-number set1) 3)) (not(equal? (setcard-number set2) 3))) 3]
    )
  )
;; (get-shading set1 set2) produces the shading of the card that will create a match with the given 2 set cards.
;; get-shading: SetCard SetCard -> Sym
(define (get-shading set1 set2)
  (cond
    [(and (not (symbol=? (setcard-shading set1) 'solid)) (not(symbol=? (setcard-shading set2) 'solid))) 'solid]
    [(and (not (symbol=? (setcard-shading set1) 'striped)) (not(symbol=? (setcard-shading set2) 'striped))) 'striped]
    [(and (not (symbol=? (setcard-shading set1) 'open)) (not(symbol=? (setcard-shading set2) 'open))) 'open]
    )
  )
