;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname bridgescore) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;************************************************
;; Jamie Sinn (20654551)
;; CS 135 Fall 2016
;; Assignment 02
;;************************************************
;;

;; QUESTION 2. A

;; (contract-points level suit doubled redoubled) produces the points a
;;     given contract will produce
;; contract-points: Num Sym Bool Bool -> Num
;; Examples:
(check-expect (contract-points 2 'clubs true false) 80)

(define (contract-points level suit doubled redoubled)
  (cond
    ;; Redoubled
    [(and doubled redoubled)
     (suit-points level suit 160)
     ]
    ;; Doubled
    [doubled
     (suit-points level suit 80)
     ]
    ;; Undoubled
    [else
     (suit-points level suit 40)
     ]
    )
  )
;; Tests:
(check-expect (contract-points 3 'NT false false) 100)
(check-expect (contract-points 4 'spades true true) 480)

;; (suit-points level suit ft_nt) produces the points a suit at a certain level
;;     will produce. 
;; suit-points: Num Sym Num -> Num
;; Examples:
(check-expect (suit-points 4 'spades 160) 480)

(define (suit-points level suit ft_nt)
  (cond
    [(major? suit) (* (- ft_nt (/ ft_nt 4)) level)]
    [(minor? suit) (* (/ ft_nt 2) level)]
    [else (+ (* (- ft_nt (/ ft_nt 4)) (- level 1)) ft_nt)]
    )
  )
;; (major? suit) produces a boolean of whether or not a suit is a major one.
;; major?s: Sym -> Bool
(define (major? suit)
  (or (equal? suit 'hearts) (equal? suit 'spades)))

;; (minor? suit) produces a boolean of whether or not a suit is a minor one.
;; minor?: Sym -> Bool
(define (minor? suit)
  (or (equal? suit 'diamonds) (equal? suit 'clubs)))

;; END 2. A

;; QUESTION 2. B
;; (penalty-points underticks vuln doubled redoubled) produces the penalty points for not meeting a contract
;; penalty-points: Num Bool Bool Bool -> Num
;; Examples:
(check-expect (penalty-points 4 false false false) -200)
(check-expect (penalty-points 1 true true false) -200)
(check-expect (penalty-points 2 true false false) -200)
(check-expect (penalty-points 3 false true false) -500)
(check-expect (penalty-points 4 true true true) -2200)

(define (penalty-points underticks vuln doubled redoubled)
  (* (cond
       [(>= underticks 4)
        (+ (pen-points 1 vuln doubled redoubled)
           (pen-points 2 vuln doubled redoubled)
           (pen-points 3 vuln doubled redoubled)
           (* (/ underticks 4) (pen-points 4 vuln doubled redoubled)))
        ]
       [(= underticks 3)
        (+ (pen-points 1 vuln doubled redoubled)
           (pen-points 2 vuln doubled redoubled)
           (pen-points 3 vuln doubled redoubled)
           )
        ]
       [(= underticks 2)
        (+ (pen-points 1 vuln doubled redoubled)
           (pen-points 2 vuln doubled redoubled)
           )
        ]
       [(= underticks 1)
        (pen-points 1 vuln doubled redoubled)
        ]
       )
     -1)
  )
;; (pen-points underticks vuln doubled redoubled) produces the penalty points for a single iteration of the points
;;     evaluator
;; pen-points: Num Bool Bool Bool -> Num
(define (pen-points underticks vuln doubled redoubled)
  (cond
    [(= underticks 1)
     (cond
       [(and (not doubled) (not redoubled))
        (cond [vuln 100] [else 50])
        ]
       [(and doubled (not redoubled))
        (cond [vuln 200] [else 100])
        ]
       [(and doubled redoubled)
        (cond [vuln 400] [else 200])
        ]
       )
     ]
    [(and (> underticks 1) (< underticks 4))
     (cond
       [(and (not doubled) (not redoubled))
        (cond [vuln 100] [else 50])
        ]
       [(and doubled (not redoubled))
        (cond [vuln 300] [else 200])
        ]
       [(and doubled redoubled)
        (cond [vuln 600] [else 400])
        ]
       )
     ]
    [(>= underticks 4)
     (cond
       [(and (not doubled) (not redoubled))
        (cond [vuln 100] [else 50])
        ]
       [(and doubled (not redoubled))
        300
        ]
       [(and doubled redoubled)
        600
        ]
       )
     ]
    )
  )
;; END 2. B


;; QUESTION 2. C

(define (duplicate-score level suit result vuln doubled redoubled)
  (cond
    [(< result 0)
     (penalty-points level vuln doubled redoubled)
     ]
    [else
     (+ (contract-points level suit doubled redoubled)
        (game-bonus (contract-points level suit doubled redoubled) vuln)
        (insult-bonus doubled redoubled)
        (overtrick-bonus suit vuln doubled redoubled)
        )
     ]
    )
  )
(check-expect (duplicate-score 3 'NT 4 false true false) 650)

(define (game-bonus contract-points vuln)
  (cond
    [(< contract-points 100)
     50
     ]
    [(and (>= contract-points 100) (not vuln))
     300
     ]
    [(and (>= contract-points 100) vuln)
     500
     ]
    [else 0]
    )
  )

(define (insult-bonus doubled redoubled)
  (cond
    [(and doubled redoubled)
     100
     ]
    [(and doubled (not redoubled))
     50
     ]
    [else 0]
    )
  )
(define (overtrick-bonus suit vuln doubled redoubled)
  (cond
    [(and (not doubled) (not redoubled))
     (cond
       [(major? suit)
        30
        ]
       [(minor? suit)
        20
        ]
       [else 30]
       )
     ]
    [(and doubled redoubled)
     (cond
       [vuln
        400
        ]
       [else 200]
       )
     ]
    [(and doubled (not redoubled))
     (cond
       [vuln
        200
        ]
       [else 100]
       )
     ]
    )
  )




