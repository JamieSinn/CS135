;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname polyominoes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "a10.rkt")

;; Uncomment the following line if you want to use
;; the examples in kanoodle.rkt
;; (require "kanoodle.rkt")

;; A Grid is a (listof (listof Char))
;; requires: both inner and outer lists of Grid are non-empty

(define-struct pos (x y))
;; A Pos is a (make-pos Int Int)

(define-struct state (puzzle pieces))
;; A State is a (make-state Grid (listof Grid))

;; A temporary neighbours function that always fails.  
;; Provide only the purpose, contract and function definition.
(define (neighbours s)
  empty)

;; (solve-puzzle grid polys viz-style)
;; Solve a polyomino puzzle, given the initially empty (or partially filled 
;; in) grid, a set of pieces that must be placed, and a Symbol indicating
;; what visualization style to use.  Legal viz styles are 'interactive
;; (draw every step of the search), 'at-end (just draw the solution, if one
;; is found), or 'offline (don't draw anything).  Produce either the solved
;; Grid (converted to a list of Strings, just for convenience) or false if
;; no solution exists.
;;
;; You don't need to modify this function at all.  It is provided for you
;; so that you can test your puzzle solving algorithm interactively.  If
;; you decide you want to write check-expect tests using solve-puzzle
;; (which you don't have to do, but can if you want), be sure to consume
;; 'offline for viz-style.

;; solve-puzzle: Grid (listof Grid) Sym -> (anyof (listof Str) false)
;; requires: viz-style is one of {'interactive, 'at-end or 'offline}

;; Some Examples are included below after the solve-puzzle function definition.

;; DO NOT MODIFY THIS CODE
(define (solve-puzzle grid polys viz-style)
  (local
    [(define result
       (search 
        (lambda (S) (empty? (state-pieces S)))
        neighbours
        (cond
          [(symbol=? viz-style 'interactive)
           (lambda (S) (draw-grid (state-puzzle S)))]
          [else false])
        (make-state grid polys)))
     
     (define maybe-last-draw
       (cond
         [(and (state? result)
               (symbol=? viz-style 'at-end))
          (draw-grid (state-puzzle result))]
         [else false]))]
    (cond
      [(boolean? result) result]
      [else (map list->string (state-puzzle result))])))

;; Examples:
;; (The examples are not provided in check-expect form.  They're meant to
;; demonstrate typical uses of the function, but we don't want to them to
;; open interactive visualizations every time you start the program.)

;; Solve offline (i.e. work like a normal Scheme function).
;(solve-puzzle
;  (strlist->grid '("...." "...." "...." "...." "...." "...."))
;  (cons '((#\L #\L) (#\. #\L)) (cons '((#\O)) tetrominoes-uc))
;  'offline)

;; Display the result graphically, if a solution is found.
;(solve-puzzle
;  (strlist->grid '("...." "...." "...." "...." "...." "...."))
;  (cons '((#\L #\L) (#\. #\L)) (cons '((#\O)) tetrominoes-uc))
;  'at-end)

;; Display every step of the search as it progresses.
;(solve-puzzle
;  (strlist->grid '("...." "...." "...." "...." "...." "...."))
;  (cons '((#\L #\L) (#\. #\L)) (cons '((#\O)) tetrominoes-uc))
;  'interactive)

;; (build-2dlist w h f) produces a 2 dimensional list, equivelent to the
;;    built in racket function build-list. Applying f to all x, and y coords.
;;    
;; build-2dlist Nat Nat (Nat Nat -> Any) -> (listof (listof Any))
;; Examples
(check-expect (build-2dlist 0 0 +) empty)
(check-expect (build-2dlist 3 3 make-pos)
              (list
               (list (make-pos 0 0) (make-pos 1 0) (make-pos 2 0))
               (list (make-pos 0 1) (make-pos 1 1) (make-pos 2 1))
               (list (make-pos 0 2) (make-pos 1 2) (make-pos 2 2))))

(define (build-2dlist w h f)
  (build-2dlist/run w 0 h f))

;; Tests
(check-expect (build-2dlist 5 5 +)
              (list
               (list 0 1 2 3 4)
               (list 1 2 3 4 5)
               (list 2 3 4 5 6)
               (list 3 4 5 6 7)
               (list 4 5 6 7 8)))
(check-expect (build-2dlist 1 1 (lambda (x y) (list x y)))
              (list (list (list 0 0))))

;; (build-2dlist/run x y ymax func) produces a 2 dimensional list
;;    of size x by ymax.
;; build-2dlist/run Nat Nat Nat (Nat Nat -> Any) -> (listof (listof Any))
(define (build-2dlist/run x y ymax func)
  (cond
    [(= y ymax) empty]
    [else (append (list
                   (map func
                        (build-list x (lambda (_x) _x))
                        (build-list x (lambda (_x) y))))
                  (build-2dlist/run x (add1 y) ymax func))]))


;; (all-positions w h) produces a list of all positions available in a w by h grid.
;; all-positions: Nat Nat -> (listof Pos)
;; Requires: w and h > 0
;; Examples
(check-expect (lists-equiv? (all-positions 3 5)
                            (list (make-pos 0 0)
                                  (make-pos 1 0)
                                  (make-pos 2 0)
                                  (make-pos 0 1)
                                  (make-pos 1 1)
                                  (make-pos 2 1)
                                  (make-pos 0 2)
                                  (make-pos 1 2)
                                  (make-pos 2 2)
                                  (make-pos 0 3)
                                  (make-pos 1 3)
                                  (make-pos 2 3)
                                  (make-pos 0 4)
                                  (make-pos 1 4)
                                  (make-pos 2 4))) true)
(check-expect (lists-equiv? (all-positions 1 1)
                            (list (make-pos 0 0))) true)

(define (all-positions w h)
  (flatten (build-2dlist w h make-pos)))

;; (flatten _list) produces a list of all elements in a list flattened into one layer.
;; flatten: (listof Any) -> (listof Any)
;; Examples
(check-expect (flatten (list (list (list 1)) (list 1))) (list 1 1))

(define (flatten _list)
  (cond
    [(empty? _list) empty]
    [(cons? (first _list)) (append (flatten (first _list)) (flatten (rest _list)))]
    [else (append (list (first _list)) (flatten (rest _list)))]))
