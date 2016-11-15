;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname funabst) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ---------------------------
;;  James Sinn (20654551)
;;  CS 135 Fall 2016
;;  Assignment 8. 
;; ---------------------------
;;

;; (and-pred func _list) produces false if the given func returns false on any element
;;    in the list _list. It produces true if all return true.
;; and-pred: ((listof Any) -> Bool) (listof Any) -> Bool
;; Requires
;; Examples
(check-expect (and-pred even? empty) true)

(define (and-pred func _list)
  (local
    [
     (define (check-v s)
       (func s))]
    (cond
      [(empty? _list) true]
      [(check-v (first _list)) (and-pred func (rest _list))]
      [else false])))

;; Tests
(check-expect (and-pred string? (list "stupefy")) true)
(check-expect (and-pred odd? (list 5 9 3)) true)
(check-expect (and-pred string? (list 5 "wow")) false)

;; (map2argfn funcs _list) produces a list of the results from applying the list of funcs
;;     to the list of numbers _list.
;; map2argfn: (listof (Num Num -> Any)) (listof Num) -> (listof Any)
;; Requires: Functions in funcs must take two numbers as arguments.
;;           _list must contain two numbers.
;; Examples
(check-expect (map2argfn (list + - * / list) '(3 2)) '(5 1 6 1.5 (3 2)))

(define (map2argfn funcs _list)
  
  (cond
    [(empty? funcs) empty]
    [else
     (local
       [
        (define toapply (first funcs))
        (define result (toapply (first _list) (second _list)))]
       (list* result (map2argfn (rest funcs) _list)))]))

;; Tests
(check-expect (map2argfn (list expt - * / list) '(3 2)) '(9 1 6 1.5 (3 2)))
(check-expect (map2argfn (list = - * / list) '(3 2)) '(#false 1 6 1.5 (3 2)))


;; An ASExp is one of:
;; * a Num
;; * a Sym (compliant with Racket rules for identifier)
;; * (cons Sym ASExplist) (where Sym is either ’+ or ’*)

;; An ASExplist is one of:
;; * empty
;; * (cons ASExp ASExplist)

;; Association List (AL) is one of
;; * empty
;; * (cons (list Sym Num) AL)
;; All Symbols are unique.


;; (evaluate exp _list) computes the value of exp using _list as a dictionary
;;     for the variables in the exp.
;; evaluate: ASExp AL -> Num
;; Examples
(check-expect (evaluate '(+ x 4) '((x 5) (y 7))) 9)

(define (evaluate exp _list)
  (local
    [
     ;; (remap-express exp _list) produces a re-mapped version of
     ;;     the variables in exp with their couterparts in _list.
     ;; remap-express: ASExp AL -> (listof Sym Num ... Num)
     (define (remap-express exp _list)
       (cond
         [(empty? exp) empty]
         [(symbol? (first exp))
          (cond
            [(symbol=? (first exp) '*) (list* '* (remap-express (rest exp) _list))]
            [(symbol=? (first exp) '+) (list* '+ (remap-express (rest exp) _list))]
            [else (list* (getvar (first exp) _list) (remap-express (rest exp) _list))])]
         [else (list* (first exp) (remap-express (rest exp) _list))]))

     ;; (getvar var _list) produces the variable with symbol var in the AL _list
     ;; getvar: Sym AL -> Num)
     ;; Requires: var must be in _list
     (define (getvar var _list)
       (cond
         [(symbol=? var (first (first _list))) (first (rest (first _list)))]
         [else (getvar var (rest _list))]))

     ;; (_apply _list func) produces the result of the list _list having all elements
     ;;   of _list with function func applied to them.
     ;; _apply: (listof Num) (Num ... Num -> Num) -> Num
     (define (_apply _list func)
       (local
         [(define (getfunc fnc)
            (cond
              [(symbol=? '* fnc) *]
              [(symbol=? '+ fnc) +]))
          (define toapply (getfunc func))]
         (cond
           [(empty? _list)
            (cond
              [(symbol=? func '*) 1]
              [(symbol=? func '+) 0])]
           [else (toapply (first _list) (_apply (rest _list) func))])))

     ;; remapped is the DRY of the remapped values.
     (define remapped (remap-express exp _list))
     ]
    (_apply (rest remapped) (first remapped))))

;; Tests

(check-expect (evaluate '(+ x 0) '((x 1))) 1)
(check-expect (evaluate '(* y 4) '((x 5) (y 7))) 28)
(check-expect (evaluate '(+ 1 2) empty) 3)

;; (arranged _list oper) produces true if the list of oper 
;; arranged?: (list(Any -> Bool) (X X -> Bool)) (listof Any) -> Bool