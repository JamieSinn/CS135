;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname inventory) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ---------------------------
;;  James Sinn (20654551)
;;  CS 135 Fall 2016
;;  Assignment 7. 
;; ---------------------------
;;

(define-struct invnode (name count left right))
;; An InventoryTree is one of:
;; * empty
;; * (make-invnode Str Nat InventoryTree InventoryTree)
;; requires: All names in the left subtree are string<? name
;; All names in the right subtree are string>? name

;; Testing Constants


;; (item-count name tree) produces the amount of a given item in a given InventoryTree
;; item-count: Str InventoryTree -> Nat
;; Examples
(check-expect (item-count "Apple" (make-invnode "Apple" 2 empty empty)) 2)


(define (item-count name tree)
  (cond
    [(empty? tree) 0]
    [(not (string=? name (invnode-name tree))) (+ (item-count name (invnode-left tree))
                                                  (item-count name (invnode-right tree)))]
    [else (+ (invnode-count tree)
             (item-count name (invnode-left tree))
             (item-count name (invnode-right tree)))]))

;; Tests
(check-expect (item-count "Apple" (make-invnode "Apple" 2
                                                (make-invnode "Apple" 2
                                                              (make-invnode "Pear" 2 empty empty)empty) empty)) 4)
(check-expect (item-count "Chocolate" (make-invnode "Apple" 2 empty empty)) 0)
(check-expect (item-count "Chocolate" empty) 0)


;; (add-item name amount tree) produces an updated InventoryTree with the item given added to the tree.
;;    If the tree does not contain the item, then it's added into the correct slot.
;; add-item: Str Nat InventoryTree -> InventoryTree
;; Examples
(check-expect (add-item "Apple" 1 (make-invnode "Apple" 2 empty empty)) (make-invnode "Apple" 3 empty empty))

(define (add-item name count tree)
  (cond
    [(empty? tree) (make-invnode name count empty empty)]
    [(string=? name (invnode-name tree))
     (make-invnode name
                   (+ (invnode-count tree) count)
                   (invnode-left tree) (invnode-right tree))]
    ;; Left
    [(string<? name (invnode-name tree))
     (make-invnode (invnode-name tree)
                   (invnode-count tree)
                   (add-item name count (invnode-left tree))
                   (invnode-left tree))]
    ;; Right
    [else
     (make-invnode (invnode-name tree)
                   (invnode-count tree)
                   (invnode-left tree)
                   (add-item name count (invnode-right tree)))]))


;; Tests
(check-expect (add-item "Flour" 3 (make-invnode "Apple" 2 empty empty)) (make-invnode "Apple" 2 empty (make-invnode "Flour" 3 empty empty)))
(check-expect (add-item "Appl" 3 (make-invnode "Apple" 2 empty empty)) (make-invnode "Apple" 2 (make-invnode "Appl" 3 empty empty) empty))

;; (use-item name amount tree) produces a tree with the amount of the given item subtracted from it. If it
;;    does not contain the item, it returns the input tree. 
;; use-item: Str Nat InventoryTree -> InventoryTree
;; Requires: amount must be a positive integer.
;; Examples
(check-expect (use-item "Apple" 2 (make-invnode "Apple" 3 empty empty)) (make-invnode "Apple" 1 empty empty))

(define (use-item name amount tree)
  (cond
    [(empty? tree) tree]
    [(string=? name (invnode-name tree))
     (make-invnode name (- (invnode-count tree) amount) (invnode-left tree) (invnode-right tree))]
    [(string<? name (invnode-name tree))
     (make-invnode (invnode-name tree)
                   (invnode-count tree)
                   (invnode-left tree)
                   (use-item name amount (invnode-right tree)))]
    [(string>? name (invnode-name tree))
     (make-invnode (invnode-name tree)
                   (invnode-count tree)
                   (use-item name amount (invnode-left tree))
                   (invnode-right tree))]))

;; Tests
(check-expect (use-item "Turkey" 1 (make-invnode "Apple" 3 empty empty)) (make-invnode "Apple" 3 empty empty))
(check-expect (use-item "A" 1 (make-invnode "Apple" 2 (make-invnode "Chicken" 1 empty empty) (make-invnode "A" 1 empty empty)))
              (make-invnode "Apple" 2 (make-invnode "Chicken" 1 empty empty) (make-invnode "A" 0 empty empty)))

;; (out-of tree) produces a list of items that have count 0.
;; out-of: InventoryTree -> (listof Str)
;; Examples
(check-expect (out-of (make-invnode "Flour" 1 (make-invnode "Apple" 0 empty empty) (make-invnode "Napkin" 0 empty empty)))
              (list "Apple" "Napkin"))

(define (out-of tree)
  (strip-out (out-of-main tree)))

;; Tests
(check-expect (out-of (make-invnode "Flour" 1 (make-invnode "Apple" 0 (make-invnode "Apple" 1 empty empty) (make-invnode "Apple" 3 empty empty)) (make-invnode "Napkin" 0 empty empty)))
              (list "Apple" "Napkin"))

;; (out-of-main tree) produces a nested list of all the nodes in the list that have count 0.
;; out-of-main: InventoryTree -> (listof (listof (anyof Str Null)) 
(define (out-of-main tree)
  (cond
    [(empty? tree) empty]
    [(= 0 (invnode-count tree)) (append (list (invnode-name tree)) (list (out-of-main (invnode-left tree))) (list (out-of-main (invnode-right tree))))]
    [(not (= 0 (invnode-count tree))) (append (list (out-of-main (invnode-left tree))) (list (out-of-main (invnode-right tree))))]))


;; (strip-out _list) produces a list of the first elements of the list of nested lists.
;; strip-out: (listof (listof Any)) -> (listof Any)
;; Examples
(check-expect (strip-out (list (list "Apple" '() '()) (list "Napkin" '() '()))) (list "Apple" "Napkin"))
(define (strip-out _list)
  (cond
    [(empty? _list) empty]
    [else (append (list (first (first _list))) (strip-out (rest _list)))]))


;; A Recipe is a (listof (list Str Int)) where each sublist must have a unique string name.
;; Requires Str and Int to be not empty.

;; (can-make? recipe tree) produces a boolean relating to whether the given tree contains enough of the
;;    required ingredients specified by the recipe.
;; can-make?:
;; Examples
(check-expect (can-make? (list (list "Apple" 2)) (make-invnode "Apple" 3 empty empty)) true)

(define (can-make? recipe tree)
  (cond
    [(empty? recipe) true]
    [(empty? tree) false]
    [(string=? (first (first recipe)) (invnode-name tree))
     (cond
       [(>= (invnode-count tree) (first (rest (first recipe))))
        (or (can-make? (rest recipe) (invnode-left tree)) (can-make? (rest recipe) (invnode-right tree)))]
       [else false])]
    [else (or (can-make? recipe (invnode-left tree)) (can-make? recipe (invnode-right tree)))]))

;; Tests
(check-expect (can-make? (list (list "Flour" 2)) (make-invnode "Flour" 1 empty empty))
              false)
(check-expect (can-make? (list (list "Apple" 2)) (make-invnode "Flour" 1
                                                               (make-invnode "Apple1" 0
                                                                             (make-invnode "Apple2" 1 empty empty)
                                                                             (make-invnode "Apple" 3 empty empty))
                                                               (make-invnode "Napkin" 0 empty empty)))
              true)
(check-expect (can-make? (list (list "Apple" 2) (list "Napkin" 2) (list "Flour" 2))
                         (make-invnode "Flour" 1 empty (make-invnode "Napkin" 3 empty (make-invnode "Apple" 3 empty empty))))
              false)