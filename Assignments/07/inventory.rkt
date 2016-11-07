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
;; add-item: Str Nat InventoryTree -> InventoryTree
;; Examples
(check-expect (add-item "Apple" 1 (make-invnode "Apple" 2 empty empty)) (make-invnode "Apple" 3 empty empty))

(define (add-item name count tree)
  (cond
    [(empty? tree) (make-invnode name count empty empty)]
    [(string=? name (invnode-name tree)) (make-invnode name
                                                       (+ (invnode-count tree) count)
                                                       (invnode-left tree) (invnode-right tree))]
    ;; Right
    [(string<? name (invnode-name tree)) ]
    ;; Left
    [(string>? name (invnode-name tree)) ]))
  