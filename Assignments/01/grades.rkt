;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname grades) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define asgn-noex 0.3125)
(define mid1-noex 0.2125)
(define mid2-noex 0.3125)
(define part-noex 0.1625)
(define pre-weight 0.55)
(define fweight 0.45)

(define (cs135-grade-sofar mid1 mid2 part assign)
  (+ (* asgn-noex assign)
     (* mid1-noex mid1)
     (* mid2-noex mid2)
     (* part-noex part)))

(define (cs135-final-exam pre final)
  (/ (- final (* pre-weight pre)) fweight))