;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname grades) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (cs135-grade-sofar mid1 mid2 part assign)
  (+ (* 0.3125 assign) ( * 0.2125 mid1) (* 0.3125 mid2) (* 0.1625 part)))

(define (cs135-final-exam pre final)
  (/(- final (* 0.55 pre))  0.45 ))