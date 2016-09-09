;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname conversion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define mile 1609.344)
(define fathom 1.8288)
(define smoot 1.7018)
(define fathomperm 0.546807)
(define nc 3.15576)
(define (m/s->mph ms)
  (/ ms (/ mile 60 60)))

(define (mph->m/s mph)
  (* mph (/ mile 60 60)))

(define (fph->m/s fph)
  (* fph (/ fathom 60 60)))

(define (fpf->mph fpf)
  (m/s->mph (fph->m/s (/ fpf 14 24))))

(define (mph->S/nc mph)
  (/ (* (mph->m/s mph) nc) smoot))