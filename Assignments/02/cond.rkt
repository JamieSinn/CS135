;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cond) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (q1a x)
  (cond [(and
          (p1? x)
          (p2? x)) 'down]
        [(and
          (p1? x)
          (not (p2? x))) 'up]
        [(and
          (not (p1? x))
          (p2? x)) 'left]
        [else 'right]))

(define (q1b x)
  (cond
    [(p1? x ) 'up]
    [(p2? x) 'down]
    [else 'up]))

(define (q1c x)
  (cond
    [(and (p1? x) (p2? x) (p3? x)) 'down]
    [(and (p1? x) (p2? x) (not (p3? x))) 'up]
    [else 'left]))