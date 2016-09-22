;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname functions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Singles, Doubles, Triples, Home Runs, At Bats
(define (batter-slugging-average s d t hr ab)
  (/ (+ s (* 2 d) (* 3 t) (* 4 hr )) ab))

;; 33,000 ftlb's of work per minute. 1 Horsepower.
(define hpc 33000)
(define (horsepower n r rpm lb)
  (* n (/ (* r 2 pi rpm lb) hpc)))

(define (y a h k x)
  (+ (+ (- (* a (sqr x)) (* 2 a h)) (* a (sqr h))) k))

(define (future-value n p r t)
  (* p (expt (+ 1 (/ r n)) (* n t))))

