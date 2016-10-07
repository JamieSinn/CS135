;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname triangle) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct triangle (point1 point2 point3))
;; (triangle-area triangle) produces the area of a triangle given by the structure Triangle which contains 3 posns.
;; triangle-area: Triangle -> Num
(define (triangle-area triangle)
  (cond
    [(or (equal? (triangle-point1 triangle)
                 (triangle-point2 triangle))
         (equal? (triangle-point2 triangle)
                 (triangle-point3 triangle))
         (equal? (triangle-point1 triangle)
                 (triangle-point3 triangle)))
     false]
    [(= (sqrt (* (triangle-s
                  (triangle-point1 triangle)
                  (triangle-point2 triangle)
                  (triangle-point3 triangle))
                 (- (triangle-s
                     (triangle-point1 triangle)
                     (triangle-point2 triangle)
                     (triangle-point3 triangle))
                    (triangle-side (triangle-point1 triangle)
                                   (triangle-point2 triangle)))
                 (- (triangle-s (triangle-point1 triangle)
                                (triangle-point2 triangle)
                                (triangle-point3 triangle))
                    (triangle-side (triangle-point1 triangle)
                                   (triangle-point3 triangle)))
                 (- (triangle-s (triangle-point1 triangle)
                                (triangle-point2 triangle)
                                (triangle-point3 triangle))
                    (triangle-side (triangle-point2 triangle)
                                   (triangle-point3 triangle)))
                 )
              ) 0) false]
    [else
     (sqrt (* (triangle-s (triangle-point1 triangle)
                          (triangle-point2 triangle)
                          (triangle-point3 triangle))
              (- (triangle-s (triangle-point1 triangle)
                             (triangle-point2 triangle)
                             (triangle-point3 triangle))
                 (triangle-side (triangle-point1 triangle)
                                (triangle-point2 triangle)))
              (- (triangle-s (triangle-point1 triangle)
                             (triangle-point2 triangle)
                             (triangle-point3 triangle))
                 (triangle-side (triangle-point1 triangle)
                                (triangle-point3 triangle)))
              (- (triangle-s (triangle-point1 triangle)
                             (triangle-point2 triangle)
                             (triangle-point3 triangle))
                 (triangle-side (triangle-point2 triangle)
                                (triangle-point3 triangle)))
              )
           )]
    
    )
  )
;; (triangle-s point1 point2 point3) produces the sum of the sides of a triangle.
;; triangle-s: Posn Posn Posn -> Num
(define (triangle-s point1 point2 point3)
  (/ (+ (triangle-side point1 point2)
        (triangle-side point1 point3)
        (triangle-side point2 point3)
        )
     2))
;; (triangle-s point1 point2 point3) produces the length of a vector.
;; triangle-s Posn Posn -> Num
(define (triangle-side point1 point2)
  (sqrt (+ (sqr (- (posn-x point1) (posn-x point2)))
           (sqr (- (posn-y point1) (posn-y point2)))
           )
        )
  )
