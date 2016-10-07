;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname olympics) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(define-struct medalcount (country gold silver bronze))

;; (higher-rank? country1 country2) produces the country with the placement based on medal count.
;;     If that is tied, then it goes to gold medals, if that is tied, it goes to silver medals. True means country1 is higher, false means country2 is.
;; higher-rank?: MedalCount MedalCount -> Bool
(define (higher-rank? country1 country2)
  (or
   (and (= (+ (medalcount-gold country1) (medalcount-silver country1) (medalcount-bronze country1))
           (+ (medalcount-gold country2) (medalcount-silver country2) (medalcount-bronze country2)))
        (> (medalcount-gold country1) (medalcount-gold country2))
        )
   
   (> (+ (medalcount-gold country1) (medalcount-silver country1) (medalcount-bronze country1))
      (+ (medalcount-gold country2) (medalcount-silver country2) (medalcount-bronze country2))
      )
   (> (medalcount-silver country1) (medalcount-silver country2))
   )
  )
