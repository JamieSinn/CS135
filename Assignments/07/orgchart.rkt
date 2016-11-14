;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname orgchart) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct orgnode (name employees))
;; An OrgChart is a (make-orgnode Str (listof OrgChart))
;; requires: name is unique
(define my-orgchart (make-orgnode "Jane"
                                  (list (make-orgnode "Bob"
                                                      (list (make-orgnode "Alice" empty)
                                                            (make-orgnode "Greg" empty)))
                                        (make-orgnode "Jim" empty)
                                        (make-orgnode "Eric"
                                                      (list (make-orgnode "Eric2" empty))))))


;; (responsible-count name chart) produces the number of employees beneath the given employee.
;;    If the employee does not exist, then 0 is returned.
;; responsible-count: Str OrgChart -> Nat
;; Examples
(check-expect (responsible-count "Jane" my-orgchart) 6)
(check-expect (responsible-count "Bob" my-orgchart) 2)


(define (responsible-count name chart)
  (cond
    [(string=? name (orgnode-name chart)) (responsible-count-list (orgnode-employees chart))]
    [else (match-name name (orgnode-employees chart))]))

;; Tests
(check-expect (responsible-count "Timmie" my-orgchart) 0)
(check-expect (responsible-count "Jim" my-orgchart) 0)
(check-expect (responsible-count "Eric" my-orgchart) 1)
(check-expect (responsible-count "Name" (make-orgnode "Jane" empty)) 0)


;; (match-name name loe) produces the count of employees below a given name.
;; match-name: Str (listof OrgChart) -> Nat
;; Requires: name to be inside the loe
;; Examples
(check-expect (match-name "Jim" (orgnode-employees my-orgchart)) 0)

(define (match-name name loe)
  (cond
    [(empty? loe) 0]
    [(string=? name (orgnode-name (first loe))) (responsible-count-list (orgnode-employees (first loe)))]
    [else (match-name name (rest loe))]))

;; (responsible-count-list loe) produces the number of employees who are in a given OrgChart
;; responsible-count-list: (listof OrgChart) -> Nat
;; Examples
(check-expect (responsible-count-list (orgnode-employees my-orgchart)) 6)
(define (responsible-count-list loe)
  (cond
    [(empty? loe) 0]
    [(not (empty? (orgnode-employees (first loe)))) (+ 1 (responsible-count-list (orgnode-employees (first loe)))
                                                       (responsible-count-list (rest loe)))]
    [else (+ 1 (responsible-count-list (rest loe)))]))

;; Tests
(check-expect (responsible-count-list empty) 0)