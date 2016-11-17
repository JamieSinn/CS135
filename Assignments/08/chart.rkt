;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname chart) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct orgnode (name employees))
;; An OrgChart is a (make-orgnode Str (listof OrgChart))
;; requires: name is unique(define my-orgchart (make-orgnode "Jane"
(define my-orglist (make-orgnode "Jane"
                                 (list (make-orgnode "Bob"
                                                     (list (make-orgnode "Alice" empty)
                                                           (make-orgnode "Greg" empty)))
                                       (make-orgnode "Jim" empty)
                                       (make-orgnode "Eric" empty))))


;; (supervised-employees name chart) produces a list of employee names that
;;    the given person specified by name directly supervises. If name is not
;;    found then false is returned.
;; supervised-employees: Str OrgChart -> (anyof Bool (listof Str))
;; Examples
(check-expect (supervised-employees "Bob" my-orglist) (list "Alice" "Greg"))

(define (supervised-employees name chart)
  (local
    [
     ;; (getemployee name loe) produces the orgnode of the given employee name.
     ;;    If the given name does not exist in loe, false is returned.
     ;; getemployee: Str (listof OrgChart) -> (anyof Bool OrgChart)
     (define (getemployee name loe)
       (cond
         [(empty? loe) false]
         [(string=? name (orgnode-name (first loe))) (first loe)]
         [else (getemployee name (rest loe))]))
     
     ;; (loe-los loe) converts the list of employees given to be a list of the
     ;;    first layer of employees in the list.
     ;; loe->los: (listof OrgChart) -> (listof Str)
     (define (loe->los loe)
       (cond
         [(empty? loe) empty]
         [else (list* (orgnode-name (first loe)) (loe->los (rest loe)))]))
     (define employeenode (getemployee name (orgnode-employees chart)))]
    (cond
      [(string=? name (orgnode-name chart)) (loe->los (orgnode-employees chart))] 
      [(false? employeenode) false]
      [else (loe->los (orgnode-employees employeenode))])))

;; Tests
(check-expect (supervised-employees "Tim" my-orglist) false)
(check-expect (supervised-employees "Eric" my-orglist) empty)
(check-expect (supervised-employees "Jane" my-orglist) (list "Bob" "Jim" "Eric"))
(check-expect (supervised-employees "Arthur" (make-orgnode "Arthur" (list (make-orgnode "Perkins" empty)))) (list "Perkins"))


;; (bottom-rung chart) produces a list of employees who do not supervise anyone.
;; bottom-rung: OrgChart -> (listof Str)
;; Examples
(check-expect (bottom-rung my-orglist) (list "Jim" "Eric" "Alice" "Greg"))

(define (bottom-rung chart)
  (local
    [
     ;; (isbottomrung? node) produces whether or not the given node has
     ;;    employees beneath them.
     ;; isbottomrung?: OrgChart -> Bool
     (define (isbottomrung? node)
       (empty? (orgnode-employees node)))
     
     ;; (loe-los loe) converts the list of employees given to be a list of the
     ;;    first layer of employees in the list.
     ;; loe->los: (listof OrgChart) -> (listof Str)
     (define (loe->los loe)
       (cond
         [(empty? loe) empty]
         [else (list* (orgnode-name (first loe)) (loe->los (rest loe)))]))
     
     ;; (flatter loe) produces a flattened version of the list of employees given with only
     ;;    those who have no sub-employees.
     ;; flatter: (listof OrgChart) -> (listof OrgChart)
     (define (flatter loe)
       (cond
         [(empty? loe) empty]
         [(orgnode? (first loe))
          (cond
            [(isbottomrung? (first loe)) (list* (first loe) (flatter (rest loe)))]
            [else (list* (flatter (rest loe)) (flatter (orgnode-employees (first loe))))])]
         [else (append (flatter (first loe))
                       (flatter (rest loe)))]))]
    (cond
      [(isbottomrung? chart) (list (orgnode-name chart))]
      [else (loe->los (flatter (flatter (orgnode-employees chart))))])))

;; Tests
(check-expect (bottom-rung (make-orgnode "Jim" empty)) (list "Jim"))
(check-expect (bottom-rung (make-orgnode "Percy" empty)) (list "Percy"))


;; (conflict? name1 name2 chart) produces true if the two names meet any of the conditions below.
;;    Have the same supervisor
;;    One is responsible for the other.
;;    Otherwise, if produces false.
;; conflict?: Str Str OrgChart -> Bool
;; Examples
(check-expect (conflict? "Jane" "Eric" my-orglist) true)

(define (conflict? name1 name2 chart)
  (local
    [
     ;; (containsemployee name loe) produces the true if the given name is in loe.
     ;;    If the given name does not exist in loe, false is returned.
     ;; containsemployee: Str (listof OrgChart) -> Bool
     (define (containsemployee? name loe)
       (cond
         [(empty? loe) false]
         [(string=? name (orgnode-name (first loe))) true]
         [else (containsemployee? name (rest loe))]))
     
     ;; (getemployee name loe) produces the orgnode of the given employee name.
     ;;    If the given name does not exist in loe, false is returned.
     ;; getemployee: Str (listof OrgChart) -> (anyof Bool OrgChart)
     (define (getemployee name loe)
       (cond
         [(empty? loe) false]
         [(string=? name (orgnode-name (first loe))) (first loe)]
         [else (getemployee name (rest loe))]))
     ]
    (cond
      [(string=? name1 (orgnode-name chart)) (containsemployee? name2 (orgnode-employees chart))]
      [(string=? name2 (orgnode-name chart)) (containsemployee? name1 (orgnode-employees chart))]
      [(not (false? (getemployee name2 (orgnode-employees chart)))) (containsemployee? name1 (orgnode-employees (getemployee name2 (orgnode-employees chart))))]
      [(not (false? (getemployee name1 (orgnode-employees chart)))) (containsemployee? name2 (orgnode-employees (getemployee name1 (orgnode-employees chart))))]
      [else false])))

;; Tests