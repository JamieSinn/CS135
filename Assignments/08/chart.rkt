;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname chart) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct orgnode (name employees))
;; An OrgChart is a (make-orgnode Str (listof OrgChart))
;; requires: name is unique(define my-orgchart (make-orgnode "Jane"
(define my-orglist (list (make-orgnode "Bob"
                    (list (make-orgnode "Alice" empty)
                          (make-orgnode "Greg" empty)))
      (make-orgnode "Jim" empty)
      (make-orgnode "Eric" empty)))