;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname clinic) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ---------------------------
;;  James Sinn (20654551)
;;  CS 135 Fall 2016
;;  Assignment 6. 
;; ---------------------------
;;

;; A PatientProfile is a (list Str Nat Num)
;; A DoctorSchedule is a (list Str (listof PatientProfile))
;; A ClinicSchedule is a non empty list of DoctorSchedule.

;; Testing Values
(define PP (list "Name" 12 100))
(define PP2 (list "NAME2" 12 101))

(define DS (list "Dr. Null" (list PP PP PP PP)))
(define DS2 (list "Dr. NaN" (list PP PP PP)))
(define DS3 (list "Dr. 1/0" (list PP PP)))
(define DS3_2 (list "Dr. 1/0" (list PP PP PP2)))
(define DS3_3 (list "Dr. 1/0" (list PP PP PP)))

(define CS_U (list DS DS2 DS3))
(define CS_S (list DS3 DS2 DS))
(define CS_S2 (list DS3_2 DS2 DS))
(define CS_S3 (list DS3_3 DS2 DS))
(define CS_M1 (list DS3 DS3 DS2 DS2 DS DS))

;; (sort-schedule schedules) produces a sorted ClinicSchedule out of the list
;;     of DoctorSchedules
;; sort-schedule: (listof DoctorSchedule) -> ClinicSchedule
;; Examples
(check-expect (sort-schedule CS_U) CS_S)

(define (sort-schedule schedules)
  (cond
    [(empty? schedules) empty]
    [else (insert-schedule (first schedules) (sort-schedule (rest schedules)))]))


;; (insert-schedule s los) inserts schedule s into the list of schedules los
;;    so that the resulting list is also sorted.
;; insert-schedule: (listof DoctorSchedule) -> (listof DoctorSchedule)
;; Examples
(check-expect (insert-schedule DS2 (list DS3 DS)) (list DS3 DS2 DS))
(define (insert-schedule s los)
  (cond
    [(empty? los) (list* s empty)]
    [(<= (length (second s)) (length (second (first los)))) (list* s los)]
    [else (list* (first los) (insert-schedule s (rest los)))]))

;; (insert-patient-schedule clinic patient) produces a resorted ClinicSchedule after
;;    inserting the patientprofile into the doctor's schedule with the least patients.
;; insert-patient-schedule: ClinicSchedule PatientProfile -> ClinicSchedule
;; Examples
(check-expect (insert-patient-schedule CS_S PP2) CS_S2)

(define (insert-patient-schedule clinic patient)
  (sort-schedule (list* (list (first (first clinic)) (append (second (first clinic)) (list patient))) (rest clinic))))

;; Tests
(check-expect (insert-patient-schedule CS_S PP) CS_S3)


;; (merge-schedule clinic1 clinic2) produces a merged ClinicSchedule of the two
;;     Schedules provided.
;; merge-schedule: ClinicSchedule ClinicSchedule -> ClinicSchedule
;; Examples
(check-expect (merge-schedule CS_S CS_S) CS_M1)

(define (merge-schedule clinic1 clinic2)
  (sort-schedule (append clinic1 clinic2)))