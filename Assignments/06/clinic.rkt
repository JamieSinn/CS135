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

;; (sort clinicschedule) sorts the ClinicSchedule in nondecreasing order
;; sort: ClinicSchedule -> ClinicSchedule
;(define (sort clinicschedule)
;  (cond
;    [(empty? clinicschedule) empty]
;    [else (... (first clinicschedule) ... (sort (rest clinicschedule)) ...)]))

;; Testing Values
(define PP (list "Name" 12 100))
(define PP2 (list "NAME2" 12 101))

(define DS (list "Dr. Null" (list PP PP PP PP)))
(define DS2 (list "Dr. NaN" (list PP PP PP)))
(define DS2_2 (list "Dr. NaN" (list PP PP)))
(define DS3 (list "Dr. 1/0" (list PP PP)))
(define DS3_2 (list "Dr. 1/0" (list PP PP PP2)))
(define DS3_3 (list "Dr. 1/0" (list PP PP PP)))
(define DS3_4 (list "Dr. 1/0" (list PP)))

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
;; Tests
(check-expect (sort-schedule (list (list "Victoria" (list (list "Hermione" 18 50)))))
              (list (list "Victoria" (list (list "Hermione" 18 50)))))

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
  (sort-schedule (list* (list (first (first clinic))
                              (append (second (first clinic)) (list patient))) (rest clinic))))

;; Tests
(check-expect (insert-patient-schedule CS_S PP) CS_S3)
(check-expect (insert-patient-schedule (list (list "Victoria" (list (list "Hermione" 18 50)))) (list "Minerva" 70 65))
              (list (list "Victoria" (list (list "Hermione" 18 50) (list "Minerva" 70 65)))))

;; (merge-schedule clinic1 clinic2) produces a merged ClinicSchedule of the two
;;     Schedules provided.
;; merge-schedule: ClinicSchedule ClinicSchedule -> ClinicSchedule
;; Examples
(check-expect (merge-schedule CS_S CS_S) CS_M1)

(define (merge-schedule clinic1 clinic2)
  (sort-schedule (append clinic1 clinic2)))

;;Tests
(check-expect (merge-schedule (list (list "d2" empty)) (list (list "d4" (list (list "f" 12 25)))))
              (list (list "d2" empty) (list "d4" (list (list "f" 12 25)))))

;; (patient-served clinic doctor) removes the first patient in the specified doctor's list of patients
;;    from the ClinicSchedule and then returns the updated and resorted ClinicSchedule
;; patient-served: ClinicSchedule Str -> ClinicSchedule
;; Examples
(check-expect (patient-served (list (list "d4" (list (list "f" 12 25)))) "d4") (list (list "d4" empty)))

(define (patient-served clinic doctor)
  (cond
    [(empty? clinic) empty]
    [(string=? doctor (first (first clinic))) (sort-schedule (list* (remove-patient (first clinic)) (remove-dschedule clinic doctor)))]
    [else (list* (first clinic) (patient-served (rest clinic) doctor))]))

;; Tests
(check-expect (patient-served empty "d1") empty)
(check-expect (patient-served CS_S "Dr. 1/0") (sort-schedule (list DS3_4 DS2 DS)))
(check-expect (patient-served CS_S "Dr. NaN") (sort-schedule (list DS3 DS2_2 DS)))

;; (remove-patient dschedule) removes the first patient from the given DoctorSchedule
;; remove-patient: DoctorSchedule -> DoctorSchedule
;; Examples
(check-expect (remove-patient DS3_2) (list "Dr. 1/0" (list PP PP2)))

(define (remove-patient dschedule)
  (list (first dschedule) (rest (first (rest dschedule)))))

;; (replace-dschedule clinic doctor) removes a given doctor's schedule from the given ClinicSchedule
;; replace-dschedule: ClinicSchedule Str -> ClinicSchedule
;; Examples
(check-expect (remove-dschedule CS_S "Dr. NaN") (list DS3 DS))

(define (remove-dschedule clinic doctor)
  (cond
    [(empty? clinic) empty]
    [(not (string=? doctor (first (first clinic)))) (list* (first clinic) (remove-dschedule (rest clinic) doctor))]
    [else (list* (remove-dschedule (rest clinic) doctor))]))