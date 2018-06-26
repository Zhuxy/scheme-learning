(library-directories "..")
(import (modules))

;4.55
;(a)
(supervisor ?x (Ben Bitdiddle))

;(b) 
(job ?x (accounting . ?y))

;(c) 
(address ?x (Slumerville . ?y))

;4.56
;(a) 
(and (supervisor ?person (Ben Bitdiddle))
    (address ?person ?address))

;(b)
(and (salary (Ben Bitdiddle) ?salary)
    (and (salary ?person ?salary_2)
        (lisp-value < ?salary_2 ?salary)))

;(c)
(and 
  (supervisor ?person ?boss)
  (not (job ?boss (computer . ?type)))
  (job ?boss ?job))

;4.57
;(a)
(rule (alternative ?alt ?target)
  (or
    (and (job ?target ?job)
      (job ?alt ?job)
      (not (same ?alt ?target)))
    (and
      (job ?target ?job)
      (can-do-job ?can-do ?job)
      (job ?alt ?can-do)
      (not (same ?alt ?target)))))

;another solution
;(rule (alternative ?alt ?target)
;  (and (job ?alt ?job-a)
;    (job ?target ?job-t)
;    (or (same ?job-a ?job-t)
;      (can-do-job ?job-a ?job-t))
;    (not (same ?alt ?target))))

(alternative ?person (Fect Cy D))

;(b)
(and
  (alternative ?alt ?target)
  (salary ?alt ?salary-a)
  (salary ?target ?salary-t)
  (lisp-value > ?salary-t ?salary-a))

;4.58
(rule (bigname ?person ?dept)
  (and (job ?person (?dept . ?job-title))
    (supervisor ?boss ?person)
    (not (job ?boss (?dept . ?job-titles)))))

;4.59
;(a)
(meeting ?dept (Friday ?time))

;(b)
(rule (meeting-time ?person ?day-and-time)
  (or
    (and 
      (job ?person (?dept . ?rest))
      (meeting ?dept ?day-and-time))
    (meeting whole-company ?day-and-time)))

;(c)
(and
  (meeting-time (Hacker Alyssa P) (Wednesday ?time))
  (meeting ?dept (Wednesday ?time)))

;4.61
(rule (?x next-to ?y in (?x ?y . ?u)))

(rule (?x next-to ?y in (?v . ?z))
  (?x next-to ?y in ?z))

(?x next-to ?y in (1 (2 3) 4))
(1 next-to (2 3) in (1 (2 3) 4))
((2 3) next-to 4 in (1 (2 3) 4))

(?x next-to ?y in (2 1 3 1)
(2 next-to 1 in (2 1 3 1)
(1 next-to 3 in (2 1 3 1)
(3 next-to 1 in (2 1 3 1)

;4.62
(rule (last-pair (?x) (?x)))
(rule (last-pair (?u . ?v) (?x))
  (last-pair ?v (?x)))

(last-pair (3) ?x)
(last-pair (3) (3))

(last-pair (1 2 3) ?x)
(last-pair (1 2 3) (3))

(last-pair (2 ?x) (3))
(last-pair (2 3) (3))

(last-pair ?x (3))
;no answer, 无限解

;4.63






