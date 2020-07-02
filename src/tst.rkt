#lang racket

(require rackunit)
(require rackunit/text-ui)
(require "contract.rkt")


(define all-tests
  (test-suite
   "Tests file for actor project"
   (test-case
    "actor position and update position"
    (let* ([a0 (actor "momo" 'blue (Place 0 0) 1 0 '())]
           [a1 (actor-send a0 (message "server" '(move 1 0)))]
           [a2 (actor-send a1 (message "server" '(move 0 1)))]
           [a3 (car (actor-update (list a2)))])
        ;(check-equal? (Place-x (actor-location a0)) 0)
      ;(check-equal? (Place-y (actor-location a0)) 0)
      ;(check-equal? (length (actor-mailbox a0)) 0)
      ;(check-equal? (length (actor-mailbox a2)) 2)
      (check-equal? (Place-x (actor-location a3)) 1)
      (check-equal? (Place-y (actor-location a3)) 1)))
   (test-case
    "checking near-collide?"
    (let* ([a0 (actor "momo" 'blue (Place 2 0) 1 0 '())]
           [me (actor "momo" 'blue (Place 0 0) 1 0 '())]
           [a1 (actor "momo" 'blue (Place 2 0) 1 0 '())]
           [a2 (actor "momo" 'blue (Place 1 0) 1 0 '())]
           )
        (check-equal? (near-collide? a0 me) #f)
      ;(check-equal? (near-collide? a0 a1) #t)
      (check-equal? (near-collide? a2 a0) #t)
      )))
;   (test-case
;    "send-msg-up"
;    (let* ([a0 (actor "momo" 'none (Place 0 3) 1 0 '())]
;           [a1 (actor "m" 'none (Place 0 0) 1 0 '())]
;           [a2 (actor "argd" 'none (Place 10 0) 1 0 '())]
;           [a3 (actor "adg" 'none (Place 10 0) 1 0 '())]
;           [w0 (world 0 10.0 (list a0 a1) (list a2 a3) '() '() #f)]
;           [w1 (send-msg-up w0)]
;           [af (actor-update (list (car (world-player w1))))])
;      (check-equal? (Place-x (actor-location (car af))) 0)
;      (check-equal? (Place-y (actor-location (car af))) 2)
;      ))
;   (test-case
;    "send-msg-left"
;    (let* ([a0 (actor "momo" 'none (Place 0 0) 1 0 '())]
;           [a1 (actor "m" 'none (Place 0 0) 1 0 '())]
;           [a2 (actor "argd" 'none (Place 10 0) 1 0 '())]
;           [a3 (actor "adg" 'none (Place 10 0) 1 0 '())]
;           [w0 (world 0 10.0 (list a0 a1) (list a2 a3) '() '() #f)]
;           [w1 (send-msg-left w0)]
;           [af (actor-update (list (car (world-player w1))))])
;      (check-equal? (Place-x (actor-location (car af))) -1)
;      (check-equal? (Place-y (actor-location (car af))) 0)
;      ))
;   (test-case
;    "send-msg-down"
;    (let* ([a0 (actor "momo" 'none (Place 0 5) 1 0 '())]
;           [a1 (actor "m" 'none (Place 0 0) 1 0 '())]
;           [a2 (actor "argd" 'none (Place 10 0) 1 0 '())]
;           [a3 (actor "adg" 'none (Place 10 0) 1 0 '())]
;           [w0 (world 0 10.0 (list a0 a1) (list a2 a3) '() '() #f)]
;           [w1 (send-msg-down w0)]
;           [af (actor-update (list (car (world-player w1))))])
;      (check-equal? (Place-x (actor-location (car af))) 0)
;      (check-equal? (Place-y (actor-location (car af))) 6)
;      ))
;   (test-case
;    "send-msg-right"
;    (let* ([a0 (actor "momo" 'none (Place 0 0) 1 0 '())]
;           [a1 (actor "m" 'none (Place 0 0) 1 0 '())]
;           [a2 (actor "argd" 'none (Place 10 0) 1 0 '())]
;           [a3 (actor "adg" 'none (Place 10 0) 1 0 '())]
;           [w0 (world 0 10.0 (list a0 a1) (list a2 a3) '() '() #f)]
;           [w1 (send-msg-right w0)]
;           [af (actor-update (list (car (world-player w1))))])
;      (check-equal? (Place-x (actor-location (car af))) 1)
;      (check-equal? (Place-y (actor-location (car af))) 0)))))
)
(run-tests all-tests)