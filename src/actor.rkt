#lang racket
(require
  (prefix-in ct: charterm)
  (prefix-in lux: lux)
  (prefix-in raart: raart)
  raart)
  
(provide actor-location actor-color actor-id actor-place actor-lives actor-send actor-update create-actors actor-mailbox actor message Place Place-x
         Place-y vactor? location? mailbox? vmessage? near-collide?)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;structures                                                                                           ;
(struct Place (x y))                                                                                  ;
(struct message (sender params))                                                                      ;
(struct actor (id color place lives score mailbox) #:transparent)                                     ;
;definitions & contrats                                                                               ;
(define vactor? actor?)                                                                               ;
(define mailbox? list?)                                                                               ;
(define vmessage? message?)                                                                           ;
(define location? Place?)                                                                             ;
;takes msg : struct message and returns if move?                                                      ;
(define (mov? msg)                                                                                    ;
  (eq? (car (message-params msg)) 'move))                                                             ;
(define (collide? msg)                                                                                ;
  (eq? (car (message-params msg)) 'collide))                                                          ;
(define (fire? msg)                                                                                   ;
  (eq? (car (message-params msg)) 'fire))                                                             ;
;tells if two actors are in same position or about to collide					      ;
(define (near-collide? act1 act2)                                                                     ;
  (or (and (= (add1 (Place-x (actor-location act1))) (Place-x (actor-location act2)))		      ;
           (= (Place-y (actor-location act1)) (Place-y (actor-location act2))))                       ;
      (and (= (Place-x (actor-location act1)) (Place-x (actor-location act2)))		              ;
           (= (Place-y (actor-location act1)) (Place-y (actor-location act2))))))		      ;
                                                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;define location of actor
(define (actor-location act)
  (actor-place act))

;; Send a message to an actor              - (actor*msg) -> actor
(define (actor-send act msg)
  (struct-copy actor act [mailbox (cons msg (actor-mailbox act))]))


;act is an actor, it updates the position of actor with the car of his mailbox. returns new place
(define (update-actor-location act)
  (Place (+ (cadr (message-params (car (actor-mailbox act)))) (Place-x (actor-place act))) (+ (caddr (message-params (car (actor-mailbox act)))) (Place-y (actor-place act)))))

;;returns list of new created actors
(define (create-actors act)
  (list (actor "Fire" 'none (Place (+ (Place-x (actor-location act)) 1) (Place-y (actor-location act))) 1 0 '())))

;moves actor by (x y)
;actor x y ->actor
(define (move-actor act x y)
  (struct-copy actor act [place (Place (+ (Place-x (actor-location act)) x) (+ (Place-y (actor-location act)) y))]))

;removes car of mailbox for an actor
;;return actor
(define (rm-car-mailbox act)
  (struct-copy actor act [mailbox (cdr (actor-mailbox act))]))

(define (sub-life act)
  (if (<= (actor-lives act) 1)
      '()
      (list (struct-copy actor act [lives (sub1 (actor-lives act))] [mailbox (cdr (actor-mailbox act))]))))
;updates an actor
;returns a list
(define (actor-update-elem act)
  (cond
    [(empty? (actor-mailbox act)) (list act)]
    [(fire? (car (actor-mailbox act))) (cons (rm-car-mailbox act) (create-actors act))]  
    [(mov? (car (actor-mailbox act))) (actor-update-elem (struct-copy actor act [place (update-actor-location act)] [mailbox (cdr (actor-mailbox act))]))]
    [(collide? (car (actor-mailbox act))) (sub-life act)]
    [else (actor-update-elem (rm-car-mailbox act))]))
(define (actor-update list-actors);
  (cond
    [(null? (cdr list-actors)) (actor-update-elem (car list-actors))]
    [else (append (actor-update-elem (car list-actors)) (actor-update (cdr list-actors)))]))



;;testing
;(define a6 (actor "(Ծ‸ Ծ 'black)" 'black (Place 30 64) 3 0 '()))
;(car (actor-update (list a6)))
;a6
;(sub-life (actor "Fire" 'none (Place 0 0) 0 0 '()))
;;;;;;;;;;;;;test;;;;;;;;;;;
;(define a0 (actor "momo" (Place 0 0) 1 0 '()))
;(define f0 (car (create-actors a0)))
;(define player (list a0 f0))
;(define f1 (cadr (actor-update player)))
;(Place-x (actor-location f1))
;(Place-y (actor-location f1))
;(define f2 (car (actor-update-elem f1)))
;(Place-x (actor-location f2))
;(Place-y (actor-location f2))
;(string-ci=? (actor-id f2) "Fire")
;(define updated (struct-copy actor f2 [place (Place (+ (Place-x (actor-location f2)) 2) (Place-y (actor-location f2)))]))
;(Place-x (actor-location updated))
;(Place-y (actor-location updated))