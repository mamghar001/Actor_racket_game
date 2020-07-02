#lang racket
(require "actor.rkt")
;(require "world.rkt")


(provide (contract-out     
    [actor-location (-> vactor? location?)]
    [actor-send (-> vactor? vmessage? vactor?)]
    [actor-update ( -> list? list?)]
    ))

(provide actor)
(provide create-actors actor-mailbox actor message Place Place-x Place-y near-collide?)
;(provide world)
;(provide world-player)


;(provide (contract-out     
 ;         [send-msg-up (-> vworld? vworld?)]
  ;        [send-msg-left (-> vworld? vworld?)]
   ;       [send-msg-right (-> vworld? vworld?)]
    ;      [send-msg-down (-> vworld? vworld?)]
     ;     ))
