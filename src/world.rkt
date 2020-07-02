#lang racket
(require
  (prefix-in ct: charterm)
  (prefix-in lux: lux)
  (prefix-in raart: raart)
  raart "actor.rkt")

(provide world vworld? send-msg-left send-msg-right send-msg-down send-msg-up world-player)

;show primary actor
;;act is an actor
(define (show-primary act)
  (raart:matte-at term-cols term-rows
                  (Place-x (actor-location act))
                  (Place-y (actor-location act))
                  (raart:bg 'red (raart:text ">-"))))

;makes raart out of list of adversary actors
;;init-rart is the initial raart that would be updated with list of actors on it
;; list is a list of actors
(define (show-adversaries intit-rart list)
  (cond
    [(null? list) intit-rart]
    [(< (Place-x (actor-location (car list))) 0) (show-adversaries intit-rart (cdr list))]
    [else (show-adversaries (place-at intit-rart (Place-y (actor-location (car list))) (Place-x (actor-location (car list))) (raart:bg (actor-color (car list)) (raart:text (actor-id (car list)))))
                            (cdr list))]))

(define (show-fire intit-rart list)
  (cond
    [(null? list) intit-rart]
    [else (show-fire (place-at intit-rart (Place-y (actor-location (car list))) (add1 (Place-x (actor-location (car list)))) (raart:text "-->"))
                     (cdr list))]))

(define (show-end intit-rart primary-actor)
  (let ([game-over (place-at (matte term-cols term-rows
                                    (frame
                                     (text " GAME OVER ! ")))
                             (+ (modulo (quotient term-rows 2) term-rows) 1) 
                             (- (modulo (quotient term-cols 2) term-cols) 12)       
                             (raart:text " Press 'p' to go back :) "))])
    (cond
      [(zero? (actor-lives primary-actor)) game-over]
      [else intit-rart])))


;; Get the terminal dimensions
(define-values (term-cols term-rows)
  (ct:with-charterm (ct:charterm-screen-size)))


;;;;;;;;;;;;;;;;;;;;;;;;;
;;        WORLD        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;player has become a list of primary-player, and fire-actors!
;;tick is an integer number
;;fps is an integer number
;;player is a list of : (primary-actor fire fire...)
;;list is the list of adversary actors
(struct world (tick fps player list previous-world next-world stop)
        #:methods lux:gen:word
        [(define (word-fps w)      ;; FPS desired rate
           (world-fps w))
         (define (word-label s ft) ;; Window label of the application
           " Our game - Our world ! ")
         (define (word-event w e)  ;; Event Handler
           (match e
             ["a" #f]  ;; Quit the application
	     ["q" (send-msg-left w)]   ;send move msg to left
             ["d" (send-msg-right w)]  ;send move msg right
             ["z" (send-msg-up w)]  ;send move msg up
             ["s" (send-msg-down w)]  ;send move msg down
             ["x" (send-msg-shoot w)] ;send msg to shoot
             ["p" (prev-world w)] ;previous world
             ["n" (nex-world w)] ;next world
             [_ (send-msg-move w)] ;if no event , send move msg to all actors
         ))
         (define (word-output w)      ;; What to display for the application
           (match-define (world tick fps player list previous-world next-world stop) w)
           (show-end (show-fire (show-adversaries (show-primary (car player)) list) (cdr player)) (car player)) ;place at --> life..
         )
         (define (word-tick w)        ;; Update function after one tick of time
           (match-define (world tick fps player list previous-world next-world stop) w)
           (if (or (zero? (actor-lives (car player))) stop)
               w
               (world (+ 1 tick) fps (actor-update (vivify-bullets player list)) (actor-update (runtime player list)) (cons w previous-world) next-world #f))) 
         ])
(define vworld? world?)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;       RUNTIME       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;


;vivifies fire-actors
;;returns new list of primary actor (primary fire fire ..)
(define (vivify-bullets player-actor list-actor-adversary)
  (letrec ([vivify-one-bullet (lambda (adversary-actor act) (cond
                                                              [(not (empty? (filter (lambda (x) (near-collide? act x)) adversary-actor))) (actor-send act (message "server" '(collide)))]
                                                              [else (actor-send act (message "server" '(move 1 0)))]))]
           [vivify-primary (lambda (primary adversary-actors) (cond
                                                                [(not (empty? (filter (lambda (x) (near-collide? primary x)) adversary-actors))) (list (struct-copy actor primary [lives (sub1 (actor-lives primary))]))]
                                                                ;[(zero? (actor-lives primary)) ]
                                                                [else (list primary)]))])
    (append (vivify-primary (car player-actor) list-actor-adversary) (map (lambda (x) (vivify-one-bullet list-actor-adversary x)) (cdr player-actor)))))



(define (move-left list-actors)
  (map (lambda (x) (actor-send x (message "server" '(move -1 0)))) list-actors))

(define (collisions act adversary-actors)
  (filter (lambda (x) (near-collide? act x)) adversary-actors))

(define (alive act adversary-actors)
  (filter (lambda (x) (not (near-collide? act x))) adversary-actors))

;returns adversary-actors
(define (runtime primary-actors list-actors)
    (cond
      [(null? primary-actors) (move-left list-actors)]
      [(not (null? (collisions (car primary-actors) list-actors))) (runtime (cdr primary-actors) (append (map (lambda (x) (actor-send x (message (actor-id (car primary-actors)) '(collide)))) (collisions (car primary-actors) list-actors))
                                                                                                         (alive (car primary-actors) list-actors)))]
      [else (runtime (cdr primary-actors) list-actors)]))
  
;;w is a world
;return an updated world
(define (send-msg-left w)
  (match-define (world tick fps player-actor list-actor previous-world next-world stop) w)
  (match-define new-primary-actor (actor-send (car player-actor) (message "server" '(move -1 0))))
  (match-define new-primary-list (cons new-primary-actor (cdr (vivify-bullets player-actor list-actor))))	
  (world tick fps new-primary-list (runtime new-primary-list list-actor) previous-world next-world #f))

(define (send-msg-right w)
  (match-define (world tick fps player-actor list-actor previous-world next-world stop) w)
  (match-define new-primary-actor (actor-send (car player-actor) (message "server" '(move 1 0))))
  (match-define new-primary-list (cons new-primary-actor (cdr (vivify-bullets player-actor list-actor))))
  (world tick fps new-primary-list (runtime new-primary-list list-actor) previous-world next-world #f))

(define (send-msg-up w)
  (match-define (world tick fps player-actor list-actor previous-world next-world stop) w)
  (match-define new-primary-actor (actor-send (car player-actor) (message "server" '(move 0 -1))))
  (match-define new-primary-list (cons new-primary-actor (cdr (vivify-bullets player-actor list-actor))))
  (world tick fps new-primary-list (runtime new-primary-list list-actor) previous-world next-world #f))

(define (send-msg-down w)
  (match-define (world tick fps player-actor list-actor previous-world next-world stop) w)
  (match-define new-primary-actor (actor-send (car player-actor) (message "server" '(move 0 1))))
  (match-define new-primary-list (cons new-primary-actor (cdr (vivify-bullets player-actor list-actor))))
  (world tick fps new-primary-list (runtime new-primary-list list-actor) previous-world next-world #f))

(define (send-msg-shoot w)
  (match-define (world tick fps player-actor list-actor previous-world next-world stop) w)
  (match-define new-primary-actor (actor-send (car player-actor) (message "server" '(fire))))
  (match-define new-primary-list (cons new-primary-actor (cdr (vivify-bullets player-actor list-actor))))
  (world tick fps new-primary-list (runtime new-primary-list list-actor) previous-world next-world #f))

(define (send-msg-move w)
  (match-define (world tick fps player-actor list-actor previous-world next-world stop) w)
  (match-define new-player (append (list (car player-actor)) (cdr (vivify-bullets player-actor list-actor))))
  (world tick fps new-player (runtime new-player list-actor) previous-world next-world #f))

;;;;;;;;;;;;;Time traveling
(define (prev-world w)
  (match-define (world tick fps player-actor list-actor previous next stop) w)
  (struct-copy world (car previous) [stop #t] [next-world (cons w next)]))

(define (nex-world w)
  (match-define (world tick fps player-actor list-actor previous next stop) w)
  (struct-copy world (car next) [stop #t] [previous-world (cons w previous)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TEST;;;;;;;;;;;;;;;;;;
;(define a0 (actor "momo" (Place 0 0) 1 0 '()))
;(define a1 (actor "momo" (Place 10 0) 1 0 '()))
;(define w0 (world 0 10.0 (list a0) (list a1)))
;(define w1 (send-msg-shoot w0))
;(define af (actor-update (list (cadr (world-player w1)))))
;(Place-x (actor-location (car af)))
;(Place-y (actor-location (car af)))
;;(define a2 (car (actor-update af)))
;(Place-x (actor-location a2))
;(Place-y (actor-location a2))
;(define a3 (car (actor-update (list a2))))
;(Place-x (actor-location a3))
;(Place-y (actor-location a3))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	
;;;;;;;;;;;;;;;;;;;;;;;;testing
;(define a0 (actor "momo" (Place 0 2) 1 0 '()))
;(define a1 (actor "momo" (Place 5 3) 1 0 '()))
;(define a2 (actor "momo" (Place 6 6) 1 0 '()))
;(define inti (show-primary a0))
;(show-adversaries inti (list a2 a1))
;(draw-here (show-adversaries inti (list a2 a1)))
;;;;;;;;;;;;;;;;;;;;;;;;testing