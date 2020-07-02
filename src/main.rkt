#lang racket
(require
  (prefix-in ct: charterm)
  (prefix-in lux: lux)
  (prefix-in raart: raart))
(require "actor.rkt" "world.rkt")
(require raart)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                              ; art
;                    ACTORS                    ;
;                                              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;colors: 'yellow 'blue 'red 'green 'black

(define a0 (actor "(ʘ‿ʘ)" 'black (Place 30 3) 1 0 '()))
(define a1 (actor "(ಠ_ಠ)" 'black (Place 50 2) 1 0 '()))
(define a2 (actor "(｡◕_◕｡)" 'black (Place 20 6) 1 0 '()))
(define a3 (actor "( oДo)" 'black (Place 30 6) 1 0 '()))
(define a4 (actor "(ಠ‿ಠ)" 'black (Place 15 4) 1 0 '()))
(define a5 (actor "(♥‿♥)" 'black (Place 50 9) 1 0 '()))
(define a6 (actor "(Ծ‸ Ծ)" 'black (Place 30 64) 1 0 '()))
(define art (list a0 a1 a2 a3 a4 a5 a6))


;                              /\___/\
;                              | o o |
;                             __\_^_/__
;                            (__/   \__)
;                             _|  .  |_
;                            (__\___/__)

;gives a bear at position (x,y)
;;x and y integers such as x > 2
(define (bear x y)
  (define b0 (actor "(" 'black (Place x (+ y 2)) 1 0 '()))
  (define b1 (actor ")" 'black (Place (+ x 1) (+ y 2)) 1 0 '()))
  (define b2 (actor "_" 'black (Place (+ x 2) (+ y 2)) 1 0 '()))
  (define b3 (actor "_" 'black (Place (+ x 3) (+ y 2)) 1 0 '()))
  (define b5 (actor "_" 'black (Place (+ x 4) (+ y 2)) 1 0 '()))
  (define b6 (actor "(" 'black (Place (+ x 5) (+ y 2)) 1 0 '()))
  (define b7 (actor ")" 'black (Place (+ x 6) (+ y 2)) 1 0 '()))
  (define b8 (actor "|" 'black (Place x (+ y 3)) 1 0 '()))
  (define b9 (actor "o" 'black (Place (+ x 2) (+ y 3)) 1 0 '()))
  (define b10 (actor "o" 'black (Place (+ x 4) (+ y 3)) 1 0 '()))
  (define b11 (actor "|" 'black (Place (+ x 6) (+ y 3)) 1 0 '()))
  (define b12 (actor "(" 'black (Place (+ x 1) (+ y 4)) 1 0 '()))
  (define b13 (actor "^" 'black (Place (+ x 3) (+ y 4)) 1 0 '()))
  (define b14 (actor ")" 'black (Place (+ x 5) (+ y 4)) 1 0 '()))
  (define b15 (actor "_" 'black (Place (+ x 2) (+ y 4)) 1 0 '()))
  (define b16 (actor "_" 'black (Place (+ x 4) (+ y 4)) 1 0 '()))
  (define b17 (actor "_" 'black (Place x (+ y 4)) 1 0 '()))
  (define b18 (actor "_" 'black (Place (- x 1) (+ y 4)) 1 0 '()))
  (define b27 (actor "_" 'black (Place (+ x 6) (+ y 4)) 1 0 '()))
  (define b28 (actor "_" 'black (Place (+ x 7) (+ y 4)) 1 0 '()))
  (define b19 (actor "(" 'black (Place (- x 2) (+ y 5)) 1 0 '()))
  (define b20 (actor "_" 'black (Place (- x 1) (+ y 5)) 1 0 '()))
  (define b21 (actor "_" 'black (Place x (+ y 5)) 1 0 '()))
  (define b22 (actor "|" 'black (Place (+ x 1) (+ y 5)) 1 0 '()))
  (define b23 (actor "|" 'black (Place (+ x 5) (+ y 5)) 1 0 '()))
  (define b24 (actor "_" 'black (Place (+ x 6) (+ y 5)) 1 0 '()))
  (define b25 (actor "_" 'black (Place (+ x 7) (+ y 5)) 1 0 '()))
  (define b26 (actor ")" 'black (Place (+ x 8) (+ y 5)) 1 0 '()))
  (define b29 (actor "|" 'black (Place x (+ y 6)) 1 0 '()))
  (define b30 (actor "_" 'black (Place (+ x 1) (+ y 6)) 1 0 '()))
  (define b31 (actor "_" 'black (Place (+ x 2) (+ y 6)) 1 0 '()))
  (define b43 (actor "|" 'black (Place (+ x 6) (+ y 6)) 1 0 '()))
  (define b44 (actor "_" 'black (Place (+ x 4) (+ y 6)) 1 0 '()))
  (define b45 (actor "_" 'black (Place (+ x 5) (+ y 6)) 1 0 '()))
  (define b46 (actor "." 'black (Place (+ x 3) (+ y 6)) 1 0 '()))
  (define b32 (actor "(" 'black (Place (- x 2) (+ y 7)) 1 0 '()))
  (define b33 (actor "_" 'black (Place (- x 1) (+ y 7)) 1 0 '()))
  (define b34 (actor "_" 'black (Place x (+ y 7)) 1 0 '()))
  (define b35 (actor "(" 'black (Place (+ x 1) (+ y 7)) 1 0 '()))
  (define b36 (actor "_" 'black (Place (+ x 2) (+ y 7)) 1 0 '()))
  (define b37 (actor "_" 'black (Place (+ x 3) (+ y 7)) 1 0 '()))
  (define b38 (actor "_" 'black (Place (+ x 4) (+ y 7)) 1 0 '()))
  (define b39 (actor ")" 'black (Place (+ x 5) (+ y 7)) 1 0 '()))
  (define b40 (actor "_" 'black (Place (+ x 6) (+ y 7)) 1 0 '()))
  (define b41 (actor "_" 'black (Place (+ x 7) (+ y 7)) 1 0 '()))
  (define b42 (actor ")" 'black (Place (+ x 8) (+ y 7)) 1 0 '()))
  (list b0 b1 b2 b3 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 b17 b18 b19 b20 b21 b22 b23 b24 b25 b26 b27 b28 b29 b30 b31 b32 b33 b34 b35 b36 b37 b38 b39 b40 b41 b42 b43 b44 b45 b46))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                              ;
;                    MAIN                      ;
;                                              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Get the terminal dimensions
(define-values (term-cols term-rows)
  (ct:with-charterm (ct:charterm-screen-size)))


;; Starter function
(define (start-application)
  (lux:call-with-chaos
   (raart:make-raart)
   (lambda () (lux:fiat-lux (world 0 10.0 (list (actor "momo" 'green (Place 0 0) 1 0 '())) (append art (bear 200 -2) (bear 150 8) (bear 320 4)) '() '() #f)))) 
   (void))
(start-application)




;;;;;;;;;;TO DO LIST :
;; VERY IMPORTANT : clean the code
;;affichage acteurs adversaire --------------->FAIT V
;; Runtime (envoyer les messages + liste d'acteurs vivants) -->Fait
;; Adapter le code de manière à permettre aux acteurs d'envoyer des messages aux autres acteurs -------->Fait
;; Adapter le code de manière à permettre de remonter le temps, comme présenté dans le message du développeur de Terminal Phase. -->Fait
;+++++++++
;;collisions entre acteurs ----------------->Fait
;;acteur vivant ou pas / lives ---->Fait
;;shoot --------------->Fait
;;adversary actors can shoot and rackets goes forward------------>Fait
