(define make-card
  (let ()
    (lambda (name type value)
	(let ()
	  (lambda (message . args)
	    (cond ((equal? message 'type) type)
		  ((equal? message 'value) value)
		  ((equal? message 'name) name)
		  ((equal? message 'attack) (set! value (- value (first args))) value)
		  (else (error "Undefined message on make-card"))))))))

(define base-deck
  (list
   (make-card 'crow 'monster 3)
   (make-card 'crow 'monster 3)
   (make-card 'firelamb 'monster 4)
   (make-card 'firelamb 'monster 4)
   (make-card 'goblin 'monster 7)
   (make-card 'goblin 'monster 7)
   (make-card 'incubus 'monster 6)
   (make-card 'incubus 'monster 6)
   (make-card 'plague 'monster 2)
   (make-card 'plague 'monster 2)
   (make-card 'slime 'monster 5)
   (make-card 'slime 'monster 5)
   (make-card 'souleater 'monster 10)
   (make-card 'souleater 'monster 10)
   (make-card 'souleater 'monster 10)
   (make-card 'spider 'monster 8)
   (make-card 'spider 'monster 8)
   (make-card 'troll 'monster 9)
   (make-card 'troll 'monster 9)

   (make-card 'longsword 'sword 7)
   (make-card 'sword 'sword 6)
   (make-card 'shortsword 'sword 5)
   (make-card 'rustysword 'sword 4)
   (make-card 'throwingknives 'sword 3)
   (make-card 'dagger 'sword 2)

   (make-card 'scaleshield 'shield 7)
   (make-card 'roundbuckler 'shield 6)
   (make-card 'leathershield 'shield 5)
   (make-card 'woodenshield 'shield 4)
   (make-card 'woodenshield 'shield 3)
   (make-card 'dustbinlid 'shield 2)
   
   (make-card 'vial 'potion 10)
   (make-card 'vial 'potion 9)
   (make-card 'pot 'potion 8)
   (make-card 'pot 'potion 7)
   (make-card 'bottle 'potion 6)
   (make-card 'bottle 'potion 5)
   (make-card 'bottle 'potion 4)
   (make-card 'powder 'potion 3)
   (make-card 'powder 'potion 2)
		   
   (make-card 'chest 'coin 10)
   (make-card 'chest 'coin 9)
   (make-card 'chest 'coin 8)
   (make-card 'pouch 'coin 7)
   (make-card 'pouch 'coin 6)
   (make-card 'pouch 'coin 5)
   (make-card 'pouch 'coin 4)
   (make-card 'coins 'coin 3)
   (make-card 'coins 'coin 2)))


;; TODO: Fischer-Yates Shuffle
(define (shuffle-deck deck) deck)
  
(define make-player
  (let ()
    (lambda (name health worth)
      (let ()
	(lambda (message)
	  (cond
	   ((equal? message 'name) name)
	   ((equal? message 'health) health)
	   ((equal? message 'worth) worth)
	   (else (error "Undefined message on make-player."))))))))

(define (make-deck) (shuffle-deck base-deck))

(define (make-game)
  (let ((player (make-player 'knight 13 0))
	(deck (make-deck)))

	(lambda (message)
	  (cond ((equal? message 'player) player)
		((equal? message 'deck) deck)
		(else (error "Undefined message on make-game"))))))

(define (play-game game)
  (display "Playing a game.")

  (define (game-iter turn)
    (cond ((> turn 10) 'end-of-line)
	  (else game-iter (+ turn 1))))

  (game-iter 0))
