(define (log . msg)
  (display msg)
  (newline))

(define no-card ())

(define make-card
  (let ()
    (lambda (name type value)
	(let ()
	  (lambda (message . args)
	    (cond ((equal? message 'type) type)
		  ((equal? message 'value) value)
		  ((equal? message 'name) name)
		  ((equal? message 'attack)
		   (set! value (- value (first args)))
		   (log name " was attacked for " value)
		   value)
		  (else (error "Undefined message on make-card"))))))))

(define (make-n-cards n name type value)
  (define (card-iter cards num)
    (cond ((= num n) cards)
	  (else (card-iter (append cards (list (make-card name type value))) (+ num 1)))))

  (card-iter '() 0))

(define (make-two-cards name monster value)
  (make-n-cards 2 name monster value))

(define (make-three-cards name monster value)
  (make-n-cards 3 name monster value))

(define (make-monster-cards)
  (append
   (make-two-cards 'crow 'monster 3)
   (make-two-cards 'firelamb 'monster 4)
   (make-two-cards 'goblin 'monster 7)
   (make-two-cards 'incubus 'monster 6)
   (make-two-cards 'plague 'monster 2)
   (make-two-cards 'slime 'monster 5)
   (make-two-cards 'spider 'monster 8)
   (make-two-cards 'troll 'monster 9)

   (make-three-cards 'souleater 'monster 10)))

(define (make-sword-cards)
  (list
   (make-card 'longsword 'sword 7)
   (make-card 'sword 'sword 6)
   (make-card 'shortsword 'sword 5)
   (make-card 'rustysword 'sword 4)
   (make-card 'throwingknives 'sword 3)
   (make-card 'dagger 'sword 2)))

(define (make-shield-cards)
  (list
   (make-card 'scaleshield 'shield 7)
   (make-card 'roundbuckler 'shield 6)
   (make-card 'leathershield 'shield 5)
   (make-card 'woodenshield 'shield 4)
   (make-card 'woodenshield 'shield 3)
   (make-card 'dustbinlid 'shield 2)))
  
(define (make-potion-cards)
  (list
   (make-card 'vial 'potion 10)
   (make-card 'vial 'potion 9)
   (make-card 'pot 'potion 8)
   (make-card 'pot 'potion 7)
   (make-card 'bottle 'potion 6)
   (make-card 'bottle 'potion 5)
   (make-card 'bottle 'potion 4)
   (make-card 'powder 'potion 3)
   (make-card 'powder 'potion 2)))

(define (make-coin-cards)
  (list
   (make-card 'chest 'coin 10)
   (make-card 'chest 'coin 9)
   (make-card 'chest 'coin 8)
   (make-card 'pouch 'coin 7)
   (make-card 'pouch 'coin 6)
   (make-card 'pouch 'coin 5)
   (make-card 'pouch 'coin 4)
   (make-card 'coins 'coin 3)
   (make-card 'coins 'coin 2)))
  
(define base-deck
  (append
   (make-monster-cards)
   (make-sword-cards)
   (make-shield-cards)
   (make-potion-cards)
   (make-coin-cards)))
		   
(define (shuffle-deck deck)
  (let ((vector-deck (list->vector deck)))

    (define (iter i)
	(let ((j (random (length deck))))

	(cond ((= i 0) vector-deck)
	       (else
		(let ((j-value (vector-ref vector-deck j))
		      (i-value (vector-ref vector-deck i)))

		  (vector-set! vector-deck i j-value)
		  (vector-set! vector-deck j i-value)
		  (iter (- i 1)))))))

  (vector->list (iter (- (length deck) 1)))))
  
(define make-player
  (let ()
    (lambda (name health worth)
      (let ()
	(lambda (message . args)
	  (cond
	   ((equal? message 'name) name)
	   ((equal? message 'health) health)
	   ((equal? message 'worth) worth)
	   ((equal? message 'attack) (set! health (- health (car args))) health)
	   (else (error "Undefined message on make-player."))))))))

(define (make-deck) (shuffle-deck base-deck))

(define the-empty-deck ())

;; TODO: Implement deal-to-next-slot, returns slots method.
;;                 play-card-against-card, returns game method.
(define (make-game)
  (let ((player (make-player 'knight 13 0))
	(deck (make-deck))
	(dealt-cards (make-vector 4 no-card))
        (active-cards (make-vector 2 no-card))
	(keep-cards (make-vector 1 no-card)))

	(lambda (message)
	  (cond ((equal? message 'player) player)
		((equal? message 'deck) deck)
		((equal? message 'slots) slots)
		((equal? message 'deal-card)
		 (cond ((equal? deck the-empty-deck) the-empty-deck)
		       (else
			(let ((card (car deck)))
			    (set! deck (cdr deck))
			    card))))
		(else (error "Undefined message on make-game"))))))

;(define (game-flow game)
  ;(define (deal-iter empty-slots)
    ;(cond ((empty? (game 'deck)) the-empty-deck)
  ; (let

(define (player-dead? player)
  (< (player 'health) 1))

(define (game-over? deck slots)
  (and (empty? deck))) ;; The slot check has been removed since it
                       ;; is not implemented.

(define (play-game game)
  (log "Playing a game.")

  (define (game-iter turn)
    (cond ((player-dead? (game 'player)) (log "You lost.") #f)
	  ((game-over? (game 'deck) (game 'slots)) (log "You won!") #f)
	  ((> turn 52) 'end-of-line)
	  (else
	   (game 'deal-card)
	   (game-iter (+ turn 1)))))

  (game-iter 0))
