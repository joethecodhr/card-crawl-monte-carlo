(define no-card ())

(define (make-card name type value)
 (cons type (cons name value)))

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
  
(define (make-player-card name health)
 (make-card name 'player health))
