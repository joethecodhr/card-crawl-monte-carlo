(define (make-game deck dealer-slots player-slots discard-pile sacrifice-counter game-state)
 (list
  ;; dealers deck
  deck
  ;; playfield
  (cons dealer-slots player-slots)
  ;; the discard pile
  discard-pile
  ;; sacrifice counter
  sacrifice-counter
  ;; game state
  game-state))

(define (make-new-game)
 (make-game (make-deck) (make-dealer-slots) (make-player-slots) 'the-empty-deck 0 'game-not-started))

(define (deck game) (list-ref game 0))
(define (dealer-slots game) (car (list-ref game 1)))
(define (player-slots game) (cdr (list-ref game 1)))
(define (discard-pile game) (list-ref game 2))
(define (sacrifice-counter game) (list-ref game 3))
(define (game-state game) (list-ref game 4))

;; Deal cards into empty slots, until there are no more empty slots.
;; Returns (list [new deck state] [new slot state])
(define (deal game)
 (let* ((game-deck (deck game))
        (slots (dealer-slots game)))

  (define (slot-iter new-slots remaining-slots remaining-deck)
   (cond
    ((= (length new-slots) (length slots)) (list remaining-deck new-slots))
    ((= (length remaining-deck) 0) (list remaining-deck new-slots))
    (else
     (let ((test-slot (car remaining-slot))
           (top-card (list-ref remaining-deck 0)))
      (cond ((= (length new-slots) (length slots))
             new-slots)
            ((equal? (slot-card test-slot) 'the-empty-slot)
             (slot-iter (append new-slots (list (put-card-in-slot top-card test-slot))) (list-tail remaining-slots 1) (list-tail remaining-deck 1)))
            (else
             (slot-iter (append new-slots (list test-slot)) (list-tail remaining-slots 1) remaining-deck)))))))

  (let ((post-deal-state (slot-iter '() slots game-deck)))
   (list
    (list-ref post-deal-state 0)
    (cons (list-ref post-deal-state 1) (player-slots game))
    (discard-pile game)
    (sacrifice-counter game)
    (game-state game)))))

;; Takes a card from a slot
;; Returns (list [card taken] [new slot state])
(define (take-card-from-slot slot-number slots)
 (cond ((or (< slot-number 0) (> slot-number (- (length slots) 1))) (error 'take-card-from-slot "slot-number out of range"))
       ((equal? (slot-card (list-ref slots slot-number)) 'the-empty-slot) (error 'take-card-from-slot "slot is empty"))
       (else (list (list-ref slots slot-number) (replace-card-in-slot slot-number slots 'the-empty-card)))))

;; Replaces a card in a slot
;; Returns [new slot state]
(define (replace-card-in-slot slot-number slots card)
  (define (slot-iter return-slots remaining-slots)
   (cond ((= (length return-slots) (length slots)) return-slots)
         ((= slot-number (length return-slots)) (slot-iter (append return-slots (list card)) (list-tail remaining-slots 1)))
         (else (slot-iter (append return-slots (list (car remaining-slots))) (list-tail remaining-slots 1)))))

  (slot-iter '() slots))

;; Game over conditions.
(define (player-is-dead? game) #f)
(define (dealer-slots-empty? game) #f)

;; Game loop!
(define (do-nothing-action state) state)
(define (determine-player-action game) do-nothing-action)

(define (one-turn game player-action-fn)
 (let* ((post-deal-state (deal game))
        (post-player-action-state (player-action-fn post-deal-state)))
  post-player-action-state))

(define (play-game game)
  (define (game-loop game-state turn-count)
   (display game-state)
   (display "\n")

   (let* ((player-action (determine-player-action game-state))
          (end-of-turn-state (one-turn game-state player-action))
          (new-turn-count (+ turn-count 1)))

      (display "turn ")
      (display new-turn-count)
      (display "\n")

      (cond ((player-is-dead? end-of-turn-state) (display "player-is-dead") end-of-turn-state)
            ((dealer-slots-empty? end-of-turn-state) (display "player-won") end-of-turn-state)
            ((> new-turn-count 100) (display "turn-count-too-high") end-of-turn-state)
            (else (game-loop end-of-turn-state new-turn-count)))))

  (game-loop game 0))
