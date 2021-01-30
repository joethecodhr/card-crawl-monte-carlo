(define make-game
 (list
  ;; dealers deck
  (make-deck)
  ;; playfield
  (cons (make-dealer-slots) (make-player-slots))
  ;; the discard pile
  the-empty-deck
  ;; sacrifice counter
  0))

(define (deal-card-to-empty-slot deck slots)
 (let ((card (first deck)))
  (cond ((equal? (slot-card (first slots)) 'the-empty-slot) (put-card-in-slot card (car slots)))
        (else 'the-empty-slot))))
