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

;; Deal cards into empty slots, until there are no more empty slots.
;; Returns (list [new deck state] [new slot state])
(define (deal deck slots)
  (define (slot-iter new-slots remaining-slots remaining-deck)
   (if (= (length new-slots) (length slots)) (list remaining-deck new-slots)
     (let ((test-slot (first remaining-slots))
           (top-card (first remaining-deck)))
      (cond ((= (length new-slots) (length slots))
             new-slots)
            ((equal? (slot-card test-slot) 'the-empty-slot)
             (slot-iter (append new-slots (list (put-card-in-slot top-card test-slot))) (list-tail remaining-slots 1) (list-tail remaining-deck 1)))
            (else
             (slot-iter (append new-slots (list test-slot)) (list-tail remaining-slots 1) remaining-deck))))))

  (slot-iter '() slots deck))
