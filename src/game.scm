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
   (cond
    ((= (length new-slots) (length slots)) (list remaining-deck new-slots))
    ((= (length remaining-deck) 0) (list remaining-deck new-slots))
    (else
     (let ((test-slot (first remaining-slots))
           (top-card (first remaining-deck)))
      (cond ((= (length new-slots) (length slots))
             new-slots)
            ((equal? (slot-card test-slot) 'the-empty-slot)
             (slot-iter (append new-slots (list (put-card-in-slot top-card test-slot))) (list-tail remaining-slots 1) (list-tail remaining-deck 1)))
            (else
             (slot-iter (append new-slots (list test-slot)) (list-tail remaining-slots 1) remaining-deck)))))))

  (slot-iter '() slots deck))

;; Takes a card from a slot
;; Retunrs (list [card taken] [new slot state])
(define (take-card-from-slot slot-number slots)
 (cond ((or (< slot-number 0) (> slot-number (- (length slots) 1))) (error "slot-number out of range"))
       ((equal? (slot-card (list-ref slots slot-number)) 'the-empty-slot) (error "slot is empty"))
       (else (error "not-implemented"))))

(define (replace-card-in-slot slot-number slots card)
  (define (slot-iter return-slots remaining-slots)
   (cond ((= (length return-slots) (length slots)) return-slots)
         ((= slot-number (length return-slots)) (slot-iter (append return-slots (list card))))
         (else (slot-iter (append return-slots (list-tail remaining-slots 1)) (list-tail remaining-slots 1)))))

  (slot-iter '() slots))
