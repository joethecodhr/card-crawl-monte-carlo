(define the-empty-slot 'empty-slot)
(define the-hand-slot 'hand-slot)
(define the-player-slot 'player-slot)
(define the-backpack-slot 'backpack-slot)
(define the-dealer-slot 'the-dealer-slot)

(define (make-slot slot)
 (cons slot 'the-empty-slot))

(define (slot-type slot) (car slot))

(define (slot-card slot) (cdr slot))

(define (put-card-in-slot card slot)
  (cons (car slot) card))

(define (make-dealer-slots)
 (list 
  (make-slot 'the-dealer-slot)
  (make-slot 'the-dealer-slot)
  (make-slot 'the-dealer-slot)
  (make-slot 'the-dealer-slot)))

(define (make-player-slots)
 (list 
  (make-slot 'the-hand-slot)
  (make-slot 'the-player-slot)
  (make-slot 'the-hand-slot)
  (make-slot 'the-backpack-slot)))
