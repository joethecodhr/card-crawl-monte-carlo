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

(define (make-deck) (shuffle-deck base-deck))

(define the-empty-deck '())
