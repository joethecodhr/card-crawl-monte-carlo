(load "src/cards.scm")
(load "src/deck.scm")
(load "src/slots.scm")
(load "src/game.scm")

(define-syntax assert-equals
 (syntax-rules ()
  ((_ fn expected)
   (let* ((result fn))
    (if (equal? result expected)
     (printf "PASS\n")
     (begin
      (printf "FAIL: Expected: ")
      (display expected)
      (printf " Actual: ")
      (display result)
      (printf "\n")))))))

(assert-equals #t #t)
(assert-equals (replace-card-in-slot 0 '() 'the-empty-card) (list 'the-empty-card))

