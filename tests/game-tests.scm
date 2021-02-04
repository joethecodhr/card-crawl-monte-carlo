(load "src/cards.scm")
(load "src/deck.scm")
(load "src/slots.scm")
(load "src/game.scm")

(define-syntax assert-equals
 (syntax-rules ()
  ((_ fn expected)
   (let* ((result fn))
    (if (equal? result expected)
     (begin
      (printf "PASS: Expected: ")
      (display fn)
      (printf " Actual: ")
      (display result)
      (printf "\n"))
     (begin
      (printf "FAIL: Expected: ")
      (display expected)
      (printf " Actual: ")
      (display result)
      (printf "\n")))))))

(assert-equals (replace-card-in-slot 0 '() 'foo) '())
(assert-equals (replace-card-in-slot 0 '(1) 'foo) (list 'foo))
(assert-equals (replace-card-in-slot 0 '(1 2 3 4) 'foo) (list 'foo 2 3 4))
(assert-equals (replace-card-in-slot 3 '(1 2 3 4) 'foo) (list 1 2 3 'foo))

(define c1 (make-card 'foo 'bar 'baz))

(assert-equals (take-card-from-slot 0 (list c1 c1)) (list c1 (list 'the-empty-card c1)))
(assert-equals (take-card-from-slot 1 (list c1 c1)) (list c1 (list c1 'the-empty-card)))
