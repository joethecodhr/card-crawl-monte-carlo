(load "src/cards.scm")

(assert (equal? no-card 'no-card))
(assert (equal? (make-card 'foo 'bar 'baz) (cons 'bar (cons 'foo 'baz))))
