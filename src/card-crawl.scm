;; Cards!
(load "cards.scm")

;; Deck!
(load "deck.scm")

;; The game:
;;  * 4 slots for the dealer
;;  * 4 slots for the player
(load "slots.scm")

;; Mechanics!
(load "game.scm")

;; The Dealer!
;;  * The dealer deals cards to their slots. When three slots are empty, the dealer deals three cards out.

;;  * The player can take a non-monster card and place it into their empty slots
;;    * Once a card is in the hand slot, it cannot be moved to the backpack slot
;;  * Once the cards are in the player slots they can
;;    * Play a card on another card
;;    * Sell for gold (depends on the card)
;;    * Move cards from their backpack slot to an empty hand slot
;;    * If no actions have been taken, sacrifice 5 health to shuffle the current cards into the deck and redeal.
;;      * The cost of this starts at 5 health, and goes up by +2 each usage

(define (play-a-game)
 (display (play-game make-game)))
