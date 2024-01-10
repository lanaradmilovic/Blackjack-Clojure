(ns business-test.business-test
  (:require [midje.sweet :refer :all]
            [business.business :refer :all :as b]
            [cheat-sheet.cheat-sheet :refer :all :as sheet]))
(fact "Testing 'generate-card' function. Generated card should have the correct value and suit."
      (let [card (generate-card "10" "heart")]
        (card :value) => "10"
        (card :suit) => "heart"))

(facts "Testing 'get-nth-player-card' function."
       (fact "When card has the nth player card"
             (let [test-card {:card-1 {:value "10" :suit "heart"}
                              :card-2 {:value "A" :suit "diamond"}}
                   result (get-nth-player-card test-card 2)]
               result => {:value "A" :suit "diamond"}))

       (fact "When card has no nth player card"
             (let [test-card {:card-1 {:value "10" :suit "heart"}}
                   result (get-nth-player-card test-card 2)]
               result => falsey))

       (fact "When card is nil"
             (let [result (get-nth-player-card nil 2)]
               result => falsey)))
(fact "Testing 'get-dealer-card' function."
      (fact "When card has a dealer card"
            (let [test-card {:dealer-card [{:value "10" :suit "heart"}]}
                  result (get-dealer-card test-card)]
              result => {:value "10" :suit "heart"}))

      (fact "When card has no dealer card"
            (let [test-card {:dealer-card []}
                  result (get-dealer-card test-card)]
              result => falsey))

      (fact "When card is nil"
            (let [result (get-dealer-card nil)]
              result => falsey)))

(fact "Testing 'add-new-player-card' function edge cases."
      (fact "When adding a new card to an empty hand"
            (let [test-hand {}
                  new-card {:value "10" :suit "heart"}
                  result (add-new-player-card test-hand new-card)]
              result => {:card-1 {:value "10" :suit "heart"}}))

      (fact "When adding multiple cards to a hand"
            (let [test-hand {}
                  new-card-1 {:value "10" :suit "heart"}
                  new-card-2 {:value "A" :suit "diamond"}
                  new-card-3 {:value "5" :suit "club"}
                  result (-> test-hand
                             (add-new-player-card new-card-1)
                             (add-new-player-card new-card-2)
                             (add-new-player-card new-card-3))]
              result => {:card-1 {:value "10" :suit "heart"}
                         :card-2 {:value "A" :suit "diamond"}
                         :card-3 {:value "5" :suit "club"}}))

      (fact "When adding cards with special characters"
            (let [test-hand {}
                  new-card-1 {:value "10" :suit "heart"}
                  new-card-2 {:value "A" :suit "diamond"}
                  new-card-3 {:value "*" :suit "@"}
                  result (-> test-hand
                             (add-new-player-card new-card-1)
                             (add-new-player-card new-card-2)
                             (add-new-player-card new-card-3))]
              result => {:card-1 {:value "10" :suit "heart"}
                         :card-2 {:value "A" :suit "diamond"}
                         :card-3 {:value "*" :suit "@"}})))
(fact "'add-both' should return 'nil' when provided with dereferenced values."
      (let [player-cards ({:card-1 {:value "10" :suit "heart"}}
                          {:card-2 {:value "10" :suit "heart"}})
            current-cards {:value "7" :suit "club"}]
        (b/add-both! player-cards current-cards) => falsey))
(fact "Tests edge cases for 'card-value' function."
      (card-value "jack") => 10
      (card-value "unknown") => falsey)

(fact "Updating player card value with a non-existing card number should return nil."
      (let [player-cards ({:card-1 {:value "10" :suit "heart"}}
                          {:card-2 {:value "10" :suit "heart"}})]
        (update-player-card-value player-cards 3 "5") => falsey))

(facts "Updating player card value."
       (let [player-cards (atom {:card-1 {:value "10" :suit "heart"}
                                 :card-2 {:value "10" :suit "heart"}})]
         (fact "Updating player card value with a non-existing card number should return nil"
               (b/update-player-card-value! player-cards 3 "5") => falsey)
         (fact "'update-player-card-value!' should return 'nil' when provided with dereferenced values"
               (b/update-player-card-value! @player-cards 3 "5") => falsey)))

(facts "Calculating sum of player cards."
       (let [empty-hand {}
             single-card {:card-1 {:value "10" :suit "heart"}}]

         (fact "With an empty hand should return 0"
               (player-sum empty-hand) => 0)

         (fact "With a single card should return the value of that card"
               (player-sum single-card) => 10)))

(facts "'get-ace-position' and 'get-no-ace-position' should handle different scenarios"
       (let [empty-hand {}
             hand-without-ace {:card-1 {:value "king" :suit "heart"}
                               :card-2 {:value "queen" :suit "diamond"}}
             hand-with-ace {:card-1 {:value "ace" :suit "heart"}
                            :card-2 {:value "king" :suit "diamond"}
                            :card-3 {:value "ace" :suit "spade"}}]
         (fact "Return an empty list for an empty hand"
               (get-ace-position empty-hand) => []
               (get-no-ace-position empty-hand) => [])

         (fact "Return an empty list for a hand with no 'ace' values"
               (get-ace-position hand-without-ace) => []
               (get-no-ace-position hand-without-ace) => [2 1])

         (fact "Return the positions of cards with 'ace' value"
               (get-ace-position hand-with-ace) => [3 1]
               (get-no-ace-position hand-with-ace) => [2])))

(facts "'adjust-ace-value' should handle different scenarios."
       (let [no-ace-hand (atom {:card-1 {:value "king" :suit "heart"}
                                :card-2 {:value "queen" :suit "diamond"}})
             two-ace-hand (atom {:card-1 {:value "ace" :suit "heart"}
                                 :card-2 {:value "ace" :suit "diamond"}})
             ace-value-eleven (atom {:card-1 {:value "ace" :suit "heart"}
                                     :card-2 {:value "king" :suit "diamond"}})
             ace-value-one (atom {:card-1 {:value "ace" :suit "heart"}
                                  :card-2 {:value "king" :suit "diamond"}
                                  :card-3 {:value "ace" :suit "spade"}})]

         (fact "No adjustment is made for a hand without 'ace' cards"
               (adjust-ace-value! no-ace-hand) => (deref no-ace-hand))

         (fact "No adjustment is made when the sum is less or equals to 21"
               (adjust-ace-value! no-ace-hand) => (deref no-ace-hand)
               (adjust-ace-value! ace-value-eleven) => (deref ace-value-eleven))

         (fact "Adjust 'ace' value from 11 to 1 when the sum is over 21"
               (adjust-ace-value! ace-value-one) => {:card-1 {:value 1 :suit "heart"}
                                                     :card-2 {:value "king" :suit "diamond"}
                                                     :card-3 {:value 1 :suit "spade"}}
               (adjust-ace-value! two-ace-hand) => {:card-1 {:value "ace" :suit "heart"}
                                                    :card-2 {:value 1 :suit "diamond"}})))


(fact "Testing between when number is outside the range."
      (b/between? 5 10 15) => falsey)

(fact "Testing ace-to-A function."
      (ace-to-A "ace") => "A"
      (ace-to-A "king") => "king"
      (ace-to-A "invalid") => "invalid"
      (ace-to-A nil) => falsey)

(facts "Testing 'recommend-move' function."

       (fact "Player has a Blackjack"
             (let [cheat-sheet @sheet/blackjack-cheat-sheet
                   current-cards {:player-cards (list {:value "ace" :suit "heart"} {:value "king" :suit "heart"})
                                  :dealer-card  (list {:value "king" :suit "heart"})}
                   player-hand {:card-1 {:value "ace" :suit "heart"}
                                :card-2 {:value "king" :suit "heart"}}]
               (b/recommend-move cheat-sheet current-cards player-hand) => "Blackjack!"))


       (fact "Player has more than 2 cards and current sum is less than 9"
             (let [cheat-sheet @sheet/blackjack-cheat-sheet
                   current-cards {:player-cards (list {:value "3" :suit "heart"} {:value "2" :suit "heart"} {:value "3" :suit "heart"})
                                  :dealer-card  (list {:value "king" :suit "heart"})}
                   player-hand {:card-1 {:value "3" :suit "heart"}
                                :card-2 {:value "2" :suit "heart"}
                                :card-3 {:value "3" :suit "heart"}}]

               (b/recommend-move cheat-sheet current-cards player-hand) => "H"))

       (fact "Player has 2 cards, and their sum is between 8 and 18, and both cards are different, and no Ace in the hand"
             (let [cheat-sheet @sheet/blackjack-cheat-sheet
                   current-cards {:player-cards (list {:value "5" :suit "heart"} {:value "4" :suit "heart"})
                                  :dealer-card  (list {:value "king" :suit "heart"})}
                   player-hand {:card-1 {:value "5" :suit "heart"}
                                :card-2 {:value "4" :suit "heart"}}]

               (b/recommend-move cheat-sheet current-cards player-hand) => "H"))

       (fact "Player has 2 Aces"
             (let [cheat-sheet @sheet/blackjack-cheat-sheet
                   current-cards {:player-cards (list {:value "ace" :suit "heart"} {:value "ace" :suit "heart"})
                                  :dealer-card  (list {:value "king" :suit "heart"})}
                   player-hand {:card-1 {:value "ace" :suit "heart"}
                                :card-2 {:value "ace" :suit "heart"}}]

               (b/recommend-move cheat-sheet current-cards player-hand) => "P"))

       (fact "Player has 1 Ace"
             (let [cheat-sheet @sheet/blackjack-cheat-sheet
                   current-cards {:player-cards (list {:value "ace" :suit "heart"} {:value "4" :suit "heart"})
                                  :dealer-card  (list {:value "king" :suit "heart"})}
                   player-hand {:card-1 {:value "ace" :suit "heart"}
                                :card-2 {:value "4" :suit "heart"}}]

               (b/recommend-move cheat-sheet current-cards player-hand) => "H"))

       (fact "Player has 2 cards of the same value (not Ace)"
             (let [cheat-sheet @sheet/blackjack-cheat-sheet
                   current-cards {:player-cards (list {:value "4" :suit "heart"} {:value "4" :suit "heart"})
                                  :dealer-card  (list {:value "king" :suit "heart"})}
                   player-hand {:card-1 {:value "4" :suit "heart"}
                                :card-2 {:value "4" :suit "heart"}}]

               (b/recommend-move cheat-sheet current-cards player-hand) => "H"))

       (fact "Player has 2 cards, and current sum is greater than 17"
             (let [cheat-sheet @sheet/blackjack-cheat-sheet
                   current-cards {:player-cards (list {:value "10" :suit "heart"} {:value "9" :suit "heart"})
                                  :dealer-card  (list {:value "king" :suit "heart"})}
                   player-hand {:card-1 {:value "10" :suit "heart"}
                                :card-2 {:value "9" :suit "heart"}}]

               (b/recommend-move cheat-sheet current-cards player-hand) => "S"))

       (fact "Empty hand. (Not covered in the cheat sheet)"
             (let [cheat-sheet @sheet/blackjack-cheat-sheet
                   current-cards {}
                   player-hand {}]
               (b/recommend-move cheat-sheet current-cards player-hand) => falsey))

       (fact "Player has 3 cards, and current sum is greater than 21. (Bust)"
             (let [cheat-sheet @sheet/blackjack-cheat-sheet
                   current-cards {:player-cards (list {:value "10" :suit "heart"} {:value "9" :suit "heart"} {:value "5" :suit "heart"})
                                  :dealer-card  (list {:value "king" :suit "heart"})}
                   player-hand {:card-1 {:value "10" :suit "heart"}
                                :card-2 {:value "9" :suit "heart"}
                                :card-3 {:value "5" :suit "heart"}}]
               (b/recommend-move cheat-sheet current-cards player-hand) => "Bust!")))


(facts "Testing 'generate-list' function."
      (fact "Testing generate-list function with positive n"
            (b/generate-list 5) => (reverse '(1 2 3 4 5)))

      (fact "Testing generate-list function with n = 1"
            (b/generate-list 1) => '(1))

      (fact "Testing generate-list function with n = 0"
            (b/generate-list 0) => '())

      (fact "Testing generate-list function with negative n"
            (b/generate-list -3) => '())

      (fact "Testing generate-list function with large n"
            (b/generate-list 1000) => (reverse (take 1000 (iterate inc 1)))))

(facts "Testing 'get-all-values' function."
       (fact "Testing get-all-values function with a player and dealer hand"
             (b/get-all-values {:player-cards (list {:value "4" :suit "heart"} {:value "4" :suit "heart"})
                                :dealer-card  (list {:value "king" :suit "heart"})})
             => '(4 4 10))

       (fact "Testing get-all-values function with an empty hand"
             (b/get-all-values {:player-cards '()
                                :dealer-card  nil})
             => falsey))

(facts "Testing 'start-value' function."
       (fact "When dealer's card value is king"
             (let [current-cards {:player-cards (list {:value "7" :suit "heart"} {:value "6" :suit "heart"})
                                  :dealer-card  (list {:value "king" :suit "heart"})}]
               (b/start-value current-cards) => 7))

       (fact "When dealer's card value is 7"
             (let [current-cards {:player-cards (list {:value "7" :suit "heart"} {:value "6" :suit "heart"})
                                  :dealer-card  (list {:value "7" :suit "heart"})}]
               (b/start-value current-cards) => 10))

       (fact "When dealer's card value is 2 (less than 7)"
             (let [current-cards {:player-cards (list {:value "7" :suit "heart"} {:value "6" :suit "heart"})
                                  :dealer-card  (list {:value "2" :suit "heart"})}]
               (b/start-value current-cards) => 1))

       (fact "When dealer's card value is ace"
             (let [current-cards {:player-cards (list {:value "7" :suit "heart"} {:value "6" :suit "heart"})
                                  :dealer-card  (list {:value "ace" :suit "heart"})}]
               (b/start-value current-cards) => 6)))

(facts "Testing if 'subvec-dd' returns the correct subvector."
       (fact "Index out of bounds exception when player total is greater than 18"
             (subvec-dd {:card-1 {:value "10" :suit "heart"}
                         :card-2 {:value "9" :suit "heart"}}
                        [1 2 3 4 5 6 7 8 9 10 "jack" "queen" "king" "ace"])
             => falsey)
       (fact "'subvec-dd' handles values inside the range gracefully"
             (subvec-dd {:card-1 {:value "10" :suit "heart"}
                         :card-2 {:value "5" :suit "heart"}}
                        [1 2 3 4 5 6 7 8 9 "jack" "queen" "king" "ace"])
             => [3 4 5 6] ; So that total is >= 18, <= 21
             ))

(facts "Testing 'play' function."
      (fact "Testing play function - Bust"
            (let [cheat-sheet sheet/blackjack-cheat-sheet
                  current-cards (atom {:player-cards (list {:value "10" :suit "heart"} {:value "9" :suit "heart"} {:value "3" :suit "heart"})
                                       :dealer-card  (list {:value "king" :suit "heart"})})
                  player-cards (atom {:card-1 {:value "10" :suit "heart"}
                                      :card-2 {:value "9" :suit "heart"}
                                      :card-3 {:value "3" :suit "heart"}})
                  expected-result "End of game!"]
              (b/play current-cards player-cards cheat-sheet b/suits b/initial-deck b/values)
              => expected-result))

      (fact "Testing play function - Can't calculate odds"
            (let [cheat-sheet sheet/blackjack-cheat-sheet
                  current-cards (atom {:player-cards (list {:value "10" :suit "heart"} {:value "9" :suit "heart"})
                                       :dealer-card  (list {:value "5" :suit "heart"})})
                  player-cards (atom {:card-1 {:value "10" :suit "heart"}
                                      :card-2 {:value "9" :suit "heart"}})
                  expected-result "Can't calculate odds!"]
              (b/play current-cards player-cards cheat-sheet b/suits b/initial-deck b/values)
              => expected-result)))






