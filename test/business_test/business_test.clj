(ns business-test.business-test
  (:require [midje.sweet :refer :all]
            [business.business :refer :all :as b]))
  (fact "Testing generate-card function"
        (let [card (generate-card "10" "heart")]
          (fact "Generated card should have the correct value and suit"
                (card :value) => "10"
                (card :suit) => "heart")))

(fact "Testing 'get-nth-player-card' function"
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
(fact "Testing 'get-dealer-card' function"
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

(fact "Testing add-new-player-card function edge cases"
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
        (b/add-both player-cards current-cards) => falsey))
(fact "Tests edge cases for 'card-value' function."
      (card-value "jack") => 10
      (card-value "unknown") => falsey)

(fact "Updating player card value with a non-existing card number should return nil."
      (let [player-cards ({:card-1 {:value "10" :suit "heart"}}
                          {:card-2 {:value "10" :suit "heart"}})]
        (update-player-card-value player-cards 3 "5") => falsey))

(fact "Updating player card value."
      (let [player-cards (atom {:card-1 {:value "10" :suit "heart"}
                                :card-2 {:value "10" :suit "heart"}})]
       (fact "Updating player card value with a non-existing card number should return nil."
             (b/update-player-card-value! player-cards 3 "5") => falsey)
        (fact "'update-player-card-value!' should return 'nil' when provided with dereferenced values."
              (b/update-player-card-value! @player-cards 3 "5") => falsey)))












