(ns probability.probability-test
  (:require [midje.sweet :refer :all]
            [probability.probability :refer :all :as p]
            [business.business :refer :all :as b]))

(facts "Testing 'get-all-val' function."
       (fact "Testing get-all-val function with a player and dealer hand"
             (p/get-all-val {:player-cards (list {:value "4" :suit "heart"} {:value "4" :suit "heart"})
                             :dealer-card  (list {:value "king" :suit "heart"})})
             => '("4" "4" "king"))

       (fact "Testing get-all-val function with an empty hand"
             (first (p/get-all-val {:player-cards '()
                             :dealer-card  nil}))
             => falsey))
(fact "Testing if 'num-passed-certain-value' returns the correct count."
      (let [current-cards {:player-cards (list {:value "10" :suit "heart"} {:value "9" :suit "heart"} {:value "9" :suit "heart"})
                           :dealer-card  (list {:value "king" :suit "heart"})}]
        (p/num-passed-certain-value current-cards "10") => 1
        (p/num-passed-certain-value current-cards "9") => 2
        (p/num-passed-certain-value current-cards "king") => 1
        (p/num-passed-certain-value current-cards "ace") => 0))

(fact "Testing 'odds-certain-value' function."
      (let [player-card '()
            initial-deck '()
            current-cards {:player-cards (list {:value "10" :suit "heart"} {:value "9" :suit "heart"})
                           :dealer-card  (list {:value "king" :suit "heart"})}
            value 2]
        (p/odds-certain-value b/suits current-cards value initial-deck player-card) => falsey))

(facts "Testing 'subvector' function."
       (fact "Testing subvector function with start and stop values inside the range"
             (let [input-list [1 2 3 4 5 6 7 8 9 10 "ace" "jack" "queen" "king"]
                   start-value 10
                   end-value ""
                   expected-result [10 "ace" "jack" "queen" "king"]]
               (subvector input-list start-value end-value)
               => expected-result))

       (fact "Testing subvector function with start and stop values outside the range"
             (let [input-list [1 2 3 4 5 6 7 8 9 10 "ace" "jack" "queen" "king"]
                   start-value 20
                   end-value 30
                   expected-result falsey]
               (subvector input-list start-value end-value)
               => expected-result)))
