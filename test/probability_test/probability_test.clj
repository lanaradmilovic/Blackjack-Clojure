(ns probability-test.probability-test
  (:require [midje.sweet :refer :all]
            [probability.probability :refer :all :as p]))

(fact "Testing contain-element? function"
      (fact "Testing contain-element? function with existing element"
            (p/contain-element? [1 2 3] 2) => true)

      (fact "Testing contain-element? function with non-existing element"
            (p/contain-element? [1 2 3] 4) => falsey)

      (fact "Testing contain-element? function with empty list"
            (p/contain-element? [] 1) => falsey)

      (fact "Testing contain-element? function with duplicate elements"
            (p/contain-element? [1 2 2 3] 2) => true))

(fact "Testing 'generate-list' function"
      (fact "Testing generate-list function with positive n"
            (generate-list 5) => (reverse '(1 2 3 4 5)))

      (fact "Testing generate-list function with n = 1"
            (generate-list 1) => '(1))

      (fact "Testing generate-list function with n = 0"
            (generate-list 0) => '())

      (fact "Testing generate-list function with negative n"
            (generate-list -3) => '())

      (fact "Testing generate-list function with large n"
            (generate-list 1000) => (reverse (take 1000 (iterate inc 1)))))

(fact "Testing 'get-all-val' function"
      (fact "Testing get-all-val function with a player and dealer hand"
            (get-all-val {:player-cards (list {:value "4" :suit "heart"} {:value "4" :suit "heart"})
                          :dealer-card  (list {:value "king" :suit "heart"})})
            => '("4" "4" "king"))

      (fact "Testing get-all-val function with an empty hand"
            (get-all-val {:player-cards '()
                          :dealer-card  nil})
            => '("")))

(fact "Testing 'decrement-counter-on-match' function"
      (fact "Testing decrement-counter-on-match function with matching values"
            (let [counter (atom 3)
                  card {:player-cards [{:value 10 :suit "heart"} {:value "ace" :suit "spade"}]
                        :dealer-card  {:value "king" :suit "diamond"}}
                  fit-value 10]
              (decrement-counter-on-match counter card fit-value)
              @counter
              => 2))
      (fact "Testing decrement-counter-on-match function with two matching values"
            (let [counter (atom 3)
                  card {:player-cards [{:value 10 :suit "heart"} {:value 10 :suit "spade"}]
                        :dealer-card  {:value "king" :suit "diamond"}}
                  fit-value 10]
              (decrement-counter-on-match counter card fit-value)
              @counter
              => 1))
      (fact "Testing decrement-counter-on-match function with no matching values"
            (let [counter (atom 3)
                  card {:player-cards [{:value 10 :suit "heart"} {:value "ace" :suit "spade"}]
                        :dealer-card  {:value "king" :suit "diamond"}}
                  fit-value 5]
              (decrement-counter-on-match counter card fit-value)
              @counter
              => 3))

      (fact "Testing decrement-counter-on-match function with an empty hand"
            (let [counter (atom 3)
                  card {:player-cards []
                        :dealer-card  nil}
                  fit-value 10]
              (decrement-counter-on-match counter card fit-value)
              @counter
              => 3))
      (fact "Testing decrement-counter-on-match function with counter = 0"
            (let [counter (atom 0)
                  card {:player-cards [{:value 10 :suit "heart"} {:value "ace" :suit "spade"}]
                        :dealer-card  {:value "king" :suit "diamond"}}
                  fit-value 10]
              (decrement-counter-on-match counter card fit-value)
              @counter
              => 0)))

(fact "Testing 'count-probability-hit' function in case divisor is 0"
      (let [counter (atom 10)
            divisor 0
            card {:card-1 {:value "ace" :suit "heart"}
                  :card-2 {:value "4" :suit "heart"}}
            fit-value 9]
        (count-probability-hit counter divisor card fit-value) => falsey))

(fact "Testing 'subvector' function"
      (fact "Testing subvector function with start and stop values inside the range"
            (let [input-list [1 2 3 4 5 6 7 8 9 10 "ace" "jack" "queen" "king"]
                  start-value 10
                  end-value 14
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

