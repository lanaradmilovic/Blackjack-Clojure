(ns cheat-sheet-test.cheat-sheet-test
  (:require [midje.sweet :refer :all]
            [cheat-sheet.cheat-sheet :refer :all :as cs]))

(fact "Testing cheat sheet values conversion."
      (cs/value-for-ch "ace") => "A"
      (cs/value-for-ch "jack") => "10"
      (cs/value-for-ch "2") => "2"
      (cs/value-for-ch "unknown") => "unknown")

(fact "'update-sheet' should update value at a given position."
      (let [initial-cheat-sheet (atom
                                  {:players-cards ["8" "9" "10" "11" "12" "13" "14" "15" "16" "17" "A2" "A3" "A4" "A5" "A6" "A7" "A8" "A9"
                                                   "22" "33" "44" "55" "66" "77" "88" "99" "1010" "AA"]
                                   :dealers-card  ["2" "3" "4" "5" "6" "7" "8" "9" "10" "A"]
                                   :values        (vec (repeat 28 (vec (repeat 10 "H"))))})]
        (get-in (:values (cs/update-sheet @initial-cheat-sheet 2 3 "new-value")) [2 3]) => "new-value"))

(fact "Testing exception handling in case of non-existing player's and dealer's card."
      (let
        [initial-cheat-sheet (atom
                               {:players-cards ["8" "9" "10" "11" "12" "13" "14" "15" "16" "17" "A2" "A3" "A4" "A5" "A6" "A7" "A8" "A9"
                                                "22" "33" "44" "55" "66" "77" "88" "99" "1010" "AA"]
                                :dealers-card  ["2" "3" "4" "5" "6" "7" "8" "9" "10" "A"]
                                :values        (vec (repeat 28 (vec (repeat 10 "H"))))})]
        (cs/get-move @initial-cheat-sheet "7" "1") => falsey))


(fact "Checking setting cheat sheet values."
       (let
         [initial-cheat-sheet (atom
                                {:players-cards ["8" "9" "10" "11" "12" "13" "14" "15" "16" "17" "A2" "A3" "A4" "A5" "A6" "A7" "A8" "A9"
                                                 "22" "33" "44" "55" "66" "77" "88" "99" "1010" "AA"]
                                 :dealers-card  ["2" "3" "4" "5" "6" "7" "8" "9" "10" "A"]
                                 :values        (vec (repeat 28 (vec (repeat 10 "H"))))})]
         (cs/set-value! initial-cheat-sheet "8" "2" "new-value")
         (cs/get-move @initial-cheat-sheet "8" "2") => "new-value"
         (cs/set-value! initial-cheat-sheet "8" ["2" "3" "4"] "new-value")
         (map #(cs/get-move @initial-cheat-sheet "8" %) ["2" "3" "4"]) => ["new-value" "new-value" "new-value"]
         (cs/set-value! initial-cheat-sheet "7" "1" "new-value")
         (cs/get-move @initial-cheat-sheet "7" "1") => falsey))







(let
  [initial-cheat-sheet (atom
                         {:players-cards ["8" "9" "10" "11" "12" "13" "14" "15" "16" "17" "A2" "A3" "A4" "A5" "A6" "A7" "A8" "A9"
                                          "22" "33" "44" "55" "66" "77" "88" "99" "1010" "AA"]
                          :dealers-card  ["2" "3" "4" "5" "6" "7" "8" "9" "10" "A"]
                          :values        (vec (repeat 28 (vec (repeat 10 "H"))))})]
  )