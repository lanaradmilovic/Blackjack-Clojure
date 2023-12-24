(ns cheat-sheet-test.cheat-sheet-test
  (:require [midje.sweet :refer :all]
            [cheat-sheet.cheat-sheet :refer :all :as cs]))

(fact "Tests cheat sheet values conversion."
      (cs/value-for-ch "ace") => "A"
      (cs/value-for-ch "jack") => "10"
      (cs/value-for-ch "2") => "2"
      (cs/value-for-ch "unknown") => "unknown"
      (cs/value-for-ch "Ace") => "A"
      (cs/value-for-ch " ace ") => "A"
      (cs/value-for-ch 2) => 2
      (cs/value-for-ch "") => "")


(fact "'update-sheet' should update value at a given position."
      (let [initial-cheat-sheet (atom
                                  {:players-cards ["8" "9" "10" "11" "12" "13" "14" "15" "16" "17" "A2" "A3" "A4" "A5" "A6" "A7" "A8" "A9"
                                                   "22" "33" "44" "55" "66" "77" "88" "99" "1010" "AA"]
                                   :dealers-card  ["2" "3" "4" "5" "6" "7" "8" "9" "10" "A"]
                                   :values        (vec (repeat 28 (vec (repeat 10 "H"))))})]
        (fact "Updates value at a valid position"
              (get-in (:values (cs/update-sheet @initial-cheat-sheet 2 3 "new-value")) [2 3]) => "new-value")
        (fact "Does not update for non-existing indexes"
              (cs/update-sheet @initial-cheat-sheet 100 100 "new-value") => falsey)))

(fact "Tests exception handling in case of non-existing player's and dealer's card."
      (let
        [initial-cheat-sheet (atom
                               {:players-cards ["8" "9" "10" "11" "12" "13" "14" "15" "16" "17" "A2" "A3" "A4" "A5" "A6" "A7" "A8" "A9"
                                                "22" "33" "44" "55" "66" "77" "88" "99" "1010" "AA"]
                                :dealers-card  ["2" "3" "4" "5" "6" "7" "8" "9" "10" "A"]
                                :values        (vec (repeat 28 (vec (repeat 10 "H"))))})]
        (cs/get-move @initial-cheat-sheet "invalid-row" "invalid-col") => falsey))

(fact "Checks setting cheat sheet values."
      (let [initial-cheat-sheet (atom
                                  {:players-cards ["8" "9" "10" "11" "12" "13" "14" "15" "16" "17" "A2" "A3" "A4" "A5" "A6" "A7" "A8" "A9"
                                                   "22" "33" "44" "55" "66" "77" "88" "99" "1010" "AA"]
                                   :dealers-card  ["2" "3" "4" "5" "6" "7" "8" "9" "10" "A"]
                                   :values        (vec (repeat 28 (vec (repeat 10 "H"))))})]

        (fact "Sets single column value"
              (cs/set-value! "8" "2" "new-value" initial-cheat-sheet)
              (cs/get-move @initial-cheat-sheet "8" "2") => "new-value")

        (fact "Sets multiple columns values"
              (cs/set-value! "8" ["2" "3" "4"] "new-value" initial-cheat-sheet)
              (map #(cs/get-move @initial-cheat-sheet "8" %) ["2" "3" "4"]) => ["new-value" "new-value" "new-value"])

        (fact "Does not set value in case of non-existing row and column"
              (cs/set-value! "invalid-row" "invalid-col" "new-value" initial-cheat-sheet)
              (cs/get-move @initial-cheat-sheet "invalid-row" "invalid-col") => falsey)))


(fact "Tests update-cheat-sheet-cell."
      (let [initial-cheat-sheet (atom
                                  {:players-cards ["8" "9" "10" "11" "12" "13" "14" "15" "16" "17" "A2" "A3" "A4" "A5" "A6" "A7" "A8" "A9"
                                                   "22" "33" "44" "55" "66" "77" "88" "99" "1010" "AA"]
                                   :dealers-card  ["2" "3" "4" "5" "6" "7" "8" "9" "10" "A"]
                                   :values        (vec (repeat 28 (vec (repeat 10 "H"))))})
            all-rows (cs/get-rows @initial-cheat-sheet)
            all-cols (cs/get-cols @initial-cheat-sheet)]
        (cs/update-cheat-sheet-cell all-rows all-cols initial-cheat-sheet "9" ["3" "4" "5" "6"] "DD")
        (cs/get-move @initial-cheat-sheet "9" "4") => "DD"
        (cs/update-cheat-sheet-cell all-rows all-cols initial-cheat-sheet "invalid-row" ["3" "4" "5" "6"] "DD") => falsey
        (cs/update-cheat-sheet-cell all-rows all-cols initial-cheat-sheet "9" "invalid-col" "DD") => falsey))

