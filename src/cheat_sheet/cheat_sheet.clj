(ns cheat-sheet.cheat-sheet)

(def row-num 28)
(def col-num 10)
(def blackjack-cheat-sheet (atom
                             {:players-cards ["8" "9" "10" "11" "12" "13" "14" "15" "16" "17" "A2" "A3" "A4" "A5" "A6" "A7" "A8" "A9"
                                              "22" "33" "44" "55" "66" "77" "88" "99" "1010" "AA"]
                              :dealers-card  ["2" "3" "4" "5" "6" "7" "8" "9" "10" "A"]
                              :values        (vec (repeat row-num (vec (repeat col-num "H"))))}))

(defn- get-rows
  [cheat-sheet]
  (:players-cards cheat-sheet))
(defn- get-cols
  [cheat-sheet]
  (:dealers-card cheat-sheet))
(def row-names (get-rows @blackjack-cheat-sheet))
(def col-names (get-cols @blackjack-cheat-sheet))
(defn- value-for-ch
  "Returns a cheat-sheet representation of the card value."
  [val]
  (case val
    "ace" "A"
    "jack" "10"
    "queen" "10"
    "king" "10"
    val))
(defn- get-row-idx
  [row-name cheat-sheet]
  (.indexOf (:players-cards cheat-sheet) row-name))
(defn- get-col-idx
  [col-name cheat-sheet]
  (.indexOf (:dealers-card cheat-sheet) col-name))