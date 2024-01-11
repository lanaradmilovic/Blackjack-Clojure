(ns cheat-sheet.cheat-sheet)

(def row-num 28)
(def col-num 10)
(def blackjack-cheat-sheet (atom
                             {:players-cards ["8" "9" "10" "11" "12" "13" "14" "15" "16" "17"
                                              "A2" "A3" "A4" "A5" "A6" "A7" "A8" "A9"
                                              "22" "33" "44" "55" "66" "77" "88" "99" "1010" "AA"]
                              :dealers-card  ["2" "3" "4" "5" "6" "7" "8" "9" "10" "A"]
                              :values        (vec (repeat row-num (vec (repeat col-num "H"))))}))

(defn get-rows
  [cheat-sheet]
  (:players-cards cheat-sheet))
(defn get-cols
  [cheat-sheet]
  (:dealers-card cheat-sheet))
(def row-names (get-rows @blackjack-cheat-sheet))
(def col-names (get-cols @blackjack-cheat-sheet))

(defn value-for-ch
  "Returns a cheat-sheet representation of the card value."
  [val]
  (case (clojure.string/trim (clojure.string/lower-case val))
    "ace" "A"
    "jack" "10"
    "queen" "10"
    "king" "10"
    val))


(defn- get-row-idx
  [row-name cheat-sheet]
  (try
    (.indexOf (:players-cards cheat-sheet) row-name)
    (catch NullPointerException _
      (println "Non-existing player's card."))))

(defn- get-col-idx
  [col-name cheat-sheet]
  (try (.indexOf (:dealers-card cheat-sheet) col-name)
       (catch NullPointerException _
         (println "Non-existing dealer's card."))))

(defn update-sheet
  "Updates the cheat-sheet matrix by setting the specified value at the given row and column."
  [cheat-sheet row col value]
  (try
    (and
      (<= 0 (dec (count (:values cheat-sheet))))
      (<= 0 (dec (count (first (:values cheat-sheet))))))
    (update-in cheat-sheet [:values row col] (constantly value))
    (catch Exception _
      (println "Non-existing player's or dealer's card."))))

(defn get-move
  "Returns move recommendation for player based on player's and dealer's cards."
  [cheat-sheet player-card dealer-card]
  (try
    (get-in (:values cheat-sheet)
            [(get-row-idx player-card cheat-sheet) (get-col-idx (value-for-ch dealer-card) cheat-sheet)])
    (catch NullPointerException _
      (println "Non-existing player's or dealer's card."))))

(defn set-value
  "Updates the cheat-sheet matrix by setting the specified value at the given row and multiple columns."
  [row col value cheat-sheet]
  (let [row-idx (get-row-idx row cheat-sheet)
        cols (if (vector? col) col [col])]
    (reduce (fn
              [acc col]
              (update-sheet acc row-idx (get-col-idx col cheat-sheet) value))
            cheat-sheet
            cols)))

(defn set-value!
  "Updates the cheat-sheet matrix atom using 'set-value'"
  [row cols value cheat-sheet]
  (swap! cheat-sheet (fn
                       [ch]
                       (set-value row cols value ch))))

(defn update-cheat-sheet-cell [all-rows all-cols cheat-sheet row cols value]
  "Updates the cheat-sheet matrix using 'set-value-more-cols!' for desired field
   located at the intersection of the specified row and columns."
  (if (and
        (every? #(some #{%} all-rows) [row])
        (every? #(some #{%} all-cols) cols))
    (set-value! row cols value cheat-sheet)))

(update-cheat-sheet-cell row-names col-names blackjack-cheat-sheet "9" ["3" "4" "5" "6"] "DD")
(update-cheat-sheet-cell row-names col-names blackjack-cheat-sheet "10" ["2" "3" "4" "5" "6" "7" "8" "9"] "DD")
(update-cheat-sheet-cell row-names col-names blackjack-cheat-sheet "11" ["2" "3" "4" "5" "6" "7" "8" "9" "10"] "DD")
(update-cheat-sheet-cell row-names col-names blackjack-cheat-sheet "12" ["4" "5" "6"] "S")
(update-cheat-sheet-cell row-names col-names blackjack-cheat-sheet "13" ["2" "3" "4" "5" "6"] "S")
(update-cheat-sheet-cell row-names col-names blackjack-cheat-sheet "14" ["2" "3" "4" "5" "6"] "S")
(update-cheat-sheet-cell row-names col-names blackjack-cheat-sheet "15" ["2" "3" "4" "5" "6"] "S")
(update-cheat-sheet-cell row-names col-names blackjack-cheat-sheet "16" ["2" "3" "4" "5" "6"] "S")
(update-cheat-sheet-cell row-names col-names blackjack-cheat-sheet "17" ["2" "3" "4" "5" "6" "7" "8" "9" "10" "A"] "S")
(update-cheat-sheet-cell row-names col-names blackjack-cheat-sheet "A2" ["5" "6"] "DD")
(update-cheat-sheet-cell row-names col-names blackjack-cheat-sheet "A3" ["5" "6"] "DD")
(update-cheat-sheet-cell row-names col-names blackjack-cheat-sheet "A4" ["4" "5" "6"] "DD")
(update-cheat-sheet-cell row-names col-names blackjack-cheat-sheet "A5" ["4" "5" "6"] "DD")
(update-cheat-sheet-cell row-names col-names blackjack-cheat-sheet "A6" ["3" "4" "5" "6"] "DD")
(update-cheat-sheet-cell row-names col-names blackjack-cheat-sheet "A7" ["3" "4" "5" "6"] "DD")
(update-cheat-sheet-cell row-names col-names blackjack-cheat-sheet "A7" ["2" "7" "8"] "S")

(update-cheat-sheet-cell row-names col-names blackjack-cheat-sheet "A8" ["2" "3" "4" "5" "6" "7" "8" "9" "10" "A"] "S")
(update-cheat-sheet-cell row-names col-names blackjack-cheat-sheet "A9" ["2" "3" "4" "5" "6" "7" "8" "9" "10" "A"] "S")
(update-cheat-sheet-cell row-names col-names blackjack-cheat-sheet "22" ["4" "5" "6" "7"] "P")
(update-cheat-sheet-cell row-names col-names blackjack-cheat-sheet "33" ["4" "5" "6" "7"] "P")
(update-cheat-sheet-cell row-names col-names blackjack-cheat-sheet "55" ["2" "3" "4" "5" "6" "7" "8" "9"] "DD")
(update-cheat-sheet-cell row-names col-names blackjack-cheat-sheet "66" ["3" "4" "5" "6"] "P")
(update-cheat-sheet-cell row-names col-names blackjack-cheat-sheet "77" ["2" "3" "4" "5" "6" "7"] "P")
(update-cheat-sheet-cell row-names col-names blackjack-cheat-sheet "88" ["2" "3" "4" "5" "6" "7" "8" "9" "10" "A"] "P")
(update-cheat-sheet-cell row-names col-names blackjack-cheat-sheet "99" ["7" "10" "A"] "S")
(update-cheat-sheet-cell row-names col-names blackjack-cheat-sheet "99" ["2" "3" "4" "5" "6" "8" "9"] "P")

(update-cheat-sheet-cell row-names col-names blackjack-cheat-sheet "1010" ["2" "3" "4" "5" "6" "7" "8" "9" "10" "A"] "S")
(update-cheat-sheet-cell row-names col-names blackjack-cheat-sheet "AA" ["2" "3" "4" "5" "6" "7" "8" "9" "10" "A"] "P")
